(ns metabase.driver.util.ssh
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [colorize.core :as colorize]
            [metabase.common.i18n :refer [trs tru]])
  (:import com.jcraft.jsch.JSch))

(def ^:private default-ssh-timeout 30000)

(defn start-ssh-tunnel
  "Opens a new ssh tunnel and returns the connection along with the dynamically
   assigned tunnel entrance port. It's the callers responsibility to call .disconnect
   on the returned connection object."
  [{:keys [tunnel-host tunnel-port tunnel-user tunnel-pass host port]}]
  (let [connection (doto ^com.jcraft.jsch.Session (.getSession (new com.jcraft.jsch.JSch)
                                                               ^String tunnel-user
                                                               ^String tunnel-host
                                                               tunnel-port)
                     (.setPassword ^String tunnel-pass)
                     (.setConfig "StrictHostKeyChecking" "no")
                     (.connect default-ssh-timeout)
                     (.setPortForwardingL 0 host port))
        input-port (some-> (.getPortForwardingL connection)
                           first
                           (str/split #":")
                           first
                           (Integer/parseInt))]
    (assert (number? input-port))
    (log/info
     (colorize/cyan
      (str (tru "creating ssh tunnel {0}"
                (format "%s@%s:%s -L %s:%s:%s" tunnel-user tunnel-host tunnel-port input-port host port)))))
    [connection input-port]))

(def ssh-tunnel-preferences
  "Configuration parameters to include in the add driver page on drivers that
   support ssh tunnels"
  [{:name         "tunnel-enabled"
    :display-name "Use SSH tunnel"
    :placeholder  "Enable this ssh tunnel?"
    :type         :boolean
    :default      false}
   {:name         "tunnel-host"
    :display-name "SSH tunnel host"
    :placeholder  "What hostname do you use to connect to the SSH tunnel?"
    :required     true}
   {:name         "tunnel-port"
    :display-name "SSH tunnel port"
    :type         :integer
    :default      22
    :required     false}
   {:name         "tunnel-user"
    :display-name "SSH tunnel username"
    :placeholder  "What username do you use to login to the SSH tunnel?"
    :required     true}
   {:name         "tunnel-pass"
    :display-name "SSH tunnel password"
    :type         :password
    :placeholder  "******"
    :required     true}
   #_{:name         "tunnel-private-key"
    :display-name "SSH private key to connect to the tunnel"
    :type         :string
    :placeholder  "Paste the contents of an ssh private key here"}
   #_{:name         "tunnel-private-key-file-name"
    :display-name "Path on the Metabase server to a SSH private key file to connect to the tunnel"
    :type         :string
    :placeholder  "/home/YOUR-USERNAME/.ssh/id_rsa"}])

(defn with-tunnel-config
  "Add preferences for ssh tunnels to a drivers :connection-properties"
  [driver-options]
  (concat driver-options ssh-tunnel-preferences))

(defn use-ssh-tunnel?
  "Is the SSH tunnel currently turned on for these connection details"
  [details]
  (:tunnel-enabled details))

(defn include-ssh-tunnel
  "Updates connection details for a data warehouse to use the ssh tunnel host and port
   For drivers that enter hosts including the protocol (https://host), copy the protocol over as well"
  [details]
  (if (use-ssh-tunnel? details)
    (let [[_ proto host]                    (re-find #"(.*://)?(.*)" (:host details))
          ;; don't include L7 protocol in ssh tunnel
          [connection tunnel-entrance-port] (start-ssh-tunnel (assoc details :host host))
          details-with-tunnel               (assoc details
                                              ;; This parameter is set dynamically when the connection is established
                                              :port tunnel-entrance-port
                                              :host (str proto "localhost")
                                              ;; the input port is not known until the connection is opened
                                              :tunnel-entrance-port tunnel-entrance-port
                                              :tunnel-connection connection)]
      details-with-tunnel)
    details))

(defn with-ssh-tunnel*
  "Starts an SSH tunnel, runs the supplied function with the tunnel open, then closes it"
  [{:keys [host port tunnel-host tunnel-user tunnel-pass] :as details} f]
  (if (use-ssh-tunnel? details)
    (let [details-with-tunnel (include-ssh-tunnel details)]
      (try
        (log/info (colorize/cyan (str (trs "Opened SSH tunnel"))))
        (f details-with-tunnel)
        (finally
          (.disconnect ^com.jcraft.jsch.Session (:tunnel-connection details-with-tunnel))
          (log/info (colorize/cyan (str (trs "Closed SSH tunnel")))))))
    (f details)))

(defmacro with-ssh-tunnel
  "Starts an ssh tunnel, and binds the supplied name to a database details map with it's values adjusted to use the
  tunnel"
  [[name details] & body]
  `(with-ssh-tunnel* ~details
     (fn [~name]
       ~@body)))
