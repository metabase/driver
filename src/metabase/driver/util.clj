(ns metabase.driver.util
  "Utility functions for common operations on drivers."
  (:require [clojure.tools.logging :as log]
            [metabase.common
             [config :as config]
             [i18n :refer [trs tru]]]
            [metabase.driver :as driver])
  (:import java.util.concurrent.TimeoutException))

(def ^:private can-connect-timeout-ms
  "Consider `can-connect?`/`can-connect-with-details?` to have failed after this many milliseconds. By default, this is
  5 seconds. You can configure this value by setting the env var `MB_DB_CONNECTION_TIMEOUT_MS`."
  (or (config/config-int :mb-db-connection-timeout-ms)
      5000))

(defn can-connect-with-details?
  "Check whether we can connect to a database with `driver` and `details-map` and perform a basic query such as `SELECT
  1`. Specify optional param `throw-exceptions` if you want to handle any exceptions thrown yourself (e.g., so you
  can pass the exception message along to the user); otherwise defaults to returning `false` if a connection cannot be
  established.

     (can-connect-with-details? :postgres {:host \"localhost\", :port 5432, ...})"
  ^Boolean [driver details-map & [throw-exceptions]]
  {:pre [(keyword? driver) (map? details-map)]}
  (if throw-exceptions
    (try
      (let [futur        (future (driver/can-connect? driver details-map))
            can-connect? (deref futur can-connect-timeout-ms ::timed-out)]
        (when (= can-connect? ::timed-out)
          (future-cancel futur)
          (TimeoutException. (str (tru "Timed out after {0} milliseconds." can-connect-timeout-ms)))))
      ;; actually if we are going to `throw-exceptions` we'll rethrow the original but attempt to humanize the message
      ;; first
      (catch Throwable e
        (throw (Exception. (driver/humanize-connection-error-message driver (.getMessage e)) e))))
    (try
      (can-connect-with-details? driver details-map :throw-exceptions)
      (catch Throwable e
        (log/error (trs "Failed to connect to database: {0}" (.getMessage e)))
        false))))

;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                             Available Drivers Info                                             |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn features
  "Return a set of all features supported by `driver`."
  [driver]
  (set (for [feature driver/driver-features
             :when (driver/supports? driver feature)]
         feature)))

(defn available-drivers
  "Return a set of all currently available drivers."
  []
  (set (for [driver (descendants driver/hierarchy :metabase.driver/driver)
             :when  (driver/available? driver)]
         driver)))

(defn available-drivers-info
  "Return info about all currently available drivers, including their connection properties fields and supported
  features."
  []
  (into {} (for [driver (available-drivers)]
             ;; TODO - maybe we should rename `connection-properties` -> `connection-properties` on the FE as well?
             [driver {:details-fields (driver/connection-properties driver)
                      :driver-name    (driver/display-name driver)
                      #_:features       #_(features driver)}])))
