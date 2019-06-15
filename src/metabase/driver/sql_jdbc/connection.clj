(ns metabase.driver.sql-jdbc.connection
  "Logic for creating and managing connection pools for SQL JDBC drivers. Implementations for connection-related driver
  multimethods for SQL JDBC drivers."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [colorize.core :as colorize]
            [metabase
             [connection-pool :as connection-pool]
             [driver :as driver]]
            [metabase.common.i18n :refer [trs tru]]
            [metabase.driver.util.ssh :as ssh]))

;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                                   Interface                                                    |
;;; +----------------------------------------------------------------------------------------------------------------+

(defmulti connection-details->spec
  "Given a Database `details-map`, return a JDBC connection spec."
  {:arglists '([driver details-map]), :style/indent 1}
  driver/dispatch-on-initialized-driver
  :hierarchy #'driver/hierarchy)


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                           Creating Connection Pools                                            |
;;; +----------------------------------------------------------------------------------------------------------------+

(def ^:private data-warehouse-connection-pool-properties
  "c3p0 connection pool properties for connected data warehouse DBs. See
  https://www.mchange.com/projects/c3p0/#configuration_properties for descriptions of properties."
  {"maxIdleTime"                  (* 3 60 60)
   "minPoolSize"                  1
   "initialPoolSize"              1
   "maxPoolSize"                  15
   ;; prevent broken connections closed by dbs by testing them every 3 mins
   "idleConnectionTestPeriod"     (* 3 60)
   ;; prevent overly large pools by condensing them when connections are idle for 15m+
   "maxIdleTimeExcessConnections" (* 15 60)})

(defn- create-pool!
  "Create a new C3P0 `ComboPooledDataSource` for connecting to the given `database`."
  [{:keys [id engine details], :as database}]
  {:pre [(map? database)]}
  (log/debug (colorize/cyan (str (trs "Creating new connection pool for database {0} ..." (str id)))))
  (let [details-with-tunnel (ssh/include-ssh-tunnel details) ;; If the tunnel is disabled this returned unchanged
        spec                (connection-details->spec engine details-with-tunnel)]
    (assoc (connection-pool/connection-pool-spec spec data-warehouse-connection-pool-properties)
      :ssh-tunnel (:tunnel-connection details-with-tunnel))))

(defn- destroy-pool! [database-id pool-spec]
  (log/debug (colorize/yellow (tru "Closing old connection pool for database {0} ..." (str database-id))))
  (connection-pool/destroy-connection-pool! (:datasource pool-spec))
  (when-let [ssh-tunnel (:ssh-tunnel pool-spec)]
    (.disconnect ^com.jcraft.jsch.Session ssh-tunnel)))

(defonce ^:private ^{:doc "A map of our currently open connection pools, keyed by Database `:id`."}
  database-id->connection-pool
  (atom {}))

(defn- set-pool!
  "Atomically update the current connection pool for Database with `database-id`. Use this function instead of modifying
  `database-id->connection-pool` directly because it properly closes down old pools in a thread-safe way, ensuring no
  more than one pool is ever open for a single database."
  [database-id pool-spec-or-nil]
  (let [[old-id->pool] (swap-vals! database-id->connection-pool assoc database-id pool-spec-or-nil)]
    ;; if we replaced a different pool with the new pool that is different from the old one, destroy the old pool
    (when-let [old-pool-spec (get old-id->pool database-id)]
      (when-not (identical? old-pool-spec pool-spec-or-nil)
        (destroy-pool! database-id old-pool-spec))))
  nil)

(defn notify-database-updated
  "Default implementation of `driver/notify-database-updated` for JDBC SQL drivers. We are being informed that a
  `database` has been updated, so lets shut down the connection pool (if it exists) under the assumption that the
  connection details have changed."
  [_ {database-id :id}]
  (set-pool! database-id nil))

(def ^:private create-pool-lock (Object.))

(defn db->pooled-connection-spec
  "Return a JDBC connection spec that includes a cp30 `ComboPooledDataSource`.
   Theses connection pools are cached so we don't create multiple ones to the same DB."
  [{database-id :id, :as database}]
  {:pre [(map? database) (integer? database-id) (keyword? (:engine database)) (map? (:details database))]}
  (or
   ;; we have an existing pool for this database, so use it
   (get @database-id->connection-pool database-id)
   ;; Even tho `set-pool!` will properly shut down old pools if two threads call this method at the same time, we
   ;; don't want to end up with a bunch of simultaneous threads creating pools only to have them destroyed the very
   ;; next instant. This will cause their queries to fail. Thus we should do the usual locking here and make sure only
   ;; one thread will be creating a pool at a given instant.
   (locking create-pool-lock
     (or
      ;; check if another thread created the pool while we were waiting to acquire the lock
      (get @database-id->connection-pool database-id)
      ;; create a new pool and add it to our cache, then return it
      (let [pool (create-pool! database)]
        (set-pool! database-id pool)
        pool)))))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                             metabase.driver impls                                              |
;;; +----------------------------------------------------------------------------------------------------------------+

(defn details->connection-spec-for-testing-connection
  "Return an appropriate JDBC connection spec to test whether a set of connection details is valid (i.e., implementing
  `can-connect?`)."
  [driver details]
  (let [details-with-tunnel (ssh/include-ssh-tunnel details)]
    (connection-details->spec driver details-with-tunnel)))

(defn can-connect?
  "Default implementation of `driver/can-connect?` for SQL JDBC drivers. Checks whether we can perform a simple `SELECT
  1` query."
  [driver details]
  (let [spec        (details->connection-spec-for-testing-connection driver details)
        [first-row] (jdbc/query spec ["SELECT 1"])
        [result]    (vals first-row)]
    (= 1 result)))
