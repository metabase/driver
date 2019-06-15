(ns metabase.driver.dispatch
  (:require [metabase.mbql.util :as mbql.u]))

(defmulti metabase-model
  "Return a keyword name of the Metabase model that `x` is an instance of. Should be one of `:model/Database`,
  `:model/Table`, or `:model/Field`; or `nil` if none of the above. Used to allow dispatch on Metabase Toucan models
  without needing to have the actual model classes defined."
  {:arglists '([x])}
  class)

(defmethod metabase-model :default [_] nil)

(defn dispatch-on-clause-or-model-or-class [x & _]
  (or (metabase-model x)
      (mbql.u/dispatch-by-clause-name-or-class x)))
