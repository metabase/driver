(ns metabase.driver.qp-interface
  (:require [external-function.core :refer [defexternal]]))

(defexternal execute-current-time-query
  "Execute an MBQL `query` to get the current time for a Database."
  {:style/indent 2}
  [database-id driver query])

(defexternal report-timezone
  "Get the report timezone for this Metabase instance."
  [])

(defexternal field-qualified-name-components [field])

(defexternal query->remark
  [query])

(defexternal aggregation-name [aggregation-clause])

(defexternal disable-logging? [])

(defexternal database [])

(defexternal table [table-id])

(defexternal field [field-id])
