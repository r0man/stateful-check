(ns stateful-check.debugger.error
  (:require [clojure.spec.alpha :as s]))

(defn invalid-handle
  "Return an exception that the command `handle` is not a valid."
  [handle]
  (ex-info "Invalid command handle" {:handle handle}))

(defn invalid-specification
  "Return an exception indicating that `specification` is not valid."
  [specification]
  (ex-info "Invalid Stateful Check specification"
           (merge (s/explain-data :stateful-check/specification specification)
                  {:type :stateful-check/invalid-specification
                   :invalid-specification specification})))

(defn no-specification-bound
  "Return an exception indicating that the `object` bound to `var` is
  not to valid Stateful Check specification."
  [var object]
  (ex-info "No Stateful Check specification bound to var"
           (merge (s/explain-data :stateful-check/specification object)
                  {:type :stateful-check/invalid-specification-var
                   :var var :object object})))

(defn run-not-found
  "Return an exception indicating that no Stateful Check run was found
  that matches `query`."
  [query]
  (ex-info "Stateful Check run not found"
           {:type :stateful-check.debugger/run-not-found
            :query query}))

(defn specification-not-found
  "Return an exception indicating that no Stateful Check specification
  was found that matches `query`."
  [query]
  (ex-info "Stateful Check specification not found"
           {:type :stateful-check.debugger/specification-not-found
            :query query}))
