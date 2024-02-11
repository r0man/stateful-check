(ns stateful-check.debugger.repl
  (:refer-clojure :exclude [reset!])
  (:require [stateful-check.debugger.core :as debugger]))

(def ^:dynamic *debugger*
  "The debugger used in the REPL."
  (debugger/debugger))

(defn- swap-debugger!
  "Swap the `*debugger*` by applying `f` and `args` to it."
  [f & args]
  (alter-var-root #'*debugger* #(apply f % args)))

(defn- make-query
  "Swap the `*debugger*` by applying `f` and `args` to it."
  [query]
  (let [last-run (debugger/last-run *debugger*)
        query (or query {})]
    (cond-> {}
      (map? query)
      (merge query)
      (string? query)
      (assoc :id query)
      (and (map? query) (nil? (:run query)))
      (assoc :id (:id last-run)))))

(defn get-run
  "Lookup an analysis in `*debugger*` according to `query`."
  [& {:as query}]
  (debugger/get-run *debugger* (make-query query)))

(defn eval-step
  "Evaluate the current command."
  [& {:as query}]
  (let [{:keys [case id]} (make-query query)]
    (swap-debugger! debugger/eval-step id case)
    (debugger/last-run *debugger*)))

(defn eval-stop
  "Stop the evaluation."
  [& {:as query}]
  (let [{:keys [case id]} (make-query query)]
    (swap-debugger! debugger/eval-stop id case)
    (debugger/last-run *debugger*)))

(defn get-env
  "Find the environment for `query` in `*debugger*`."
  [& {:as query}]
  (debugger/get-env *debugger* (make-query query)))

(defn get-eval-state
  "Get the current evaluation state in `*debugger*` for `query`."
  [& {:as query}]
  (-> (debugger/get-failing-case *debugger* (make-query query))
      :state-machine :state))

(defn get-arguments
  "Find the bindings for `query` in `*debugger*`."
  [& {:as query}]
  (:arguments (debugger/get-env *debugger* (make-query query))))

(defn get-bindings
  "Find the bindings for `query` in `*debugger*`."
  [& {:as query}]
  (:bindings (debugger/get-env *debugger* (make-query query))))

(defn get-state
  "Find the state for `query` in `*debugger*`."
  [& {:as query}]
  (:state (debugger/get-env *debugger* (make-query query))))

(defn clear
  "Clear the `*debugger*`."
  []
  (swap-debugger! (constantly (debugger/debugger))))

(defn run-specification
  "Run the `specification` using `options` and return the analyzed results."
  [specification & [options]]
  (swap-debugger! debugger/run-specification specification options)
  (debugger/last-run *debugger*))

(defn analyze-run
  "Analyze the Stateful Check `run`."
  [run]
  (-> (swap-debugger! debugger/analyze-run run)
      (debugger/last-run)))

(defn scan
  "Scan for Stateful Check specifications."
  []
  (-> (swap-debugger! debugger/scan)
      (debugger/specifications)))

(defn specification
  "Get the Stateful Check specification of `*debugger*` by `id`."
  [id]
  (debugger/specification *debugger* id))

(defn specifications
  "Return the Stateful Check specifications of the `*debugger*`."
  []
  (debugger/specifications *debugger*))

(defn reset
  "Reset the `*debugger*`."
  []
  (and (clear) (scan) *debugger*))
