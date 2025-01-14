(ns stateful-check.debugger.render
  (:require [cider.nrepl.middleware.test.extensions :refer [diffs-result]]
            [clojure.pprint :as pp]
            [orchard.inspect :as inspect]
            [orchard.print :as orchard.print]
            [stateful-check.symbolic-values :as sv]))

(defn- render-value
  "Render the given `value` as a string."
  [value]
  (cond
    (satisfies? sv/SymbolicValue value)
    (pr-str value)
    (instance? clojure.lang.Atom value)
    (pr-str value) ;; Atom's aren't printed nicely by orchard
    :else
    (binding [orchard.print/*max-atom-length* 50]
      (orchard.print/print-str value))))

(defn- render-error
  "Render the given `error` as a string."
  [error]
  (into {} (for [[key error] error]
             [key (when (instance? Throwable error)
                    (.toString error))])))

(defn- render-result
  "Render the command execution `result`."
  [result]
  (cond-> result
    (contains? result :evaluation)
    (update :evaluation render-value)
    (contains? result :real)
    (update :real render-value)
    (contains? result :symbolic)
    (update :symbolic pr-str)))

(defn- render-argument
  "Render the command execution `argument`."
  [argument]
  (cond-> (select-keys argument [:index :name :real :symbolic :evaluation])
    (contains? argument :evaluation)
    (update :evaluation render-value)
    (contains? argument :real)
    (update :real render-value)
    (contains? argument :symbolic)
    (update :symbolic pr-str)))

(defn- print-object
  "Print `object` using println for matcher-combinators results and pprint
   otherwise. The matcher-combinators library uses a custom print-method
   which doesn't get picked up by pprint since it uses a different dispatch
   mechanism."
  [object]
  (let [matcher-combinators-result? (= (:type (meta object))
                                       :matcher-combinators.clj-test/mismatch)
        print-fn (if matcher-combinators-result?
                   println
                   pp/pprint)]
    (with-out-str (print-fn object))))

(defn- render-event [event]
  (cond-> event
    (contains? event :actual)
    (update :actual print-object)
    (contains? event :diffs)
    (update :diffs diffs-result)
    (contains? event :expected)
    (update :expected print-object)))

(defn- render-failure [failure]
  (cond-> (select-keys failure [:events :index :message])
    (seq (:events failure))
    (update :events #(mapv render-event %))))

(defn- render-failures [failures]
  (cond-> failures
    (contains? failures :evaluation)
    (update :evaluation #(mapv render-failure %))
    (contains? failures :real)
    (update :real #(mapv render-failure %))))

(defn- render-binding [bindings]
  (into {} (for [[handle value] bindings]
             [(pr-str handle)
              (render-value value)])))

(defn- render-bindings [bindings]
  (cond-> bindings
    (contains? bindings :evaluation)
    (update :evaluation render-binding)
    (contains? bindings :real)
    (update :real render-binding)
    (contains? bindings :symbolic)
    (update :symbolic render-binding)))

(defn- render-execution-frame
  "Render the execution `frame`."
  [frame]
  (-> (select-keys frame [:arguments :bindings :command :error :failures :handle :result])
      (update :arguments #(mapv render-argument %))
      (update :bindings render-bindings)
      (update :command select-keys [:name])
      (update :error render-error)
      (update :failures render-failures)
      (update :handle pr-str)
      (update :result render-result)))

(defn- render-execution-frames
  "Render the execution frames for `executions`."
  [{:keys [environments]} executions]
  (vec (for [[[handle _cmd-obj & _symbolic-args] _result-str] executions]
         (-> (get environments handle)
             (render-execution-frame)))))

(defn- render-sequential-executions
  "Render the sequential execution frames."
  [{:keys [sequential] :as result-data}]
  (render-execution-frames result-data sequential))

(defn- render-parallel-executions
  "Render the parallel execution frames."
  [{:keys [parallel] :as result-data}]
  (mapv #(render-execution-frames result-data %) parallel))

(defn- render-executions
  "Render the sequential and parallel execution frames."
  [result-data]
  {:sequential (render-sequential-executions result-data)
   :parallel (render-parallel-executions result-data)})

(defn- render-state-machine
  "Render the `state-machine`."
  [state-machine]
  (cond-> state-machine
    (:state state-machine)
    (update :state vec)))

(defn evaluating?
  "Render a failing test case."
  [{:keys [environments] :as result-data}]
  (some? (seq (keep (comp :evaluation :bindings) (vals environments)))))

(defn render-result-data
  "Render a failing test case."
  [result-data]
  (-> (select-keys result-data [:specification :options :state-machine])
      (update :state-machine render-state-machine)
      (assoc :eval? (evaluating? result-data))
      (assoc :executions (render-executions result-data))))

(defn- render-quickcheck-results
  "Render the test.check result/report data structure."
  [results]
  (-> (select-keys results [:failed-after-ms :failing-size :num-tests :seed :shrunk :result-data :pass? :time-elapsed-ms])
      (update :shrunk select-keys [:depth :result-data :time-shrinking-ms :total-nodes-visited])))

(defn render-run [analysis]
  (-> (render-quickcheck-results analysis)
      (merge (select-keys analysis [:id :frequencies :specification :options]))
      (update-in [:result-data] render-result-data)
      (update-in [:shrunk :result-data] render-result-data)))
