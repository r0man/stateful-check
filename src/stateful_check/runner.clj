(ns stateful-check.runner
  (:require [stateful-check.command-utils :as u]
            [stateful-check.symbolic-values :as sv]))

(defn make-sequential-runners [cmd-objs]
  (mapv (fn [[handle cmd-obj & args]]
          (if-let [function (:command cmd-obj)]
            [handle #(apply function (sv/get-real-value args %))]
            (throw (AssertionError. (str "No :command function found for "
                                         (:name cmd-obj)
                                         " command")))))
        cmd-objs))

(defrecord CaughtException [exception])

(defn run-sequential-runners [runners bindings]
  (reduce (fn [[bindings trace str-trace] [handle f]]
            (try
              (let [value (f bindings)]
                [(assoc bindings handle value)
                 (conj trace value)
                 (conj str-trace (pr-str value))])
              (catch Exception exception
                (reduced [bindings
                          (conj trace (->CaughtException exception))
                          (conj str-trace exception)
                          exception]))))
          [bindings [] []] runners))

(defn commands->runners [{:keys [sequential parallel]}]
  {:sequential (make-sequential-runners sequential)
   :parallel (mapv make-sequential-runners parallel)})

(defmacro with-timeout [timeout-ms & body]
  `(let [timeout-ms# ~timeout-ms]
     (if (<= timeout-ms# 0)
       (do ~@body)
       (let [f# (future ~@body)
             v# (deref f# timeout-ms# ::timeout)]
         (if (= v# ::timeout)
           (do (future-cancel f#)
               (throw (InterruptedException. "Timed out")))
           v#)))))

(defn runners->results [{:keys [sequential parallel]} bindings timeout-ms]
  (try
    (with-timeout timeout-ms
      (let [[bindings trace str-trace exception] (run-sequential-runners sequential bindings)
            latch (java.util.concurrent.atomic.AtomicBoolean. true)
            futures (when-not exception
                      (mapv #(future
                               (while (.get latch)
                                 ;; spin until all the futures have been
                                 ;; created (this is probably unnecessary,
                                 ;; but just in case)
                                 )
                               (run-sequential-runners % bindings))
                            parallel))]
        (try
          (.set latch false)
          (let [values (mapv deref futures)]
            {:sequential trace
             :sequential-strings str-trace
             :parallel (mapv #(nth % 1) values)
             :parallel-strings (mapv #(nth % 2) values)})
          (catch InterruptedException ex
            (mapv future-cancel futures)))))
    (catch InterruptedException ex
      (throw (ex-info "Timed out"
                      {:sequential (mapv (constantly ::unevaluated) sequential)
                       :sequential-strings (mapv (constantly "???") sequential)
                       :parallel (mapv #(mapv (constantly ::unevaluated) %) parallel)
                       :parallel-strings (mapv #(mapv (constantly "???") %) parallel)})))))

;; TODO: Rename to failure-data, failure-env, or just failure?
(defn failure-message
  "Return a vector of [handle failure] representing which command
  failed, and why. The failure entry is a map with a :message key and
  an optional :events key, which contains any failing Clojure Test
  report events emitted during the evaluation of the
  postcondition. Returns nil if no command has failed."
  [cmds-and-traces state bindings]
  (first (reduce (fn [[_ state bindings] [[handle cmd-obj & args] result]]
                   (if (instance? CaughtException result)
                     (reduced [[handle {:message "Unexpected exception thrown."}]])
                     (let [replaced-args (sv/get-real-value args bindings)
                           next-state (u/make-next-state cmd-obj state replaced-args result)]
                       (if-let [failure (u/check-postcondition cmd-obj state next-state replaced-args result)]
                         (reduced [[handle failure]])
                         [nil
                          next-state
                          (assoc bindings handle result)]))))
                 [nil state bindings] cmds-and-traces)))

(defn failure-env
  "Return a map of {handle frame} representing the execution frame for
  each handle."
  [cmds-and-traces state bindings]
  (first (reduce (fn [[env state bindings] [index [[handle cmd-obj & args] _result-str result]]]
                   (let [real-args (sv/get-real-value args bindings)
                         next-bindings (assoc bindings handle result)
                         frame {:arguments {:symbolic (vec args) :real (vec real-args)}
                                :bindings {:before bindings :after next-bindings}
                                :command cmd-obj
                                :handle handle
                                :index index
                                :result result
                                :state {:before state}}]
                     ;; TODO: This is using the real state, what about the next state?
                     (let [next-state (u/make-next-state cmd-obj state real-args result)
                           frame (assoc-in frame [:state :after] next-state)
                           failure (u/check-postcondition cmd-obj state next-state real-args result)]
                       [(assoc env handle (cond-> frame failure (assoc :failure failure)))
                        next-state next-bindings])))
                 [{} state bindings] (map-indexed vector cmds-and-traces))))
