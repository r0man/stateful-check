(ns stateful-check.core
  (:require [clojure.test :as t]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.results :refer [Result]]
            [stateful-check.generator :as g]
            [stateful-check.report :as report]
            [stateful-check.runner :as r])
  (:import [java.util UUID]
           [stateful_check.runner CaughtException]))

(def default-num-tests 200)
(def default-max-tries 1)
(def default-timeout-ms 0)

(defn- failure-messages
  "Return a map mapping from a command handle to a set of messages
  indicating failures that occurred during all the interleavings of a
  command set."
  [spec commands results bindings]
  (let [interleavings (g/every-interleaving (mapv vector
                                                  (:sequential commands)
                                                  (:sequential results))
                                            (mapv (partial mapv vector)
                                                  (:parallel commands)
                                                  (:parallel results)))
        init-state-fn (or (:initial-state spec)
                          (constantly nil))
        init-state    (if (:setup spec)
                        (init-state-fn (get bindings g/setup-var))
                        (init-state-fn))
        messages      (map #(r/failure-message % init-state bindings) interleavings)]
    (when (every? some? messages) ;; if all paths failed
      (->> messages
           (map (fn [[handle message]]
                  {handle #{message}}))
           (apply merge-with into)))))

(defn combine-cmds-with-traces [command result result-str]
  (let [last-str (pr-str result)]
    [command
     (cond
       (= ::r/unevaluated result) result
       (instance? CaughtException result) result
       (= last-str result-str) result-str
       :else (str result-str
                  "\n    >> object may have been mutated later into " last-str " <<\n"))
     result]))

(defn- failure-env
  "Return a map mapping from a command handle to a set of messages
  indicating failures that occurred during all the interleavings of a
  command set."
  [spec commands results bindings]
  (let [sequential (mapv combine-cmds-with-traces
                         (:sequential commands)
                         (:sequential results)
                         (:sequential-strings results))
        parallel (mapv (partial mapv combine-cmds-with-traces)
                       (:parallel commands)
                       (:parallel results)
                       (:parallel-strings results))
        init-state-fn (or (:initial-state spec)
                          (constantly nil))
        init-state (if (:setup spec)
                     (init-state-fn (get bindings g/setup-var))
                     (init-state-fn))
        sequential-env (r/failure-env sequential init-state bindings)
        last-env (get sequential-env (ffirst (last sequential)))]
    (into sequential-env
          (mapcat (fn [sequential]
                    (r/failure-env
                     sequential
                     (-> last-env :state :after)
                     (-> last-env :bindings :after)))
                  parallel))))

(defn- failure-env
  "Return a map mapping from a command handle to a set of messages
  indicating failures that occurred during all the interleavings of a
  command set."
  [spec commands results bindings]
  (let [sequential (mapv combine-cmds-with-traces
                         (:sequential commands)
                         (:sequential results)
                         (:sequential-strings results))
        parallel (mapv (partial mapv combine-cmds-with-traces)
                       (:parallel commands)
                       (:parallel results)
                       (:parallel-strings results))
        init-state-fn (or (:initial-state spec)
                          (constantly nil))
        init-state (if (:setup spec)
                     (init-state-fn (get bindings g/setup-var))
                     (init-state-fn))
        sequential-env (r/failure-env sequential {:real init-state :symbolic init-state} bindings)
        last-env (get sequential-env (ffirst (last sequential)))]
    (into sequential-env
          (mapcat (fn [[thread sequential]]
                    (r/failure-env
                     sequential
                     (-> last-env :state :after)
                     (-> last-env :bindings :after)
                     thread))
                  (map-indexed vector parallel)))))

(def ^:dynamic *run-commands* nil)

(defn spec->property
  "Turn a specification into a testable property."
  ([spec] (spec->property spec nil))
  ([spec options]
   (for-all [commands (g/commands-gen spec (:gen options))]
            (let [runners (r/commands->runners commands)
                  setup-fn (:setup spec)]
              (when *run-commands*
                (doseq [cmds (cons (:sequential commands)
                                   (:parallel commands))]
                  (->> cmds
                       (into {} (map (fn [[_ {:keys [name]} _]]
                                       [name 1])))
                       (swap! *run-commands* #(merge-with + %1 %2)))))
              (reduce (fn [result try]
                        (let [setup-result (when-let [setup setup-fn]
                                             (setup))
                              bindings (if setup-fn
                                         {g/setup-var setup-result}
                                         {})]
                          (try
                            (let [results (r/runners->results runners bindings (get-in options [:run :timeout-ms] default-timeout-ms))]
                              (if-let [messages (failure-messages spec commands results bindings)]
                                (reduced (reify Result
                                           (pass? [_]
                                             false)
                                           (result-data [_]
                                             {:commands commands
                                              :environment (failure-env spec commands results bindings)
                                              :id (UUID/randomUUID)
                                              :message "Generative test failed."
                                              :messages messages
                                              :options options
                                              :parallel (mapv (partial mapv combine-cmds-with-traces)
                                                              (:parallel commands)
                                                              (:parallel results)
                                                              (:parallel-strings results))
                                              :sequential (mapv combine-cmds-with-traces
                                                                (:sequential commands)
                                                                (:sequential results)
                                                                (:sequential-strings results))
                                              :specification spec})))
                                result))
                            (catch clojure.lang.ExceptionInfo ex
                              (reduced (reify Result
                                         (pass? [_]
                                           false)
                                         (result-data [_]
                                           (if (= (.getMessage ex) "Timed out")
                                             (let [results (ex-data ex)]
                                               {:commands commands
                                                :id (UUID/randomUUID)
                                                :environment (failure-env spec commands results bindings)
                                                :message "Generative test timed out."
                                                :messages {nil "Test timed out."}
                                                :options options
                                                :parallel (mapv (partial mapv combine-cmds-with-traces)
                                                                (:parallel commands)
                                                                (:parallel results)
                                                                (:parallel-strings results))
                                                :sequential (mapv combine-cmds-with-traces
                                                                  (:sequential commands)
                                                                  (:sequential results)
                                                                  (:sequential-strings results))
                                                :specification spec})
                                             ;; TODO: Not sure if we should re-throw this exception and let test.check handle it.
                                             {:clojure.test.check.properties/error ex
                                              :commands commands
                                              :id (UUID/randomUUID)
                                              :message "Generative test threw an exception."
                                              :options options
                                              :specification spec})))))
                            (finally
                              (when-let [cleanup (:cleanup spec)]
                                (if setup-fn
                                  (cleanup setup-result)
                                  (cleanup)))))))
                      (reify Result
                        (pass? [_]
                          true)
                        (result-data [_]
                          {:commands commands
                           :id (UUID/randomUUID)
                           :options options
                           :specification spec}))
                      (range (get-in options [:run :max-tries] default-max-tries)))))))

(defn run-specification
  "Run a specification. This will convert the spec into a property and
  run it using clojure.test.check/quick-check. This function then
  returns the full quick-check result."
  ([specification] (run-specification specification nil))
  ([specification options]
   (quick-check (get-in options [:run :num-tests] default-num-tests)
                (spec->property specification options)
                :reporter-fn (get-in options [:report :reporter-fn] identity)
                :seed (get-in options [:run :seed] (System/currentTimeMillis))
                :max-size (get-in options [:gen :max-size] g/default-max-size))))

(defn specification-correct?
  "Test whether or not the specification matches reality. This
  generates test cases and runs them. If run with in an `is`, it will
  report details (and pretty-print them) if it fails.

  The `options` map consists of three potential keys: `:gen`, `:run`,
  and `:report`, each of which influence a different part of the test.

  `:gen` has four sub-keys:
   - `:threads` specifies how many parallel threads to execute
   - `:max-length` specifies a max length for command sequences
   - `:max-size` specifies a maximum size for generated values
   - `:shrink-strategies` specifies a sequence of shrink strategies
     that should be tried (in order) to reduce the size of a failing
     test (see `stateful-check.generator/default-shrink-strategies`
     and `stateful-check.shrink-strategies`)

  `:run` has three sub-keys:
   - `:max-tries` specifies how attempts to make to fail a test
   - `:num-tests` specifies how many tests to run
   - `:seed` specifies the initial seed to use for generation
   - `:timeout-ms` specifies the maximum number of milliseconds that a
     test is permitted to run for - taking longer is considered a
     failure (default: 0, meaning no timeout; see NOTE below for more
     details)

  `:report` has two sub-keys, but only works within an `is`:
   - `:first-case?` specifies whether to print the first failure
   - `:stacktrace?` specifies whether to print exception stacktraces
   - `:command-frequency?` specifies whether to print information
     about how often each command was run

  The `:timeout-ms` option is unsafe in general, but may be helpful in
  some circumstances. It allows you to categorise a test as a failure
  if it takes more than a given time, but each of the threads must
  respond to being interrupted by completing and shutting down. If
  these threads do not shut themselves down then they may continue to
  consume system resources (CPU and memory, among other things),
  impacting other tests."
  ([specification] (specification-correct? specification nil))
  ([specification options]
   (:pass? (run-specification specification options))))
;; We need this to be a separate form, for some reason. The attr-map
;; in defn doesn't work if you use the multi-arity form.
(alter-meta! #'specification-correct? assoc :arglists
             `([~'specification]
               [~'specification {:gen {:threads ~g/default-threads
                                       :max-length ~g/default-max-length
                                       :max-size ~g/default-max-size
                                       :shrink-strategies g/default-shrink-strategies}
                                 :run {:max-tries ~default-max-tries
                                       :num-tests ~default-num-tests
                                       :seed (System/currentTimeMillis)
                                       :timeout-ms ~default-timeout-ms}
                                 :report {:first-case? false
                                          :stacktrace? false
                                          :command-frequency? false}}]))

(defmethod t/assert-expr 'specification-correct?
  [msg [_ specification options]]
  `(let [spec# ~specification
         options# ~options
         options# (if (contains? (:report options#) :reporter-fn)
                    options#
                    (assoc-in options# [:report :reporter-fn] report/default-reporter-fn))
         [results# frequencies#] (binding [*run-commands* (atom {})]
                                   [(run-specification spec# options#)
                                    @*run-commands*])]
     (report/report-result ~msg results# frequencies#)))
