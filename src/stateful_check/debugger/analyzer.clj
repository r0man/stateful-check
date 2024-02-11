(ns stateful-check.debugger.analyzer
  (:require [stateful-check.command-utils :as u]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.generator :as g]
            [stateful-check.symbolic-values :as sv])
  (:import [clojure.lang Var]
           [java.util Base64 UUID]
           [stateful_check.runner CaughtException]))

(defn- find-argument-list [command num-args]
  (first (filter #(= num-args (count %)) (-> command :meta :arglists))))

(defn- analyze-argument
  [command arg-num {:keys [index] :as argument}]
  (let [arg-list (find-argument-list command arg-num)
        arg-name (str (nth arg-list index index))]
    (cond-> argument
      arg-name (assoc :name arg-name))))

(defn- analyze-command
  "Analyze the Stateful Check `cmd` map."
  [{:keys [command] :as cmd}]
  (let [meta-data (or (meta command)
                      (and (instance? Var command)
                           (-> command deref meta)))]
    (cond-> cmd
      meta-data
      (assoc :meta meta-data))))

(defn- analyze-event
  "Analyze a test report event in a similar way to
  `cider.nrepl.middleware.test/test-report`, with the exception the
  difference that we don't have a test namespace, var and testing
  context available. Printing is done in the rendering code."
  [[index m]]
  (let [{:keys [actual expected fault] t :type} m]
    (merge (dissoc m :expected :actual)
           {:index index}
           (when (and (#{:fail :error} t) (not fault))
             {:expected expected})
           (when (#{:fail} t)
             {:actual actual})
           (when (#{:error} t)
             (let [^Throwable exception actual
                   ^StackTraceElement frame (some-> exception .getStackTrace first)]
               {:error exception
                :line (some-> frame .getLineNumber)})))))

(defn analyze-failure
  "Analyze a postcondition failure."
  [[index {:keys [events] :as failure}]]
  (cond-> (assoc failure :index index)
    (seq events)
    (update :events #(mapv analyze-event (map-indexed vector %)))))

(defn analyze-failures
  "Analyze all postcondition failures."
  [failures]
  {:real (mapv analyze-failure (map-indexed vector failures))})

(def ^:private mutated-rexeg
  #"(?s)(.*)(\n\s+>> object may have been mutated later into (.+) <<\n)")

(defn- analyze-result
  "Analyze the execution `result`."
  [handle result result-str]
  (let [matches (when (string? result-str)
                  (re-matches mutated-rexeg result-str))]
    (cond-> {:symbolic handle}
      (not (instance? CaughtException result))
      (assoc :real result)
      (seq matches)
      (assoc :real-str (nth matches 1))
      (and (not (seq matches))
           (string? result-str))
      (assoc :real-str result-str)
      (seq matches)
      (assoc :real-mutated (nth matches 3))
      (or (seq matches) (not= result-str (pr-str result)))
      (assoc :real-mutated? true))))

(defn- analyze-sequential-environment
  "Return a map from a command handle to execution frame."
  [{:keys [failures]} cmds-and-traces state bindings & [thread]]
  (first (reduce (fn [[env state bindings]
                      [index [[handle cmd-obj & symbolic-args] result-str result]]]
                   (let [real-args (sv/get-real-value symbolic-args (:real bindings))
                         next-bindings {:real (assoc (:real bindings) handle result)
                                        :symbolic (assoc (:symbolic bindings) handle handle)}
                         next-state {:real (u/make-next-state cmd-obj (:real state) real-args result)
                                     :symbolic (u/make-next-state cmd-obj (:symbolic state) symbolic-args handle)}
                         ;; TODO: Can we do this here? The failures captured by
                         ;; stateful-check are from all sequential and parallel
                         ;; inter-leavings. The failures captured here only from
                         ;; this failing one.
                         ;; failure (u/check-postcondition cmd-obj (:real state) (:real next-state) real-args result)
                         failures (get failures handle)
                         command (analyze-command cmd-obj)
                         num-args (count symbolic-args)
                         frame (cond-> {:arguments (mapv (fn [index real symbolic]
                                                           (analyze-argument
                                                            command num-args
                                                            {:index index
                                                             :real real
                                                             :symbolic symbolic}))
                                                         (range (count symbolic-args)) real-args symbolic-args)
                                        :bindings next-bindings
                                        :command command
                                        :handle handle
                                        :index index
                                        :result (analyze-result handle result result-str)
                                        :state next-state}
                                 (seq failures)
                                 (assoc :failures (analyze-failures failures))
                                 (nat-int? thread)
                                 (assoc :thread thread)
                                 (instance? CaughtException result)
                                 (assoc-in [:error :real] (:exception result)))]
                     [(assoc env handle frame) next-state next-bindings]))
                 [{} state bindings] (map-indexed vector cmds-and-traces))))

(defn- analyze-environments
  "Return a map mapping from a command handle to execution environment."
  [{:keys [specification sequential parallel] :as result-data}]
  (let [setup-fn (:setup specification)
        ;; TODO: Don't call setup, since it might change mutable objects. Get
        ;; initial bindings from somewhere else.
        setup-result (when-let [setup setup-fn]
                       (setup))
        bindings (if setup-fn
                   {:real {g/setup-var setup-result}
                    :symbolic {g/setup-var g/setup-var}}
                   {:real {} :symbolic {}})
        init-state-fn (or (:initial-state specification)
                          (constantly nil))
        init-state (if (:setup specification)
                     {:real (init-state-fn (get bindings g/setup-var))
                      :symbolic (init-state-fn g/setup-var)}
                     {:real (init-state-fn)
                      :symbolic (init-state-fn)})
        sequential-env (analyze-sequential-environment result-data sequential init-state bindings)
        last-env (get sequential-env (ffirst (last sequential)))
        environments (into sequential-env
                           (mapcat (fn [[thread sequential]]
                                     (analyze-sequential-environment
                                      result-data sequential
                                      (:state last-env)
                                      (:bindings last-env)
                                      thread))
                                   (map-indexed vector parallel)))]
    ;; TODO: Don't call setup (see above) and also not cleanup
    (when-let [cleanup (:cleanup specification)]
      (if (:setup specification)
        (cleanup setup-result)
        (cleanup)))
    (assoc environments (sv/->RootVar "init")
           {:bindings bindings
            :handle (sv/->RootVar "init")
            :state init-state})))

(defn- analyze-result-data
  [result-data]
  (let [environments (analyze-environments result-data)
        state-machine (state-machine/make-state-machine result-data)]
    (assoc result-data :environments environments :state-machine state-machine)))

(defn analyze-run
  "Analyze the Stateful Check `results`."
  [{:keys [id result-data pass?] :as results}]
  (let [{:keys [specification]} result-data]
    (cond-> (assoc results :id (str (or id (UUID/randomUUID))))
      (and specification (not pass?))
      (update-in [:result-data] analyze-result-data)
      (and specification (not pass?))
      (update-in [:shrunk :result-data] analyze-result-data))))

(defn analyze-test-run
  "Analyze the Clojure Test report `event`."
  [{:keys [ns var] :as event}]
  (when-let [results (:stateful-check event)]
    (some-> (analyze-run results)
            (update-in [:specification] assoc
                       :ns ns
                       :var var
                       :id (str ns "/" var)
                       :type :test
                       :test event))))
