(ns stateful-check.debugger.eval
  (:require [stateful-check.command-utils :as u]
            [stateful-check.debugger.analyzer :as analyzer]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r]
            [stateful-check.symbolic-values :as sv])
  (:import [stateful_check.runner CaughtException]))

(defn- get-environment [result-data handle & keys]
  (get-in result-data (concat [:environments handle] keys)))

(defn- clear-environments [environments]
  (into {} (for [[handle environment] environments]
             [handle (-> environment
                         (update :bindings dissoc :evaluation)
                         (update :error dissoc :evaluation)
                         (update :failures dissoc :evaluation)
                         (update :result dissoc :evaluation)
                         (update :state dissoc :evaluation))])))

(defn- start
  [{:keys [specification] :as result-data}]
  (let [{:keys [setup initial-state]} specification
        setup-result (when (ifn? setup) (setup))
        bindings (if setup {g/setup-var setup-result} {})
        state (when initial-state
                (if setup
                  (initial-state setup-result)
                  (initial-state)))]
    (-> (update result-data :environments clear-environments)
        (assoc-in [:environments (sv/->RootVar "init") :bindings :evaluation] bindings)
        (assoc-in [:environments (sv/->RootVar "init") :state :evaluation] state)
        (update :state-machine state-machine/update-next-state :start))))

(defn- reset [result-data]
  (-> (update result-data :environments clear-environments)
      (update :state-machine state-machine/update-next-state :reset)))

(defn- setup-result
  [{:keys [environments] :as result-data}]
  (get-in environments [(sv/->RootVar "init") :bindings :evaluation (sv/->RootVar "setup")]))

(defn- stop
  [{:keys [specification] :as result-data}]
  (when-let [cleanup (:cleanup specification)]
    (if (:setup specification)
      (cleanup (setup-result result-data))
      (cleanup)))
  (update result-data :state-machine state-machine/update-next-state :stop))

(defn- previous-handle
  [{:keys [environments sequential parallel]} current-handle]
  (let [{:keys [index thread]} (get environments current-handle)]
    (cond
      ;; Sequential, first command
      (and (nil? thread) (zero? index))
      (sv/->RootVar "init")
      ;; Sequential, not first command
      (and (nil? thread) (pos-int? index))
      (ffirst (nth sequential (dec index)))
      ;; Parallel, first command, no sequential
      (and (nat-int? thread) (zero? index) (not (seq sequential)))
      (sv/->RootVar "init")
      ;; Parallel, first command, sequential
      (and (nat-int? thread) (zero? index) (seq sequential))
      (ffirst (last sequential))
      ;; Parallel, not first command
      (and (nat-int? thread) (pos-int? index))
      (ffirst (nth (nth parallel thread) (dec index))))))

(defn- previous-env
  [{:keys [environments] :as result-data} current-handle]
  (get environments (previous-handle result-data current-handle)))

(defn- get-arguments
  [result-data current-handle]
  (get-environment result-data current-handle :arguments))

(defn- get-bindings
  [result-data current-handle]
  (:evaluation (:bindings (previous-env result-data current-handle))))

(defn- get-state
  [result-data current-handle]
  (:evaluation (:state (previous-env result-data current-handle))))

(defn- execute-command [result-data handle]
  (let [handle (sv/->RootVar handle)
        cmd-obj (get-environment result-data handle :command)
        arguments (get-arguments result-data handle)
        bindings (get-bindings result-data handle)
        real-args (sv/get-real-value (map :symbolic arguments) bindings)
        state (get-state result-data handle)
        result (try (apply (:command cmd-obj) real-args)
                    (catch Throwable exception
                      (r/->CaughtException exception)))
        next-state (u/make-next-state cmd-obj state real-args result)
        failure (u/check-postcondition cmd-obj state next-state real-args result)]
    (cond-> {:arguments arguments
             :bindings (assoc bindings handle result)
             :command cmd-obj
             :handle handle
             :state next-state}
      failure
      (assoc :failures [(analyzer/analyze-failure [0 failure])])
      (instance? CaughtException result)
      (assoc :error (:exception result))
      (not (instance? CaughtException result))
      (assoc :result result))))

(defn- add-evaluation
  [result-data {:keys [bindings error failures handle state result] :as execution}]
  ;; TODO: Add arguments
  (cond-> result-data
    ;; Bindings
    true
    (assoc-in [:environments handle :bindings :evaluation] bindings)
    ;; State
    true
    (assoc-in [:environments handle :state :evaluation] state)
    ;; Error
    (contains? execution :error)
    (assoc-in [:environments handle :error :evaluation] error)
    (not (contains? execution :error))
    (update-in [:environments handle :error] dissoc :evaluation)
    ;; Failure
    (seq failures)
    (assoc-in [:environments handle :failures :evaluation] failures)
    (not (seq failures))
    (update-in [:environments handle :failures] dissoc :evaluation)
    ;; Result
    (contains? execution :result)
    (assoc-in [:environments handle :result :evaluation] result)
    (not (contains? execution :result))
    (update-in [:environments handle :result] dissoc :evaluation)))

(defn- execute-commands
  [{:keys [state-machine] :as result-data}]
  (let [evaluations (map deref (mapv #(future (execute-command result-data %))
                                     (:state state-machine)))]
    (-> (reduce add-evaluation result-data evaluations)
        (update :state-machine state-machine/update-next-state :pass))))

(defn- eval-step* [result-data]
  (let [current-state (-> result-data :state-machine :state)
        next-result-data (cond (= #{"init"} current-state)
                               (start result-data)
                               (= #{"final"} current-state)
                               (reset result-data)
                               (set? current-state)
                               (execute-commands result-data))]
    next-result-data))

(defn- eval-stop* [result-data]
  (let [current-state (-> result-data :state-machine :state)
        next-result-data (cond (= #{"init"} current-state)
                               result-data
                               (= #{"final"} current-state)
                               result-data
                               (set? current-state)
                               (stop result-data))]
    next-result-data))

(defn eval-step [run case]
  (if (= "first" (some-> case name))
    (update run :result-data eval-step*)
    (update-in run [:shrunk :result-data] eval-step*)))

(defn eval-stop [run case]
  (if (= "first" (some-> case name))
    (update run :result-data eval-stop*)
    (update-in run [:shrunk :result-data] eval-stop*)))
