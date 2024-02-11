(ns stateful-check.debugger.state-machine
  (:require [stateful-check.symbolic-values])
  (:import [stateful_check.symbolic_values RootVar]))

(def ^:private init-state #{"init"})
(def ^:private final-state #{"final"})

(defn- var-name [^RootVar var]
  (.-name var))

(defn- sequential-state [executions index]
  (some->> (ffirst (nth executions index nil)) var-name vector set))

(defn- parallel-state [executions index]
  (set (map var-name (keep #(ffirst (nth % index nil)) executions))))

(defn- last-sequential-state [sequential]
  (sequential-state sequential (dec (count sequential))))

(defn- last-parallel-state [parallel]
  (parallel-state parallel (dec (apply max (map count parallel)))))

(defn- add-sequential-executions
  [state-machine {:keys [sequential]}]
  (let [executions (vec sequential)]
    (reduce (fn [state-machine index]
              (let [current-state (sequential-state executions index)]
                (if-let [next-state (sequential-state executions (inc index))]
                  (->  state-machine
                       (assoc-in [current-state :fail] current-state)
                       (assoc-in [current-state :pass] next-state)
                       (assoc-in [current-state :stop] final-state))
                  state-machine)))
            state-machine (range (count executions)))))

(defn- add-parallel-executions
  [state-machine {:keys [parallel]}]
  (if (seq parallel)
    (let [executions (vec parallel)]
      (reduce (fn [state-machine index]
                (let [current-state (parallel-state executions index)
                      next-state (parallel-state executions (inc index))]
                  (if (and (seq current-state) (seq next-state))
                    (->  state-machine
                         (assoc-in [current-state :fail] current-state)
                         (assoc-in [current-state :pass] next-state)
                         (assoc-in [current-state :stop] final-state))
                    state-machine)))
              state-machine (range (apply max (map count executions)))))
    state-machine))

(defn connect-executions
  [state-machine {:keys [sequential parallel]}]
  (if (and (seq sequential) (seq parallel))
    (let [current-state (last-sequential-state sequential)
          next-state (parallel-state parallel 0)]
      (-> state-machine
          (assoc-in [current-state :fail] current-state)
          (assoc-in [current-state :pass] next-state)
          (assoc-in [current-state :stop] final-state)))
    state-machine))

(defn- add-start
  [state-machine {:keys [sequential parallel]}]
  (assoc-in state-machine [init-state :start]
            (cond (seq sequential)
                  (sequential-state sequential 0)
                  (seq parallel)
                  (parallel-state parallel 0)
                  :else final-state)))

(defn- add-end
  [state-machine {:keys [sequential parallel]}]
  (cond (seq parallel)
        (assoc-in state-machine [(last-parallel-state parallel) :pass] final-state)
        (seq sequential)
        (assoc-in state-machine [(last-sequential-state sequential) :pass] final-state)))

(defn make-state-machine
  [result-data]
  {:state init-state
   :definition
   (-> {final-state {:reset init-state}}
       (add-start result-data)
       (add-sequential-executions result-data)
       (add-parallel-executions result-data)
       (connect-executions result-data)
       (add-end result-data))})

(defn- current-transitions [state-machine]
  (get-in state-machine [:definition (:state state-machine)]))

(defn- valid-transition?
  [state-machine transition]
  (contains? (current-transitions state-machine) transition))

(defn get-next-state
  [state-machine transition]
  (let [current-state (:state state-machine)]
    (get-in state-machine [:definition current-state transition])))

(defn update-next-state
  [state-machine transition]
  (if (valid-transition? state-machine transition)
    (assoc state-machine :state (get-next-state state-machine transition))
    (throw (ex-info "Invalid transition"
                    {:state-machine state-machine
                     :transition transition
                     :transitions (vec (keys (current-transitions state-machine)))}))))
