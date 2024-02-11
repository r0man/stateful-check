(ns stateful-check.debugger.test-report
  (:require [clojure.spec.alpha :as s]
            [stateful-check.debugger.specs]))

(defn events
  "Return all test events from the CIDER test `report`."
  [report]
  (->> report :results vals
       (mapcat vals)
       (apply concat)))

(defn stateful-check-event?
  "Return true if `event` is a Stateful Check test event, otherwise false."
  [{:keys [stateful-check type]}]
  (and (contains? #{:error :pass :fail} type)
       (map? stateful-check)
       ;; (s/valid? :stateful-check/specification (:specification stateful-check))
       ))

(defn- enhance-event
  "Enhance the test `event` by adding :ns, :var, :id, and :type to the specification."
  [{:keys [ns var] :as event}]
  (cond-> event
    (stateful-check-event? event)
    (update-in [:stateful-check :specification] assoc
               :ns ns
               :var var
               :id (str ns "/" var)
               :type :test)))

(defn stateful-check-events
  "Return all Stateful Check events from the CIDER test `report`."
  [report]
  (map enhance-event (filter stateful-check-event? (events report))))

(defn specifications
  "Return all Stateful Check specifications from the CIDER test `report`."
  [report]
  (into #{} (map (comp :specification :stateful-check))
        (stateful-check-events report)))

(defn find-events
  "Find the Stateful Check test events for `ns` and `var`."
  [report ns var]
  (->> (get-in report [:results (symbol ns) (symbol var)])
       (filter stateful-check-event?)
       (map enhance-event)))
