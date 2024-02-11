(ns stateful-check.debugger.test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer [specification-correct?]]))

(def records-failure-spec-id
  "stateful-check.debugger.test/records-failure-spec")

(def records-error-spec-id
  "stateful-check.debugger.test/records-error-spec")

(def records-failure-test-id
  "stateful-check.debugger.test/test-records-failure")

(def records (atom {}))

(defn store-record-failure [value]
  (let [id (str "id-" value)
        value (if (>= (count @records) 2) "boom" value)]
    (swap! records assoc id {:id id :value value})
    {:id id :value value}))

(defn store-record-error [value]
  (let [id (str "id-" value)]
    (if (>= (count @records) 2)
      (throw (ex-info "BOOM" {:id id :value value})))
    (swap! records assoc id {:id id :value value})
    {:id id :value value}))

(defn retrieve-record [id]
  (get @records id))

(def store-record-error-spec
  {:args (fn [_] [gen/small-integer])
   :command #'store-record-error
   :next-state (fn [state [value] record]
                 (assoc state (:id record)
                        {:id (:id record)
                         :value value}))})

(def store-record-failure-spec
  {:args (fn [_] [gen/small-integer])
   :command #'store-record-failure
   :next-state (fn [state [value] record]
                 (assoc state (:id record)
                        {:id (:id record)
                         :value value}))})

(def retrieve-record-spec
  {:requires seq
   :args (fn [state]
           [(gen/elements (keys state))])
   :command #'retrieve-record
   :postcondition (fn [state _ [id] value]
                    (is (= (get state id) value)))})

(def records-error-spec
  {:commands {:store #'store-record-error-spec
              :retrieve #'retrieve-record-spec}
   :setup #(reset! records {})
   :initial-state (constantly {})
   :cleanup (fn [_] (reset! records {}))})

(def records-failure-spec
  {:commands {:store #'store-record-failure-spec
              :retrieve #'retrieve-record-spec}
   :setup #(reset! records {})
   :initial-state (constantly {})
   :cleanup (fn [_] (reset! records {}))})

(def records-spec-options
  {:gen {:threads 0}
   :report {:first-case? true}
   :run {:seed 100}})

;; Define 2 non-passing, interactive tests.

(deftest ^:interactive test-records-error
  (is (specification-correct? records-error-spec records-spec-options)))

(deftest ^:interactive test-records-failure
  (is (specification-correct? records-failure-spec records-spec-options)))
