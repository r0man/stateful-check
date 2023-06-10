(ns stateful-check.store-record-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [stateful-check.core :refer :all]
            [stateful-check.report :as report]))

(def records (atom {}))

(defn store-record [value]
  (let [id (gensym "id")]
    (swap! records assoc id value)
    {:id id :value value}))

(defn retrieve-record [id]
  (get @records id))

(defn retrieve-record [id]
  (if (> (count @records) 10)
    "BOOM"
    (get @records id)))

(def store-record-spec
  {:args (fn [_] [gen/int])
   :command #'store-record
   :next-state (fn [state [value] record]
                 (assoc state (:id record) value))})

(def retrieve-record-spec
  {:requires seq
   :args (fn [state]
           [(gen/elements (keys state))])
   :command #'retrieve-record
   :postcondition (fn [state _ [id] value]
                    (is (= (get state id) value)))})

(def spec
  {:commands {:store #'store-record-spec
              :retrieve #'retrieve-record-spec}})

(def options
  {:gen {:threads 2}
   :run {:seed 0 :max-tries 10}
   :report {:first-case? true}})

(deftest test-spec
  (is (specification-correct? spec options)))

;;   First failing test case
;;   -----------------------------
;; Sequential prefix:
;;   #<1> = (:store -3)  = id16060
;;   #<2> = (:store 1)  = id16061
;;   #<3> = (:store 3)  = id16062
;;   #<4> = (:retrieve #<3>)  = "BOOM"
;;     Postcondition returned falsey.
;;   #<5> = (:store 1)  = id16063

;;   Smallest case after shrinking
;;   -----------------------------
;; Sequential prefix:
;;   #<3> = (:store 0)  = id16088
;;   #<4> = (:retrieve #<3>)  = "BOOM"
;;     Postcondition returned falsey.

;; Seed: 0

(def results
  (run-specification spec options))

(report/print-results results)
(->> results :result-data :environment)
