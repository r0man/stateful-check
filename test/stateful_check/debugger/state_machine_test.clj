(ns stateful-check.debugger.state-machine-test
  (:require [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.state-machine :as state-machine]
            [stateful-check.debugger.test :as test]))

(stest/instrument)

(def debugger
  (debugger/scan (debugger/debugger)))

(deftest test-make-specification
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-failure-spec-id test/records-spec-options)
        {:keys [result-data]} (debugger/last-run debugger)]
    (is (= {:state #{"init"}
            :definition
            {#{"final"} {:reset #{"init"}}
             #{"init"} {:start #{"1"}}
             #{"1"} {:fail #{"1"} :pass #{"2"} :stop #{"final"}}
             #{"2"} {:fail #{"2"} :pass #{"3"} :stop #{"final"}}
             #{"3"} {:fail #{"3"} :pass #{"4"} :stop #{"final"}}
             #{"4"} {:fail #{"4"} :pass #{"5"} :stop #{"final"}}
             #{"5"} {:pass #{"final"}}}}
           (state-machine/make-state-machine result-data)))))

(deftest test-get-next-state
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-failure-spec-id test/records-spec-options)
        {:keys [result-data]} (debugger/last-run debugger)
        state-machine (state-machine/make-state-machine result-data)]
    (is (= #{"1"} (state-machine/get-next-state state-machine :start)))
    (is (= #{"2"} (-> state-machine
                      (state-machine/update-next-state :start)
                      (state-machine/get-next-state :pass))))
    (is (= #{"3"} (-> state-machine
                      (state-machine/update-next-state :start)
                      (state-machine/update-next-state :pass)
                      (state-machine/get-next-state :pass))))))

(deftest test-update-next-state
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger test/records-failure-spec-id test/records-spec-options)
        {:keys [result-data]} (debugger/last-run debugger)
        state-machine (state-machine/make-state-machine result-data)]
    (is (= #{"1"} (-> state-machine
                      (state-machine/update-next-state :start)
                      :state)))))
