(ns stateful-check.nrepl.middleware-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is join-fixtures use-fixtures testing]]
            [stateful-check.debugger.eval :as eval]
            [stateful-check.debugger.test :as test]
            [stateful-check.nrepl.test-session :as session])
  (:import [java.util UUID]))

(use-fixtures :each (join-fixtures [session/session-fixture session/middleware-fixture]))

(defn- run-failing-test []
  (session/message {:op "test"
                    :ns "stateful-check.debugger.test"
                    :var "test-records"}))

(deftest test-stateful-check-analysis
  (let [result (session/message {:op "stateful-check/analysis"
                                 :analysis "11111111-1111-1111-1111-111111111111"})]
    (is (= #{"done" "stateful-check/analysis-not-found"} (:status result)))))

(deftest test-stateful-check-analyze-test
  (testing "without a test run"
    (let [result (session/message {:op "stateful-check/analyze-test"
                                   :test "user/not-a-test"})]
      (is (= #{"done" "stateful-check/test-not-found"} (:status result)))))
  (testing "with failing a test run"
    (run-failing-test)
    (let [result (session/message {:op "stateful-check/analyze-test"
                                   :test test/records-failure-test-id})]
      (is (= #{"done"} (:status result)))
      (is (= "false" (:pass? (:stateful-check/analyze-test result)))))))

(deftest test-stateful-check-analyze-no-events
  (let [result (session/message {:op "stateful-check/analyze-test"
                                 :test "unkown/test"})]
    (is (= #{"done" "stateful-check/test-not-found"}
           (:status result)))))

(deftest test-stateful-check-inspect-object-not-found
  (let [result (session/message {:op "stateful-check/inspect"})]
    (is (= #{"done" "stateful-check/object-not-found"} (:status result)))))

(deftest test-stateful-check-print-object-not-found
  (let [result (session/message {:op "stateful-check/print"})]
    (is (= #{"done" "stateful-check/object-not-found"} (:status result)))))

(deftest test-stateful-check-run
  (testing "without specifications"
    (let [result (session/message {:op "stateful-check/run"
                                   :specification test/records-failure-spec-id})]
      (is (= #{"done" "stateful-check/specification-not-found"} (:status result)))
      (is (nil? (:stateful-check/run result)))))
  (testing "with specifications"
    (session/message {:op "stateful-check/scan"})
    (let [result (session/message {:op "stateful-check/run"
                                   :specification test/records-failure-spec-id})]
      (is (= #{"done"} (:status result)))
      (let [results (:stateful-check/run result)]
        (is (= "false" (:pass? results)))))))

(deftest test-stateful-check-scan
  (testing "without tests run"
    (let [result (session/message {:op "stateful-check/scan"})]
      (is (= #{"done"} (:status result)))
      (let [specifications (:stateful-check/scan result)]
        (is (set/subset? #{test/records-failure-spec-id}
                         (set (map :id specifications)))))))
  (testing "with tests run"
    (run-failing-test)
    (let [result (session/message {:op "stateful-check/scan"})]
      (is (= #{"done"} (:status result)))
      (let [specifications (:stateful-check/scan result)]
        (is (set/subset? #{test/records-failure-spec-id
                           test/records-failure-test-id}
                         (set (map :id specifications))))))))

(deftest test-stateful-check-specifications
  (testing "without any specifications"
    (let [result (session/message {:op "stateful-check/specifications"})]
      (is (= #{"done"} (:status result)))
      (is (empty? (:stateful-check/specifications result)))))
  (testing "with specifications"
    (session/message {:op "stateful-check/scan"})
    (let [result (session/message {:op "stateful-check/specifications"})]
      (let [specifications (:stateful-check/specifications result)]
        (is (set/subset? #{test/records-failure-spec-id}
                         (set (map :id specifications)))))))
  (testing "with specifications loaded and tests run"
    (run-failing-test)
    (session/message {:op "stateful-check/scan"})
    (let [result (session/message {:op "stateful-check/specifications"})]
      (let [specifications (:stateful-check/specifications result)]
        (is (set/subset? #{test/records-failure-spec-id
                           test/records-failure-test-id}
                         (set (map :id specifications))))))))

(deftest test-stateful-evaluate-step
  (session/message {:op "stateful-check/scan"})
  (let [run (-> {:op "stateful-check/run"
                 :specification test/records-failure-spec-id}
                session/message :stateful-check/run :id)
        {:keys [stateful-check/eval-step status]}
        (session/message {:op "stateful-check/eval-step"
                          :run run
                          :case "smallest"})]
    (is (= #{"done"} status))
    (is (= run (:id eval-step)))

    ;; (clojure.pprint/pprint
    ;;  (session/message {:op "stateful-check/evaluate-step"
    ;;                    :run run
    ;;                    :case "smallest"}))

    (session/message {:op "stateful-check/evaluate-step"
                      :run run
                      :case "smallest"})

    (session/message {:op "stateful-check/evaluate-step"
                      :run run
                      :case "smallest"})))
