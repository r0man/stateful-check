(ns stateful-check.debugger.core-test
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.test :as test]
            [stateful-check.symbolic-values :as sv]
            [clojure.edn :as edn])
  (:import [java.util UUID]))

(stest/instrument)

(def example-id
  "stateful-check.debugger.test/records-failure-spec")

(def example-specification
  test/records-failure-spec)

(defn- run-specification [specification & [options]]
  (assoc (stateful-check/run-specification specification)
         :specification specification :options options))

(def test-report
  {:summary {:ns 1 :var 1 :test 1 :pass 0 :fail 1 :error 0}
   :results
   {'stateful-check.debugger.test
    {'test-records-error
     [{:index 0
       :ns 'stateful-check.debugger.test
       :file "core.clj"
       :type :fail
       :line 75
       :var 'test-records-error
       :expected "all executions to match specification\n"
       :stateful-check (run-specification test/records-error-spec
                                          test/records-spec-options)
       :context nil
       :actual "the above execution did not match the specification\n"
       :message "Sequential prefix: ..."}]
     'test-records-failure
     [{:index 0
       :ns 'stateful-check.debugger.test
       :file "core.clj"
       :type :fail
       :line 75
       :var 'test-records-failure
       :expected "all executions to match specification\n"
       :stateful-check (run-specification test/records-failure-spec
                                          test/records-spec-options)
       :context nil
       :actual "the above execution did not match the specification\n"
       :message "Sequential prefix: ..."}]}}})

(def debugger
  (debugger/debugger {:test {:report (atom test-report)}}))

(deftest test-ns-specifications
  (is (every? map? (debugger/ns-specifications))))

(deftest test-find-test-event
  (is (nil? (debugger/find-test-event debugger 'a/b)))
  (is (nil? (debugger/find-test-event debugger "a/b")))
  (let [id 'stateful-check.debugger.test/test-records-failure]
    (is (debugger/find-test-event debugger id))
    (is (debugger/find-test-event debugger (str id)))))

(deftest test-analyze-results
  (let [results (run-specification example-specification)
        debugger (debugger/analyze-run debugger results)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-analyze-test-event
  (let [results (run-specification example-specification)
        event {:ns 'user :var 'test :stateful-check results}
        debugger (debugger/analyze-test-run debugger event)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-last-run
  (is (nil? (debugger/last-run debugger)))
  (let [debugger (debugger/scan debugger)]
    (is (nil? (debugger/last-run debugger)))
    (let [debugger (debugger/run-specification debugger example-id)
          run (debugger/last-run debugger)]
      (is (s/valid? :stateful-check/run run)))))

(deftest test-print
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger example-id)
        run (:id (debugger/last-run debugger))]
    (is (string? (with-out-str (debugger/print debugger run))))))

(deftest test-run-specfication
  (let [debugger (debugger/scan debugger)
        debugger (debugger/run-specification debugger example-id test/records-spec-options)]
    (is (s/valid? :stateful-check/debugger debugger))
    (let [environments (-> debugger debugger/last-run :result-data :environments)]
      (testing "environment #1"
        (let [env (get environments (sv/->RootVar "1"))]
          (is (= [{:index 0 :real -3 :symbolic -3 :name "value"}]
                 (:arguments env)))
          (is (= {(sv/->RootVar "setup") {}
                  (sv/->RootVar "1") {:id "id--3" :value -3}}
                 (-> env :bindings :real)))
          (is (= {(sv/->RootVar "setup") (sv/->RootVar "setup")
                  (sv/->RootVar "1") (sv/->RootVar "1")}
                 (-> env :bindings :symbolic)))
          (is (= (sv/->RootVar "1") (:handle env)))
          (is (= 0 (:index env)))
          (is (= {:real {:id "id--3" :value -3}
                  :real-str {:value -3, :id "id--3"}
                  :symbolic (sv/->RootVar "1")}
                 (update (:result env) :real-str edn/read-string)))
          (is (= {:real {"id--3" {:id "id--3" :value -3}}
                  :symbolic {(get (sv/->RootVar "1") :id)
                             {:id (get (sv/->RootVar "1") :id) :value -3}}}
                 (:state env))))))
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-scan
  (let [specs (debugger/specifications (debugger/scan debugger))]
    (is (set/subset?
         #{{:id "stateful-check.debugger.test/records-error-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-error-spec}
           {:id "stateful-check.debugger.test/records-failure-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-failure-spec}
           {:ns 'stateful-check.debugger.test
            :var 'test-records-error
            :id "stateful-check.debugger.test/test-records-error"
            :type :test}
           {:ns 'stateful-check.debugger.test
            :var 'test-records-failure
            :id "stateful-check.debugger.test/test-records-failure"
            :type :test}}
         (set (map #(select-keys % [:id :ns :var :type]) specs))))))

(deftest test-specification
  (is (nil? (debugger/specification debugger example-id)))
  (let [debugger (debugger/scan debugger)
        specification (debugger/specification debugger example-id)]
    (is (s/valid? :stateful-check.debugger/specification specification))
    (is (= (:commands example-specification) (:commands specification)))
    (is (= 'stateful-check.debugger.test (:ns specification)))
    (is (= 'records-failure-spec (:var specification)))
    (is (= :var (:type specification)))
    (is (= "stateful-check.debugger.test/records-failure-spec"
           (:id specification)))))

(deftest test-specifications
  (is (nil? (debugger/specifications debugger)))
  (let [specifications (debugger/specifications (debugger/scan debugger))]
    (is (not-empty specifications))
    (is (every? #(s/valid? :stateful-check.debugger/specification %) specifications))))
