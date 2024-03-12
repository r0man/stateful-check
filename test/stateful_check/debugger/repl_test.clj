(ns stateful-check.debugger.repl-test
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.repl :as repl]
            [stateful-check.debugger.test :as test]
            [stateful-check.symbolic-values :as sv])
  (:import [java.util UUID]))

(stest/instrument)

(def ^:private spec-id
  "stateful-check.debugger.test/records-failure-spec")

(def ^:private spec-options
  test/records-spec-options)

(def ^:private root-var sv/->RootVar)

(deftest test-clear
  (let [debugger (repl/clear)]
    (is (s/valid? :stateful-check/debugger debugger))
    (is (= (debugger/debugger) debugger))))

(deftest test-get-env
  (repl/reset)
  (is (nil? (repl/get-env)))
  (repl/run-specification spec-id spec-options)
  (let [env (repl/get-env :case :first :handle "1")]
    (is (= (root-var "1") (:handle env))))
  (let [env (repl/get-env :handle "2")]
    (is (= (root-var "2") (:handle env)))))

(deftest test-get-run
  (repl/reset)
  (is (nil? (repl/get-run)))
  (let [run-1 (repl/run-specification spec-id spec-options)]
    (is (s/valid? :stateful-check.debugger/run run-1))
    (is (= run-1 (repl/get-run)))
    (is (= run-1 (repl/get-run :id (:id run-1))))
    (is (= run-1 (repl/get-run :run (:id run-1))))
    (let [run-2 (repl/run-specification spec-id spec-options)]
      (is (not= run-1 run-2))
      (is (s/valid? :stateful-check.debugger/run run-2))
      (is (= run-2 (repl/get-run)))
      (is (= run-2 (repl/get-run :id (:id run-2))))
      (is (= run-2 (repl/get-run :run (:id run-2)))))))

(deftest test-run-specification
  (repl/reset)
  (let [run (repl/run-specification spec-id spec-options)]
    (is (s/valid? :stateful-check.debugger/run run))
    (testing "first failing case"
      (testing "arguments"
        (is (= [{:index 0, :real -3, :symbolic -3, :name "value"}]
               (repl/get-arguments :case :first :handle "1")))
        (is (= [{:index 0, :real 2, :symbolic 2, :name "value"}]
               (repl/get-arguments :case :first :handle "2")))
        (is (= [{:index 0, :real -2, :symbolic -2, :name "value"}]
               (repl/get-arguments :case :first :handle "3")))
        (is (= [{:index 0, :real -4, :symbolic -4, :name "value"}]
               (repl/get-arguments :case :first :handle "4")))
        (is (= [{:index 0, :real "id--4", :symbolic (get (root-var "4") :id), :name "id"}]
               (repl/get-arguments :case :first :handle "5"))))
      (testing "bindings"
        (is (= {:real
                {(root-var "setup") {}
                 (root-var "1") {:id "id--3", :value -3}},
                :symbolic
                {(root-var "setup") (root-var "setup")
                 (root-var "1") (root-var "1")}}
               (repl/get-bindings :case :first :handle "1")))
        (is (= {:real
                {(root-var "setup") {},
                 (root-var "1") {:id "id--3", :value -3},
                 (root-var "2") {:id "id-2", :value 2}},
                :symbolic
                {(root-var "setup") (root-var "setup")
                 (root-var "1") (root-var "1")
                 (root-var "2") (root-var "2")}}
               (repl/get-bindings :case :first :handle "2")))
        (is (= {:real
                {(root-var "setup") {},
                 (root-var "1") {:id "id--3", :value -3},
                 (root-var "2") {:id "id-2", :value 2},
                 (root-var "3") {:id "id--2", :value "boom"}},
                :symbolic
                {(root-var "setup") (root-var "setup")
                 (root-var "1") (root-var "1")
                 (root-var "2") (root-var "2")
                 (root-var "3") (root-var "3")}}
               (repl/get-bindings :case :first :handle "3")))
        (is (= {:real
                {(root-var "setup") {},
                 (root-var "1") {:id "id--3", :value -3},
                 (root-var "2") {:id "id-2", :value 2},
                 (root-var "3") {:id "id--2", :value "boom"},
                 (root-var "4") {:id "id--4", :value "boom"}},
                :symbolic
                {(root-var "setup") (root-var "setup")
                 (root-var "1") (root-var "1")
                 (root-var "2") (root-var "2")
                 (root-var "3") (root-var "3")
                 (root-var "4") (root-var "4")}}
               (repl/get-bindings :case :first :handle "4")))
        (is (= {:real
                {(root-var "setup") {},
                 (root-var "1") {:id "id--3", :value -3},
                 (root-var "2") {:id "id-2", :value 2},
                 (root-var "3") {:id "id--2", :value "boom"},
                 (root-var "4") {:id "id--4", :value "boom"},
                 (root-var "5") {:id "id--4", :value "boom"}},
                :symbolic
                {(root-var "setup") (root-var "setup"),
                 (root-var "1") (root-var "1"),
                 (root-var "2") (root-var "2"),
                 (root-var "3") (root-var "3"),
                 (root-var "4") (root-var "4"),
                 (root-var "5") (root-var "5")}}
               (repl/get-bindings :case :first :handle "5")))))
    (testing "smallest failing case"
      (testing "arguments"
        (is (= [{:index 0, :real 0, :symbolic 0, :name "value"}]
               (repl/get-arguments :case :smallest :handle "2")))
        (is (= [{:index 0, :real -1, :symbolic -1, :name "value"}]
               (repl/get-arguments :case :smallest :handle "3")))
        (is (= [{:index 0, :real 0, :symbolic 0, :name "value"}]
               (repl/get-arguments :case :smallest :handle "4")))
        (is (= [{:index 0, :real "id-0", :symbolic (get (root-var "2") :id), :name "id"}]
               (repl/get-arguments :case :smallest :handle "5")))))))

(deftest test-run-specification-map
  (repl/reset)
  (let [run (repl/run-specification test/records-failure-spec spec-options)]
    (is (s/valid? :stateful-check.debugger/run run))
    (is (-> run :specification :id UUID/fromString))
    (is (-> run :specification :id repl/specification)))
  (let [run (repl/run-specification test/records-error-spec spec-options)]
    (is (s/valid? :stateful-check.debugger/run run))
    (is (-> run :specification :id UUID/fromString))
    (is (-> run :specification :id repl/specification))))

(deftest test-run-specification-var
  (repl/reset)
  (let [run (repl/run-specification #'test/records-failure-spec spec-options)]
    (is (s/valid? :stateful-check.debugger/run run))
    (is (= "stateful-check.debugger.test/records-failure-spec"
           (-> run :specification :id)))
    (is (-> run :specification :id repl/specification)))
  (let [run (repl/run-specification #'test/records-error-spec spec-options)]
    (is (s/valid? :stateful-check.debugger/run run))
    (is (= "stateful-check.debugger.test/records-error-spec"
           (-> run :specification :id)))
    (is (-> run :specification :id repl/specification))))

(deftest test-evaluate-step
  (repl/reset)
  (repl/run-specification spec-id spec-options)
  (is (= #{"init"} (repl/get-eval-state :case :first)))
  (testing "step to #<1>"
    (repl/eval-step :case :first)
    (is (= #{"1"} (repl/get-eval-state :case :first)))
    (is (= {(root-var "setup") {}}
           (:evaluation (repl/get-bindings :case :first :handle "init"))
           (:real (repl/get-bindings :case :first :handle "init"))))
    (is (= {}
           (:evaluation (repl/get-state :case :first :handle "init"))
           (:real (repl/get-state :case :first :handle "init")))))
  (testing "step to #<2>"
    (repl/eval-step :case :first)
    (is (= #{"2"} (repl/get-eval-state :case :first)))
    (is (= {(root-var "setup") {}
            (root-var "1") {:id "id--3", :value -3}}
           (:real (repl/get-bindings :case :first :handle "1"))
           (:evaluation (repl/get-bindings :case :first :handle "1"))))
    (is (= {"id--3" {:id "id--3", :value -3}}
           (:evaluation (repl/get-state :case :first :handle "1"))
           (:real (repl/get-state :case :first :handle "1")))))
  (testing "step to #<3>"
    (repl/eval-step :case :first)
    (is (= #{"3"} (repl/get-eval-state :case :first)))
    (is (= {(root-var "setup") {}
            (root-var "1") {:id "id--3", :value -3}
            (root-var "2") {:id "id-2", :value 2}}
           (:evaluation (repl/get-bindings :case :first :handle "2"))
           (:real (repl/get-bindings :case :first :handle "2"))))
    (is (= {"id--3" {:id "id--3", :value -3}
            "id-2" {:id "id-2", :value 2}}
           (:evaluation (repl/get-state :case :first :handle "2"))
           (:real (repl/get-state :case :first :handle "2")))))
  (testing "step to #<4>"
    (repl/eval-step :case :first)
    (is (= #{"4"} (repl/get-eval-state :case :first)))
    (is (= {(root-var "setup") {}
            (root-var "1") {:id "id--3", :value -3}
            (root-var "2") {:id "id-2", :value 2}
            (root-var "3") {:id "id--2", :value "boom"}}
           (:real (repl/get-bindings :case :first :handle "3"))
           (:evaluation (repl/get-bindings :case :first :handle "3"))))
    (is (= {"id--3" {:id "id--3", :value -3},
            "id-2" {:id "id-2", :value 2},
            "id--2" {:id "id--2", :value -2}}
           (:evaluation (repl/get-state :case :first :handle "3"))
           (:real (repl/get-state :case :first :handle "3")))))
  (testing "step to #<5>"
    (repl/eval-step :case :first)
    (is (= #{"5"} (repl/get-eval-state :case :first)))
    (is (= {(root-var "setup") {}
            (root-var "1") {:id "id--3", :value -3}
            (root-var "2") {:id "id-2", :value 2}
            (root-var "3") {:id "id--2", :value "boom"}
            (root-var "4") {:id "id--4", :value "boom"}}
           (:evaluation (repl/get-bindings :case :first :handle "4"))
           (:real (repl/get-bindings :case :first :handle "4"))))
    (is (= {"id--3" {:id "id--3", :value -3},
            "id-2" {:id "id-2", :value 2},
            "id--2" {:id "id--2", :value -2}
            "id--4" {:id "id--4", :value -4}}
           (:evaluation (repl/get-state :case :first :handle "4"))
           (:real (repl/get-state :case :first :handle "4")))))
  (testing "step to #<final>"
    (repl/eval-step :case :first)
    (is (= #{"final"} (repl/get-eval-state :case :first)))
    (is (= {(root-var "setup") {}
            (root-var "1") {:id "id--3", :value -3}
            (root-var "2") {:id "id-2", :value 2}
            (root-var "3") {:id "id--2", :value "boom"}
            (root-var "4") {:id "id--4", :value "boom"}
            (root-var "5") {:id "id--4", :value "boom"}}
           (:evaluation (repl/get-bindings :case :first :handle "5"))
           (:real (repl/get-bindings :case :first :handle "5"))))
    (is (= {"id--3" {:id "id--3", :value -3},
            "id-2" {:id "id-2", :value 2},
            "id--2" {:id "id--2", :value -2}
            "id--4" {:id "id--4", :value -4}}
           (:evaluation (repl/get-state :case :first :handle "5"))
           (:real (repl/get-state :case :first :handle "5"))))
    (let [{:keys [failures]} (repl/get-env :case :first :handle "5")]
      (is (= [{:actual '(not (= {:id "id--4", :value -4}
                                {:id "id--4", :value "boom"})),
               :expected '(= (get state id) value),
               :index 0}]
             (->> failures :evaluation first :events
                  (map #(select-keys % [:actual :expected :index])))
             (->> failures :real first :events
                  (map #(select-keys % [:actual :expected :index])))))))
  (testing "step to back to #<init>"
    (repl/eval-step :case :first)
    (is (= #{"init"} (repl/get-eval-state :case :first)))))

(deftest test-evaluate-stop
  (repl/reset)
  (repl/run-specification spec-id spec-options)
  (is (= #{"init"} (repl/get-eval-state :case :first)))
  (testing "stop after to #<1>"
    (repl/eval-step :case :first)
    (repl/eval-stop :case :first)
    (is (= #{"final"} (repl/get-eval-state :case :first))))
  (testing "start again, and stop after to #<2>"
    (repl/eval-step :case :first)
    (is (= #{"init"} (repl/get-eval-state :case :first)))
    (repl/eval-step :case :first)
    (is (= #{"1"} (repl/get-eval-state :case :first)))
    (repl/eval-step :case :first)
    (is (= #{"2"} (repl/get-eval-state :case :first)))
    (repl/eval-stop :case :first)
    (is (= #{"final"} (repl/get-eval-state :case :first)))))

(deftest test-reset
  (let [debugger (repl/reset)]
    (is (s/valid? :stateful-check/debugger debugger))))

(deftest test-scan
  (repl/clear)
  (is (nil? (repl/specifications)))
  (let [specifications (repl/scan)]
    (is (seq specifications))
    (is (every? #(s/valid? :stateful-check.debugger/specification %) specifications))
    (is (= specifications (repl/specifications)))))

(deftest test-specification
  (repl/reset)
  (let [id "stateful-check.debugger.test/records-failure-spec"
        specification (repl/specification id)]
    (is (s/valid? :stateful-check/specification specification))
    (is (= {:id "stateful-check.debugger.test/records-failure-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-failure-spec}
           (select-keys specification [:id :ns :var :type])))))

(deftest test-specifications
  (repl/reset)
  (let [specifications (repl/specifications)]
    (is (seq specifications))
    (is (every? #(s/valid? :stateful-check/specification %) specifications))
    (is (set/subset?
         #{{:id "stateful-check.debugger.test/records-error-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-error-spec}
           {:id "stateful-check.debugger.test/records-failure-spec"
            :ns 'stateful-check.debugger.test
            :type :var
            :var 'records-failure-spec}}
         (set (map #(select-keys % [:id :ns :var :type]) specifications))))))
