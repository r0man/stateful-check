(ns stateful-check.debugger.specs
  (:require [clojure.spec.alpha :as s]
            [stateful-check.specs]
            [stateful-check.symbolic-values])
  (:import [stateful_check.symbolic_values RootVar LookupVar]))

;; Debugger Specification

(s/def :stateful-check.debugger.specification/id string?)
(s/def :stateful-check.debugger.specification/ns simple-symbol?)
(s/def :stateful-check.debugger.specification/type #{:test :var})
(s/def :stateful-check.debugger.specification/var simple-symbol?)

(s/def :stateful-check.debugger/specification
  (s/merge :stateful-check/specification
           (s/keys :req-un [:stateful-check.debugger.specification/id
                            :stateful-check.debugger.specification/type]
                   :opt-un [:stateful-check.debugger.specification/ns
                            :stateful-check.debugger.specification/var])))

;; Debugger Argument

(s/def :stateful-check.debugger.argument/evaluation any?)
(s/def :stateful-check.debugger.argument/index nat-int?)
(s/def :stateful-check.debugger.argument/name string?)
(s/def :stateful-check.debugger.argument/real any?)
(s/def :stateful-check.debugger.argument/symbolic any?)

(s/def :stateful-check.debugger/argument
  (s/keys :req-un [:stateful-check.debugger.argument/index
                   :stateful-check.debugger.argument/real
                   :stateful-check.debugger.argument/symbolic]
          :opt-un [:stateful-check.debugger.argument/evaluation
                   :stateful-check.debugger.argument/name]))

(s/def :stateful-check.debugger/arguments
  (s/coll-of  :stateful-check.debugger/argument :kind vector?))

;; Debugger Binding

(s/def :stateful-check.debugger.bindings/evaluation :stateful-check/bindings)
(s/def :stateful-check.debugger.bindings/real :stateful-check/bindings)
(s/def :stateful-check.debugger.bindings/symbolic :stateful-check/bindings)

(s/def :stateful-check.debugger/bindings
  (s/keys :req-un [:stateful-check.debugger.bindings/real
                   :stateful-check.debugger.bindings/symbolic]
          :opt-un [:stateful-check.debugger.bindings/evaluation]))

;; Debugger Error

(s/def :stateful-check.debugger.error/evaluation #(instance? Throwable %))
(s/def :stateful-check.debugger.error/real #(instance? Throwable %))
(s/def :stateful-check.debugger.error/symbolic #(instance? Throwable %))

(s/def :stateful-check.debugger/error
  (s/keys :opt-un [:stateful-check.debugger.error/evaluation
                   :stateful-check.debugger.error/symbolic
                   :stateful-check.debugger.error/real]))

;; Debugger Result

(s/def :stateful-check.debugger.result/evaluation any?)
(s/def :stateful-check.debugger.result/real any?)
(s/def :stateful-check.debugger.result/symbolic any?)

(s/def :stateful-check.debugger/result
  (s/keys :req-un [:stateful-check.debugger.result/symbolic]
          :opt-un [:stateful-check.debugger.result/evaluation
                   :stateful-check.debugger.result/real]))

;; Debugger Environment

(s/def :stateful-check.debugger.environment/arguments :stateful-check.debugger/arguments)
(s/def :stateful-check.debugger.environment/bindings :stateful-check.debugger/bindings)
(s/def :stateful-check.debugger.environment/command :stateful-check/command)
(s/def :stateful-check.debugger.environment/error :stateful-check.debugger/error)
(s/def :stateful-check.debugger.environment/handle :stateful-check.symbolic-value/root)
(s/def :stateful-check.debugger.environment/index nat-int?)
(s/def :stateful-check.debugger.environment/result :stateful-check.debugger/result)

(s/def :stateful-check.debugger/environment
  (s/keys :req-un [:stateful-check.debugger.environment/bindings
                   :stateful-check.debugger.environment/handle]
          :opt-un [:stateful-check.debugger.environment/arguments
                   :stateful-check.debugger.environment/command
                   :stateful-check.debugger.environment/error
                   :stateful-check.debugger.environment/index
                   :stateful-check.debugger.environment/result]))

(s/def :stateful-check.debugger/environments
  (s/map-of :stateful-check.symbolic-value/root :stateful-check.debugger/environment))

;; Debugger Result Data

(s/def :stateful-check.debugger/result-data
  (s/merge :stateful-check.run/result-data
           (s/keys :opt-un [:stateful-check.debugger/environments])))

;; Debugger Run

(s/def :stateful-check.debugger.run/id string?)
(s/def :stateful-check.debugger.run/ns simple-symbol?)
(s/def :stateful-check.debugger.run/var simple-symbol?)

(s/def :stateful-check.debugger/run
  (s/merge :stateful-check/run
           (s/keys :req-un [:stateful-check.debugger.run/id]
                   :opt-un [:stateful-check.debugger.run/ns
                            :stateful-check.debugger.run/var
                            :stateful-check.debugger/result-data])))

;; Debugger

(s/def :stateful-check.debugger/last-runs
  (s/coll-of :stateful-check.debugger.run/id :kind vector?))

(s/def :stateful-check.debugger/runs
  (s/map-of :stateful-check.debugger.run/id
            :stateful-check.debugger/run))

(s/def :stateful-check.debugger/specifications
  (s/map-of :stateful-check.debugger.specification/id
            :stateful-check.debugger/specification))

(s/def :stateful-check/debugger
  (s/keys :req-un [:stateful-check.debugger/last-runs
                   :stateful-check.debugger/specifications
                   :stateful-check.debugger/runs]))
