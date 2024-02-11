(ns stateful-check.specs
  (:require [clojure.spec.alpha :as s]
            [stateful-check.runner]
            [stateful-check.symbolic-values])
  (:import [clojure.lang Var]
           [stateful_check.runner CaughtException]
           [stateful_check.symbolic_values RootVar LookupVar]))

;; Symbolic Values

(s/def :stateful-check.symbolic-value/lookup
  #(instance? LookupVar %))

(s/def :stateful-check.symbolic-value/root
  #(instance? RootVar %))

(s/def :stateful-check/symbolic-value
  (s/or :lookup :stateful-check.symbolic-value/lookup
        :root :stateful-check.symbolic-value/root))

;; Bindings

(s/def :stateful-check/bindings
  (s/map-of :stateful-check.symbolic-value/root any?))

;; Command

(s/def :stateful-check.command/args
  (s/or :ifn ifn? :var #(instance? Var %)))

(s/def :stateful-check.command/command
  (s/or :ifn ifn? :var #(instance? Var %)))

(s/def :stateful-check.command/next-state
  (s/or :ifn ifn? :var #(instance? Var %)))

(s/def :stateful-check/command
  (s/keys :req-un [:stateful-check.command/args
                   :stateful-check.command/command]
          :opt-un [:stateful-check.command/next-state]))

;; Specification

(s/def :stateful-check.specification/command
  (s/or :map :stateful-check/command :var #(instance? Var %)))

(s/def :stateful-check.specification/commands
  (s/map-of keyword? :stateful-check.specification/command))

(s/def :stateful-check.specification/cleanup
  (s/or :ifn ifn? :var #(instance? Var %)))

(s/def :stateful-check.specification/generate-command
  (s/or :ifn ifn? :var #(instance? Var %)))

(s/def :stateful-check.specification/initial-state
  (s/or :ifn ifn? :var #(instance? Var %)))

(s/def :stateful-check.specification/setup
  (s/or :ifn ifn? :var #(instance? Var %)))

(s/def :stateful-check/specification
  (s/keys :req-un [:stateful-check.specification/commands]
          :opt-un [:stateful-check.specification/cleanup
                   :stateful-check.specification/generate-command
                   :stateful-check.specification/initial-state
                   :stateful-check.specification/setup]))

(s/def :stateful-check/options (s/nilable map?))

;; Evaluation

(s/def :stateful-check.evaluation/result any?)

(s/def :stateful-check.evaluation/result-str
  ;; TODO: Should this always be a string?
  (s/or :string string? :exception #(instance? CaughtException %)))

(s/def :stateful-check.evaluation/command
  (s/cat :handle :stateful-check.symbolic-value/root
         :command :stateful-check/command
         :args (s/* any?)))

(s/def :stateful-check/evaluation
  (s/tuple :stateful-check.evaluation/command
           :stateful-check.evaluation/result-str
           :stateful-check.evaluation/result))

(s/def :stateful-check.evaluation/sequential
  (s/coll-of :stateful-check/evaluation :kind vector?))

(s/def :stateful-check.evaluation/parallel
  (s/coll-of :stateful-check.evaluation/sequential :kind vector?))

;; Run (aka Test Check Report)

(s/def :stateful-check.run/depth int?)
(s/def :stateful-check.run/fail (s/coll-of any? :kind vector?))
(s/def :stateful-check.run/failed-after-ms nat-int?)
(s/def :stateful-check.run/failing-size int?)
(s/def :stateful-check.run/num-tests nat-int?)
(s/def :stateful-check.run/pass? boolean?)
(s/def :stateful-check.run/result (s/or :status boolean? :error #(instance? Throwable %)))
(s/def :stateful-check.run/seed int?)
(s/def :stateful-check.run/smallest (s/coll-of any? :kind vector?))
(s/def :stateful-check.run/time-elapsed-ms nat-int?)
(s/def :stateful-check.run/time-shrinking-ms nat-int?)
(s/def :stateful-check.run/total-nodes-visited nat-int?)

(s/def :stateful-check.run/result-data
  (s/keys :req-un [:stateful-check/specification
                   :stateful-check/options
                   :stateful-check.evaluation/sequential
                   :stateful-check.evaluation/parallel]))

(s/def :stateful-check.run/shrunk
  (s/keys :req-un [:stateful-check.run/depth
                   :stateful-check.run/pass?
                   :stateful-check.run/result
                   :stateful-check.run/result-data
                   :stateful-check.run/smallest
                   :stateful-check.run/time-shrinking-ms
                   :stateful-check.run/total-nodes-visited]))

(s/def :stateful-check/run
  (s/keys :req-un [:stateful-check.run/num-tests
                   :stateful-check.run/pass?
                   :stateful-check.run/seed]
          :opt-un [:stateful-check.run/shrunk
                   :stateful-check.run/fail
                   :stateful-check.run/failed-after-ms
                   :stateful-check.run/failing-size
                   :stateful-check.run/result
                   :stateful-check.run/result-data
                   :stateful-check.run/time-elapsed-ms]))
