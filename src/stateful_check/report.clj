(ns stateful-check.report
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.test :as t]
            [clojure.test.check.clojure-test :as clojure-test]
            [clojure.test.check.properties]
            [stateful-check.generator :as g]
            [stateful-check.runner :as r])
  (:import (stateful_check.runner CaughtException)))

(defn- exeptional-events
  "Return the exceptional events of the Stateful Check `results`."
  [{:keys [result-data]}]
  (->> result-data :messages vals
       (mapcat identity)
       (mapcat :events)
       (filter #(instance? Throwable (:actual %)))))

(defn- print-frequencies
  "Print the Stateful Check command execution frequencies."
  [frequencies]
  (print "Command execution counts:")
  (pp/print-table (->> frequencies
                       (sort-by val)
                       reverse ;; big numbers on top
                       (map #(hash-map :command (key %)
                                       :count (val %))))))

(defn- print-error
  "Print the `error` according to `options`."
  [error options]
  (if (get-in options [:report :stacktrace?] false)
    (.printStackTrace ^Throwable error (java.io.PrintWriter. *out*))
    (println (.toString ^Object error))))

(defn- print-messages
  "Print a Clojure Test assertion errors."
  [handle messages options]
  (doseq [{:keys [message events]} (get messages handle)]
    (if-not (seq events)
      (println "   " message)
      (doseq [{:keys [message] :as event} events]
        (when message
          (println "     " message))
        (doseq [detail [:expected :actual]]
          (when (contains? event detail)
            (->> (str/split (with-out-str
                              (let [value (get event detail)]
                                (if (instance? Throwable value)
                                  (print-error value options)
                                  (pp/pprint value))))
                            #"\n")
                 (remove str/blank?)
                 (str/join "\n             ")
                 (str (format "%12s: " (name detail)))
                 (println))))))))

(defn- print-sequence
  "Print a Stateful Check command execution."
  [commands messages options]
  (doseq [[[handle cmd & args] trace] commands]
    (printf "  %s = %s %s\n"
            (pr-str handle)
            (cons (:name cmd)
                  args)
            (if (= ::r/unevaluated trace)
              ""
              (str " = "
                   (if (instance? CaughtException trace)
                     (print-error (:exception trace) options)
                     trace))))
    (print-messages handle messages options)))

(defn print-execution
  "Print sequential and parallel Stateful Check command executions."
  ([{:keys [sequential parallel messages options]}]
   (print-execution sequential parallel messages options))
  ([sequential parallel messages options]
   (printf "Sequential prefix:\n")
   (print-sequence sequential messages options)
   (doseq [[i thread] (map vector (range) parallel)]
     (printf "\nThread %s:\n" (g/index->letter i))
     (print-sequence thread messages options))
   (print-messages nil messages options)))

(defn- print-first-failing-case
  "Print the first failing case of the Stateful Check results."
  [{:keys [result-data]}]
  (let [options (:options result-data)]
    (when (get-in options [:report :first-case?] false)
      (println "  First failing test case")
      (println "  -----------------------------")
      (print-execution result-data))))

(defn- print-smallest-failing-case
  "Print the smallest failing case of the Stateful Check results."
  [{:keys [result-data]}]
  (let [options (:options result-data)]
    (println "  Smallest case after shrinking")
    (println "  -----------------------------")
    (print-execution result-data)))

(defn- print-summary
  "Print the summary of the Stateful Check results."
  [{:keys [result-data] :as results}]
  (let [options (:options result-data)]
    (println "Seed:" (:seed results))
    (when (> (get-in options [:gen :threads] 0) 1)
      (println (str "  Note: Test cases with multiple threads are not deterministic, so using the\n"
                    "        same seed does not guarantee the same result.")))))

(defn print-results
  "Print the summary of the Stateful Check results."
  ([results]
   (print-results nil results))
  ([msg results]
   (let [error (-> results :result-data :clojure.test.check.properties/error)
         options (-> results :result-data :options)]
     (when msg
       (println msg))
     (when (instance? Throwable error)
       (println (.getMessage ^Throwable error)))
     (when (-> results :result-data :sequential seq)
       (when (get-in options [:report :first-case?] false)
         (print-first-failing-case results)
         (println))
       (print-smallest-failing-case (:shrunk results))
       (println)
       (print-summary results)))))

(defn default-reporter-fn
  "Default function passed as the :reporter-fn to clojure.test.check/quick-check.

  Delegates to clojure.test.check.clojure-test/report, except
  for :shrunk and :trial events, since those tend to be quite verbose
  and easily flood standard output."
  [{:keys [type result-data] :as m}]
  (cond
    (= :complete type)
    (clojure-test/default-reporter-fn m)
    (= :trial type)
    (clojure-test/default-reporter-fn m)))

(defn- report-pass?
  "Return true if the Stateful Check `results` indicate a passing test."
  [results]
  (:pass? results))

(defn- report-error?
  "Return true if the Stateful Check `results` indicate an error-ed test."
  [results]
  (and (not (:pass? results))
       (or (-> results :result-data :clojure.test.check.properties/error)
           (seq (exeptional-events results)))))

(defn- report-fail?
  "Return true if the Stateful Check `results` indicate a failing test."
  [results]
  (not (:pass? results)))

(defn- error-event
  "Return the report for a test that threw an exception."
  [msg results]
  (let [error (-> results :result-data :clojure.test.check.properties/error)
        error-event (first (exeptional-events results))]
    {:type :error
     :message (with-out-str
                (binding [*out* (java.io.PrintWriter. *out*)]
                  (print-results msg results)))
     :expected (:expected error-event)
     :actual (or error (:actual error-event))
     :fault true
     :stateful-check results}))

(defn- fail-event
  "Return the report for a failing test."
  [msg results]
  {:type :fail
   :message (with-out-str
              (binding [*out* (java.io.PrintWriter. *out*)]
                (print-results msg results)))
   :expected (symbol "all executions to match specification"),
   :actual (symbol "the above execution did not match the specification")
   :stateful-check results})

(defn- pass-event
  "Return the report for a passing test."
  [msg results]
  {:type :pass
   :message msg
   :expected true
   :actual true
   :stateful-check results})

(defn report-result
  "Report the Stateful Check test assertion."
  [msg results frequencies]
  (let [options (-> results :result-data :options)]
    (when (get-in options [:report :command-frequency?] false)
      (print-frequencies frequencies))
    (cond (report-pass? results)
          (t/do-report (pass-event msg results))
          (report-error? results)
          (t/do-report (error-event msg results))
          (report-fail? results)
          (t/do-report (fail-event msg results)))
    (:pass? results)))
