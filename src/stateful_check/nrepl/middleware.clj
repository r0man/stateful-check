(ns stateful-check.nrepl.middleware
  (:require [cider.nrepl.middleware.inspect :as middleware.inspect]
            [cider.nrepl.middleware.test :refer [current-report]]
            [cider.nrepl.middleware.util :refer [transform-value]]
            [cider.nrepl.middleware.util.error-handling :as error-handling]
            [haystack.analyzer :as haystack.analyzer]
            [nrepl.middleware :as middleware]
            [nrepl.middleware.interruptible-eval :as ie]
            [nrepl.middleware.print :as print]
            [nrepl.middleware.session :as session]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as t]
            [orchard.inspect :as inspect]
            [stateful-check.core :as stateful-check]
            [stateful-check.debugger.core :as debugger]
            [stateful-check.debugger.render :as render]
            [stateful-check.generator :as g])
  (:import [java.io StringWriter]
           [java.util UUID]))

(defn- criteria
  "Make the search criteria map from the NREPL msg."
  [{:keys [analysis ns var]}]
  (cond-> {}
    (string? analysis)
    (assoc :id (UUID/fromString analysis))
    (or (string? ns) (symbol? ns))
    (assoc :ns (symbol (name ns)))
    (or (string? var) (symbol? var))
    (assoc :var (symbol (name var)))))

(defn- make-debugger
  "Make a new debugger."
  []
  (debugger/debugger
   {:render (fn [value]
              (binding [inspect/*max-atom-length* 50]
                (inspect/inspect-value value)))
    :test {:report current-report}}))

(defn- debugger
  "Return the debugger from `msg` or a new one."
  [msg]
  (or (-> msg :session meta ::debugger) (make-debugger)))

(defn- swap-debugger!
  "Apply `f` with `args` to the debugger of the NREPL `session`."
  [{:keys [session]} f & args]
  (-> session
      (alter-meta! update ::debugger #(apply f (or % (make-debugger)) args))
      (get ::debugger)))

(defn- stateful-check-analyze-test-reply
  "Handle a Stateful Check test analysis NREPL operation."
  [{:keys [test] :as msg}]
  (if-let [event (debugger/find-test-event (debugger msg) test)]
    {:stateful-check/analyze-test
     (-> (swap-debugger! msg debugger/analyze-test-run event)
         (debugger/last-run)
         (render/render-run)
         (transform-value))}
    {:status :stateful-check/test-not-found}))

(defn- parse-query [query]
  (cond-> query
    (string? (:analysis query))
    (update :analysis #(UUID/fromString %))))

(defn- parse-gen-options
  "Parse the Stateful Check generation options."
  [{:keys [threads max-length max-size]}]
  {:max-length (if (nat-int? max-length) max-length g/default-max-length)
   :max-size (if (nat-int? max-size) max-size g/default-max-size)
   :shrink-strategies g/default-shrink-strategies
   :threads (if (nat-int? threads) threads g/default-threads)})

(defn- parse-run-options
  "Parse the Stateful Check run options."
  [{:keys [assume-immutable-results max-tries num-tests seed timeout-ms]}]
  {:assume-immutable-results (= "true" (str assume-immutable-results))
   :max-tries (if (nat-int? max-tries) max-tries stateful-check/default-max-tries)
   :num-tests (if (nat-int? num-tests) num-tests stateful-check/default-num-tests)
   :seed (if (int? seed) seed (System/currentTimeMillis))
   :timeout-ms (if (nat-int? timeout-ms) timeout-ms stateful-check/default-timeout-ms)})

(defn- parse-report-options
  "Parse the Stateful Check report options."
  [{:keys [command-frequency? first-case? stacktrace?]}]
  {:command-frequency? (= "true" (str command-frequency?))
   :first-case? (= "true" (str first-case?))
   :stacktrace? (= "true" (str stacktrace?))})

(defn- parse-options
  "Parse the Stateful Check options."
  [{:keys [gen run report]}]
  {:gen (parse-gen-options gen)
   :run (parse-run-options run)
   :report (parse-report-options report)})

(defn- stateful-check-inspect-reply
  "Handle a Stateful Check inspect NREPL operation."
  [msg]
  (let [query (parse-query (:query msg))]
    (if-let [object (debugger/get-object (debugger msg) query)]
      (let [inspector (inspect/start (inspect/fresh) object)]
        (#'middleware.inspect/inspector-response
         msg (middleware.inspect/swap-inspector! msg (constantly inspector))))
      {:status :stateful-check/object-not-found
       :query (transform-value query)})))

(defn- stateful-check-run-reply
  "Handle the Stateful Check specification run NREPL operation."
  [{:keys [id session specification transport options] :as msg}]
  (let [{:keys [exec]} (meta session)]
    (exec id
          (fn []
            (with-bindings (assoc @session #'ie/*msg* msg)
              (if-let [specification (debugger/specification (debugger msg) specification)]
                (t/send transport (response-for msg :stateful-check/run
                                                (-> (swap-debugger! msg debugger/run-specification (:id specification) (parse-options options))
                                                    (debugger/last-run)
                                                    (render/render-run)
                                                    (transform-value))))
                (t/send transport (response-for msg :status :stateful-check/specification-not-found)))))
          (fn []
            (t/send transport (response-for msg :status :done))))))

(defn- stateful-check-scan-reply
  "Scan public vars and test reports for Stateful Check specifications."
  [msg]
  {:stateful-check/scan
   (->> (swap-debugger! msg debugger/scan)
        (debugger/specifications)
        (transform-value))})

(defn- stateful-check-analysis-reply
  "Handle a Stateful Check analysis NREPL operation."
  [{:keys [analysis] :as msg}]
  (let [id (UUID/fromString analysis)]
    (if-let [analysis (debugger/get-run (debugger msg) id)]
      {:stateful-check/analysis (render/render-run analysis)}
      {:status :stateful-check/analysis-not-found})))

(defn- stateful-check-eval-step-reply
  "Evaluate a command execution step."
  [{:keys [id session case run transport] :as msg}]
  (let [{:keys [exec]} (meta session)]
    (exec id
          (fn []
            (with-bindings (assoc @session #'ie/*msg* msg)
              ;; TODO: Using *out* and *err* from the session bindings hangs the
              ;; REPL as soon as something is printed. Find a better way to do
              ;; this.
              (binding [*out* (java.io.StringWriter.)
                        *err* (java.io.StringWriter.)]
                (try
                  (let [result (-> (swap-debugger! msg debugger/eval-step run case)
                                   (debugger/get-run run)
                                   (render/render-run)
                                   (transform-value))
                        err (str *err*)
                        out (str *out*)]
                    (when (pos? (count out))
                      (t/send transport (response-for msg :out out)))
                    (when (pos? (count err))
                      (t/send transport (response-for msg :err err)))
                    (t/send transport (response-for msg :stateful-check/eval-step result)))
                  (catch Throwable e
                    (.printStackTrace e))))))
          (fn []
            (t/send transport (response-for msg :status :done))))))

(defn- stateful-check-eval-stop-reply
  "Stop the evaluation of a test run."
  [{:keys [id session case run transport] :as msg}]
  (let [{:keys [exec]} (meta session)]
    (exec id
          (fn []
            (with-bindings (assoc @session #'ie/*msg* msg)
              ;; TODO: Using *out* and *err* from the session bindings hangs the
              ;; REPL as soon as something is printed. Find a better way to do
              ;; this.
              (binding [*out* (java.io.StringWriter.)
                        *err* (java.io.StringWriter.)]
                (try
                  (let [result (-> (swap-debugger! msg debugger/eval-stop run case)
                                   (debugger/get-run run)
                                   (render/render-run)
                                   (transform-value))
                        err (str *err*)
                        out (str *out*)]
                    (when (pos? (count out))
                      (t/send transport (response-for msg :out out)))
                    (when (pos? (count err))
                      (t/send transport (response-for msg :err err)))
                    (t/send transport (response-for msg :stateful-check/eval-stop result)))
                  (catch Throwable e
                    (.printStackTrace e))))))
          (fn []
            (t/send transport (response-for msg :status :done))))))

(defn- stateful-check-print-reply
  "Handle a Stateful Check print NREPL operation."
  [{:keys [query ::print/print-fn] :as msg}]
  (if-let [object (debugger/get-object (debugger msg) (parse-query query))]
    (let [writer (StringWriter.)]
      (print-fn object writer)
      {:stateful-check/print (str writer)})
    {:status :stateful-check/object-not-found
     :query (transform-value query)}))

(defn- stateful-check-specifications-reply
  "List all Stateful Check specifications from loaded namespaces."
  [msg]
  {:stateful-check/specifications
   (-> (debugger msg)
       (debugger/specifications)
       (transform-value))})

(defn- stateful-check-stacktrace-reply
  "Handle a Stateful Check stacktrace NREPL operation."
  [{:keys [query transport ::print/print-fn] :as msg}]
  (if-let [exception (debugger/get-error (debugger msg) (parse-query query))]
    (do (doseq [cause (haystack.analyzer/analyze exception print-fn)]
          (t/send transport (response-for msg cause)))
        (t/send transport (response-for msg :status :done)))
    (t/send transport (response-for msg :status :stateful-check/no-error))))

(defn- handle-message
  "Handle a Stateful Check NREPL `msg`."
  [handler msg]
  (case (:op msg)
    "stateful-check/eval-step" (stateful-check-eval-step-reply msg)
    "stateful-check/eval-stop" (stateful-check-eval-stop-reply msg)
    "stateful-check/run" (stateful-check-run-reply msg)
    (error-handling/with-safe-transport handler msg
      "stateful-check/analysis" stateful-check-analysis-reply
      "stateful-check/analyze-test" stateful-check-analyze-test-reply
      "stateful-check/inspect" stateful-check-inspect-reply
      "stateful-check/print" stateful-check-print-reply
      "stateful-check/scan" stateful-check-scan-reply
      "stateful-check/specifications" stateful-check-specifications-reply
      "stateful-check/stacktrace" stateful-check-stacktrace-reply)))

(def ^:private descriptor
  {:doc "Stateful Check debugger"
   :requires #{#'print/wrap-print #'session/session}
   :handles
   {"stateful-check/analysis"
    {:doc "Return a Stateful Check analysis for a test."
     :requires {"test" "The identifier of the test."}
     :returns {"stateful-check/analysis" "The Stateful Check analysis."
               "status" "done"}}

    "stateful-check/analyze-test"
    {:doc "Return the Stateful Check report of a test."
     :requires {"test" "The identifier of the Stateful Check test."}
     :returns {"stateful-check/analyze-test" "The analysis of the test results."
               "status" "done"}}

    "stateful-check/eval-step"
    {:doc "Evaluate a command of a Stateful Check run."
     :requires {"run" "The identifier of the run."}
     :optional (merge print/wrap-print-optional-arguments
                      {"case" "The \"first\" or \"smallest\" failing case."
                       "run" "The identifier of the Stateful Check run."})
     :returns {"stateful-check/evaluate-step" "The Stateful Check run."
               "status" "done"}}

    "stateful-check/eval-stop"
    {:doc "Stop the evaluation of a Stateful Check run."
     :requires {"run" "The identifier of the run."}
     :optional (merge print/wrap-print-optional-arguments
                      {"case" "The \"first\" or \"smallest\" failing case."
                       "run" "The identifier of the Stateful Check run."})
     :returns {"stateful-check/evaluate-stop" "The Stateful Check run."
               "status" "done"}}

    "stateful-check/inspect"
    {:doc "Inspect an object of a Stateful Check run."
     :requires {"query" "The query for the object to inspect."}
     :returns {"value" "The inspected object."
               "status" "done"}}

    "stateful-check/print"
    {:doc "Print an object of a Stateful Check run."
     :requires {"query" "The query for the object to print."}
     :optional print/wrap-print-optional-arguments
     :returns {"value" "The printed object."
               "status" "done"}}

    "stateful-check/run"
    {:doc "Run a Stateful Check specification."
     :requires {"specification" "The id of the Stateful Check specification."}
     :optional (merge print/wrap-print-optional-arguments
                      {"options" "The Stateful Check options used to run the specification."})
     :returns {"stateful-check/run" "The analysis of the test run."
               "status" "done"}}

    "stateful-check/scan"
    {:doc "Scan public vars and test reports for Stateful Check specifications."
     :returns {"stateful-check/scan" "The list of Stateful Check specifications."
               "status" "done"}}

    "stateful-check/specifications"
    {:doc "List all Stateful Check specifications from loaded namespaces."
     :returns {"stateful-check/specifications" "The list of Stateful Check specifications."
               "status" "done"}}

    "stateful-check/stacktrace"
    {:doc "Return messages describing each cause and stack frame of the exception at query."
     :requires {"query" "The query for the exception to inspect."}
     :optional print/wrap-print-optional-arguments
     :returns {"status" "\"done\", or \"stateful-check/no-error\" no error was found."}}}})

(defn wrap-stateful-check [handler]
  (fn [msg] (handle-message handler msg)))

(middleware/set-descriptor! #'wrap-stateful-check descriptor)
