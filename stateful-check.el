;;; stateful-check.el --- Stateful Check Debugger -*- lexical-binding: t -*-

;; Copyright © 2023 r0man

;; Author: r0man <roman@burningswell.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Stateful Check Debugger

;;; Commentary:

;; This major mode provides a debugger for Stateful Check specifications.  The
;; failing test cases of a Stateful Check run are rendered in an interactive
;; buffer.  Objects in that buffer, such as the arguments a command was invoked
;; with, the result of invoking the command and the execution state can be
;; viewed in the Cider Inspector.  The debugger also provides functionality to
;; step through the commands of a failing test case.

;;; Usage:

;; Run M-x stateful-check to open the Stateful Check transient menu.

;;; Code:

(require 'ansi-color)
(require 'button)
(require 'cider-client)
(require 'cider-common)
(require 'cider-inspector)
(require 'cider-mode)
(require 'cider-overlays)
(require 'cider-popup)
(require 'cider-stacktrace)
(require 'cider-test)
(require 'cl-lib)
(require 'easymenu)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defcustom stateful-check-buffer "*stateful-check*"
  "The name of the Stateful Check buffer."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'stringp
  :type 'string)

(defcustom stateful-check-auto-select-buffer t
  "Determines if the debugger buffer should be auto selected."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom stateful-check-render-options t
  "Whether to render options in the debugger buffer."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom stateful-check-thread-name-index "abcdefghijklmnopqrstuvwxzy"
  "The index used to pick thread names."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'stringp
  :type 'string)

(defcustom stateful-check-gen-max-length 5
  "Specifies a max length for command sequences."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom stateful-check-gen-max-size 200
  "Specifies a maximum size for generated values."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom stateful-check-gen-threads 0
  "Specifies how many parallel threads to execute."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom stateful-check-run-assume-immutable-results-p nil
  "Specifies whether the runner should assume that the results of running commands are immutable."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom stateful-check-run-max-tries 1
  "Specifies how attempts to make to fail a test."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom stateful-check-run-num-tests 200
  "Specifies how many tests to run."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom stateful-check-run-seed nil
  "Specifies the initial seed to use for generation."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom stateful-check-run-timeout-ms 0
  "Specifies the maximum number of milliseconds that a test is permitted to run for."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'integerp
  :type 'integer)

(defcustom stateful-check-report-first-case-p t
  "Specifies whether to print the first failure."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom stateful-check-report-command-frequency-p nil
  "Specifies whether to print information about how often each command was run."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'booleanp
  :type 'boolean)

(defcustom stateful-check-render-execution 'stateful-check--render-execution-long
  "Specifies a function that renders a command execution."
  :group 'stateful-check
  :package-version '(cider . "0.27.0")
  :safe #'symbolp
  :type 'symbol)

(defvar-local stateful-check--current-run nil
  "The Stateful Check run of the current buffer.")

;; Stateful Check options

(defun stateful-check-gen-options ()
  "Return the Stateful Check generation options."
  (nrepl-dict "max-length" stateful-check-gen-max-length
              "max-size" stateful-check-gen-max-size
              "threads" stateful-check-gen-threads))

(defun stateful-check-run-options ()
  "Return the Stateful Check run options."
  (nrepl-dict "assume-immutable-results"
              (if stateful-check-run-assume-immutable-results-p "true" "false")
              "max-tries" stateful-check-run-max-tries
              "num-tests" stateful-check-run-num-tests
              "seed" stateful-check-run-seed
              "timeout-ms" stateful-check-run-timeout-ms))

(defun stateful-check-report-options ()
  "Return the Stateful Check report options."
  (nrepl-dict "first-case?"
              (if stateful-check-report-first-case-p "true" "false")
              "command-frequency?"
              (if stateful-check-report-command-frequency-p "true" "false")))

(defun stateful-check-options ()
  "Return the Stateful Check options."
  (nrepl-dict "gen" (stateful-check-gen-options)
              "report" (stateful-check-report-options)
              "run" (stateful-check-run-options)))

;; NREPL operations

(defun cider-sync-request:stateful-check-analysis (id)
  "Return Stateful Check analysis by ID."
  (cider-ensure-op-supported "stateful-check/analysis")
  (thread-first `("op" "stateful-check/analysis"
                  "id" ,id)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/analysis")))

(defun cider-sync-request:stateful-check-analyze-test (test)
  "Analyze Stateful Check TEST report."
  (cider-ensure-op-supported "stateful-check/analyze-test")
  (thread-first `("op" "stateful-check/analyze-test"
                  "test" ,test)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/analyze-test")))

(defun cider-sync-request:stateful-check-inspect (query)
  "Inspect the Stateful Check test run object for the QUERY."
  (cider-ensure-op-supported "stateful-check/inspect")
  (thread-first `("op" "stateful-check/inspect"
                  "query" ,query)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "value")))

(defun cider-sync-request:stateful-check-print (index)
  "Print the Stateful Check test run object at INDEX."
  (cider-ensure-op-supported "stateful-check/print")
  (thread-first `("op" "stateful-check/print"
                  "index" ,index)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/print")))

(defun cider-sync-request:stateful-check-scan ()
  "Scan public vars and test reports for Stateful Check specifications."
  (cider-ensure-op-supported "stateful-check/scan")
  (thread-first `("op" "stateful-check/scan")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/scan")))

(defun cider-sync-request:stateful-check-specifications ()
  "List all known Stateful Check specifications."
  (cider-ensure-op-supported "stateful-check/specifications")
  (thread-first `("op" "stateful-check/specifications")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "stateful-check/specifications")))

(defun cider-request:stateful-check-eval-step (run case callback)
  "Evaluate the current command for the failing CASE of RUN."
  (cider-ensure-op-supported "stateful-check/eval-step")
  (thread-first `("op" "stateful-check/eval-step"
                  "run" ,run
                  "case", (or case "smallest"))
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-eval-stop (run case callback)
  "Stop the evaluation of the failing CASE of RUN."
  (cider-ensure-op-supported "stateful-check/eval-stop")
  (thread-first `("op" "stateful-check/eval-stop"
                  "run" ,run
                  "case", (or case "smallest"))
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-stacktrace (query callback)
  "Request the stacktrace of Stateful Check run matching QUERY and invoke CALLBACK."
  (cider-ensure-op-supported "stateful-check/stacktrace")
  (thread-first `("op" "stateful-check/stacktrace"
                  "query" ,query)
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-test (test-report callback)
  "Run Stateful Check TEST-REPORT and invoke CALLBACK."
  (thread-first `("op" "test-var-query"
                  "ns" ,(stateful-check--run-ns test-report)
                  "tests" ,(list (stateful-check--run-var test-report)))
                (cider-nrepl-send-request callback)))

(defun cider-request:stateful-check-run (specification options callback)
  "Run the Stateful Check SPECIFICATION using OPTIONS and invoke CALLBACK."
  (cider-ensure-op-supported "stateful-check/run")
  (thread-first `("op" "stateful-check/run"
                  "specification" ,(stateful-check--specification-id specification)
                  "options" ,options)
                (cider-nrepl-send-request callback)))

;; Misc

(defun stateful-check--delete-property-region (property)
  "Delete region of at which PROPERTY changes, relative to the current point."
  (let ((start (previous-single-property-change (point) property))
        (end (next-single-property-change (point) property)))
    (when (and start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (list start end)))))

(defun stateful-check--strip-handle (handle)
  "Strip the Stateful Check HANDLE of its angle brackets."
  (when handle (string-trim (string-trim handle "#") "<" ">")))

(defun stateful-check--thread-name (index)
  "Return the thread name from `stateful-check-thread-name-index' for INDEX."
  (char-to-string (elt stateful-check-thread-name-index index)))

(defun stateful-check--run-id-at-point ()
  "Return the Stateful Check test run id at point, or nil."
  (when-let ((ns (get-text-property (point) 'ns))
             (var (get-text-property (point) 'var)))
    (format "%s/%s" ns var)))

;; Specification

(defun stateful-check--specification-id (specification)
  "Return the id of the SPECIFICATION."
  (nrepl-dict-get specification "id"))

;; Run

(defun stateful-check--run-specification (run)
  "Return the specification of the Stateful Check RUN."
  (nrepl-dict-get-in run '("specification")))

(defun stateful-check--run-options (run)
  "Return the options used for the Stateful Check RUN."
  (nrepl-dict-get-in run '("options")))

(defun stateful-check--run-pass-p (run)
  "Return non-nil if Stateful Check RUN passed, otherwise nil."
  (equal "true" (nrepl-dict-get run "pass?")))

(defun stateful-check--run-replace (run)
  "Replace the Stateful Check run at point with RUN."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos))
        (column (current-column)))
    (when (stateful-check--delete-property-region 'stateful-check-run)
      ;; Render into a temp buffer first, to avoid issues with `insert-rectangle'
      ;; messing up text after the replaced region.
      (insert (with-temp-buffer
                (stateful-check--insert-run run)
                (buffer-string)))
      (goto-char (point-min))
      (forward-line (- line 1))
      (move-to-column column))))

;; Run

(defun stateful-check--run-ns (run)
  "Return the namespace of the RUN."
  (nrepl-dict-get-in run '("specification" "ns")))

(defun stateful-check--run-var (run)
  "Return the namespace of the RUN."
  (nrepl-dict-get-in run '("specification" "var")))

(defun stateful-check--run-first-case (run)
  "Return the first failing case from the Stateful Check RUN."
  (nrepl-dict-get-in run '("result-data")))

(defun stateful-check--run-smallest-case (run)
  "Return the smallest failing case from the Stateful Check RUN."
  (nrepl-dict-get-in run '("shrunk" "result-data")))

(defun stateful-check--run-sort (specifications)
  "Sort the Stateful Check SPECIFICATIONS by their id slot."
  (seq-sort-by #'stateful-check--specification-id #'string< specifications))

(defun stateful-check--read-specification-id (specifications)
  "Read one of the Stateful Check specification name from SPECIFICATIONS, with completion."
  (completing-read "Stateful Check Specification: "
                   (seq-map #'stateful-check--specification-id
                            (stateful-check--run-sort specifications))))

(defun stateful-check--read-specification (specifications)
  "Read one of the Stateful Check SPECIFICATIONS, with completion."
  (let ((id (stateful-check--read-specification-id specifications)))
    (seq-find (lambda (specification)
                (equal id (stateful-check--specification-id specification)))
              specifications)))

;; Render

(defun stateful-check--render-handle (failing-case execution)
  "Render the Stateful Check EXECUTION handle of the FAILING-CASE."
  (nrepl-dbind-response execution (handle result)
    (nrepl-dbind-response result (evaluation)
      (let ((eval-p (stateful-check--failing-case-eval-p failing-case))
            (states (stateful-check--eval-states failing-case))
            (state (stateful-check--strip-handle handle)))
        (cond ((and eval-p (member state states))
               (cider-insert handle 'bold))
              ((and eval-p evaluation)
               (insert handle))
              ((and eval-p (not evaluation))
               (cider-insert handle 'font-lock-comment-face))
              (t (insert handle)))))))

(defun stateful-check--render-argument (argument)
  "Render the Stateful Check command ARGUMENT."
  (when (nrepl-dict-p argument)
    (nrepl-dbind-response argument (index symbolic)
      (cider-propertize-region
          (list 'stateful-check-argument argument
                'cider-value-idx index
                'mouse-face 'highlight)
        (insert (cider-font-lock-as-clojure symbolic))))))

(defun stateful-check--render-failure-event (failing-case event)
  "Render the test assertion EVENT of FAILING-CASE into the current buffer."
  (nrepl-dbind-response event (context message expected actual diffs error gen-input)
    (cl-flet ((insert-label (s)
                (cider-insert (format "%14s: " s) 'font-lock-comment-face))
              (insert-align-label (s)
                (insert (format "%18s" s)))
              (insert-rect (s)
                (cl-letf (((symbol-function 'message) (lambda (_ &rest _))))
                  (let ((start (point)))
                    (insert-rectangle (thread-first
                                        s
                                        cider-font-lock-as-clojure
                                        (split-string "\n")))
                    (ansi-color-apply-on-region start (point)))
                  (beginning-of-line))))
      (cider-propertize-region (list 'stateful-check-test-event event)
        (let ((beg (point))
              (bg `(:background ,cider-test-items-background-color :extend t)))
          (when context  (cider-insert context 'font-lock-doc-face t))
          (when message  (cider-insert message 'font-lock-string-face t))
          (when expected
            (insert-label "expected")
            (insert-rect expected))
          (if diffs
              (dolist (d diffs)
                (cl-destructuring-bind (actual (removed added)) d
                  (insert-label "actual")
                  (insert-rect actual)
                  (insert-label "diff")
                  (insert "- ")
                  (insert-rect removed)
                  (insert-align-label "+ ")
                  (insert-rect added)))
            (when actual
              (insert-label "actual")
              (insert-rect actual)))
          (when error
            (insert-label "error")
            (cider-propertize-region
                (list 'cider-value-idx error
                      'mouse-face 'highlight)
              (stateful-check--render-error
               failing-case (nrepl-dict "error" (nrepl-dict "real" error))))
            (insert "\n"))
          (when gen-input
            (insert-label "input")
            (insert (cider-font-lock-as-clojure gen-input)))
          (overlay-put (make-overlay beg (point)) 'font-lock-face bg))))))

(defun stateful-check--render-failure (failing-case failure)
  "Render the Stateful Check post-condition FAILURE."
  (when (nrepl-dict-p failure)
    (nrepl-dbind-response failure (events message)
      (cider-propertize-region (list 'stateful-check-failure failure)
        (if (zerop (length events))
            (when message
              (insert (format "      %s\n" message)))
          (seq-doseq (event events)
            (stateful-check--render-failure-event failing-case event)))))))

(defun stateful-check--render-failures (failing-case failures)
  "Render the evaluation FAILURES of the FAILING-CASE."
  (nrepl-dbind-response failures (evaluation real)
    (let ((eval-p (stateful-check--failing-case-eval-p failing-case))
          (start (point)))
      (cond
       ((and evaluation)
        (seq-doseq (failure evaluation)
          (stateful-check--render-failure failing-case failure)))
       ((and eval-p real)
        (seq-doseq (failure real)
          (stateful-check--render-failure failing-case failure))
        (add-text-properties start (point) (list 'face 'font-lock-comment-face)))
       ((and (not eval-p) real)
        (seq-doseq (failure real)
          (stateful-check--render-failure failing-case failure)))))))

(defun stateful-check--show-error ()
  "Show the error at point in the stacktrace navigator."
  (when-let (query (stateful-check--query-at-point))
    (let (causes)
      (cider-request:stateful-check-stacktrace
       query (lambda (response)
               (nrepl-dbind-response response (class status)
                 (cond (class  (setq causes (cons response causes)))
                       (status (when causes
                                 (cider-stacktrace-render
                                  (cider-popup-buffer cider-error-buffer
                                                      cider-auto-select-error-buffer
                                                      #'cider-stacktrace-mode
                                                      'ancillary)
                                  (reverse causes)))))))))))

(defun stateful-check--failing-case-assume-immutable-results-p (failing-case)
  "Return non-nil if FAILING-CASE was run with the assume-immutable-results option."
  (equal "true" (nrepl-dict-get-in failing-case '("options" "run" "assume-immutable-results"))))

(defun stateful-check--render-result (failing-case execution)
  "Render the EXECUTION result of a Stateful Check FAILING-CASE."
  (let ((eval-p (stateful-check--failing-case-eval-p failing-case))
        (immutable-results-p (stateful-check--failing-case-assume-immutable-results-p failing-case)))
    (nrepl-dbind-response execution (result)
      (nrepl-dbind-response result (evaluation real real-str real-mutated? real-mutated)
        (cider-propertize-region
            (list 'stateful-check-result result
                  'cider-value-idx result
                  'mouse-face 'highlight)
          (cond
           ((and (not immutable-results-p) real-mutated)
            (insert (cider-font-lock-as-clojure real-str))
            (insert "\n")
            (cider-insert
             (format "      >> object may have been mutated later into %s <<"
                     real-mutated)
             'font-lock-type-face))
           ((and real-mutated? real-str)
            (insert (cider-font-lock-as-clojure real-str)))
           ((and eval-p evaluation)
            (insert (cider-font-lock-as-clojure evaluation)))
           ((and eval-p real)
            (insert (cider-propertize real 'font-lock-comment-face)))
           (real (insert (cider-font-lock-as-clojure real)))))))))

(defun stateful-check--render-error-button (exception)
  "Render the EXCEPTION as a text button."
  (insert-text-button exception
                      'follow-link t
                      'action '(lambda (_button) (stateful-check--show-error))
                      'help-echo "View causes and stacktrace"))

(defun stateful-check--render-error (failing-case execution)
  "Render the EXECUTION error of a Stateful Check FAILING-CASE."
  (nrepl-dbind-response execution (error result)
    (nrepl-dbind-response error (evaluation real)
      (let ((eval-p (stateful-check--failing-case-eval-p failing-case))
            (eval-result-p (and result (nrepl-dict-contains result "evaluation"))))
        (cider-propertize-region
            (list 'stateful-check-error error
                  'cider-value-idx error
                  'mouse-face 'highlight)
          (cond ((and eval-p evaluation)
                 (stateful-check--render-error-button evaluation))
                ((and eval-p (not eval-result-p) real)
                 (stateful-check--render-error-button
                  (cider-propertize real 'font-lock-comment-face)))
                ((and (not eval-result-p) real)
                 (stateful-check--render-error-button real))))))))

(defun stateful-check--render-execution-short (failing-case execution)
  "Render the Stateful Check EXECUTION of the FAILING-CASE in short form."
  (nrepl-dbind-response execution (arguments command failures)
    (cider-propertize-region (list 'stateful-check-execution execution)
      (insert "    ")
      (stateful-check--render-handle failing-case execution)
      (insert (cider-propertize " = " 'font-lock-comment-face)
              (cider-propertize "(" 'paren-face))
      (nrepl-dbind-response command (name)
        (insert (cider-propertize name 'var)))
      (seq-doseq (argument arguments)
        (insert " ")
        (stateful-check--render-argument argument))
      (insert (cider-propertize ")" 'paren-face)
              (cider-propertize " = " 'font-lock-comment-face))
      (stateful-check--render-result failing-case execution)
      (stateful-check--render-error failing-case execution)
      (insert "\n")
      (stateful-check--render-failures failing-case failures))))

(defun stateful-check--render-execution-long (failing-case execution)
  "Render the Stateful Check EXECUTION of the FAILING-CASE in long form."
  (nrepl-dbind-response execution (arguments command failures)
    (cider-propertize-region (list 'stateful-check-execution execution)
      (insert "    ")
      (stateful-check--render-handle failing-case execution)
      (insert (cider-propertize " = " 'font-lock-comment-face)
              (cider-propertize "(" 'paren-face))
      (nrepl-dbind-response command (name)
        (insert (cider-propertize name 'var)))
      (seq-doseq (argument arguments)
        (insert " ")
        (stateful-check--render-argument argument))
      (insert (cider-propertize ")" 'paren-face))
      (insert "\n      ")
      (cider-insert "=>" 'font-lock-comment-face)
      (insert " ")
      (stateful-check--render-result failing-case execution)
      (stateful-check--render-error failing-case execution)
      (insert "\n")
      (stateful-check--render-failures failing-case failures))))

(defun stateful-check--render-execution (failing-case execution)
  "Render the Stateful Check EXECUTION of the FAILING-CASE."
  (funcall stateful-check-render-execution failing-case execution))

(defun stateful-check--render-sequential-executions (failing-case executions)
  "Render the sequential Stateful Check EXECUTIONS of the FAILING-CASE."
  (cider-propertize-region (list 'stateful-check-sequential-executions executions)
    (cider-insert "\n  Sequential prefix:" 'bold t)
    (cider-insert "  ---------------------------\n" 'font-lock-comment-face)
    (seq-doseq (execution executions)
      (stateful-check--render-execution failing-case execution))))

(defun stateful-check--render-parallel-executions (failing-case executions)
  "Render the parallel Stateful Check EXECUTIONS of the FAILING-CASE."
  (cider-propertize-region (list 'stateful-check-parallel-executions executions)
    (seq-map-indexed (lambda (executions index)
                       (let ((thread (stateful-check--thread-name index)))
                         (cider-insert (format "  Thread %s:" thread) 'bold t)
                         (cider-insert "  ---------------------------\n" 'font-lock-comment-face)
                         (seq-doseq (execution executions)
                           (stateful-check--render-execution failing-case execution))
                         (insert "\n")))
                     executions)))

(defun stateful-check--render-executions (failing-case executions)
  "Render the sequential and parallel Stateful Check EXECUTIONS of the FAILING-CASE."
  (nrepl-dbind-response executions (sequential parallel)
    (stateful-check--render-sequential-executions failing-case sequential)
    (when (cl-plusp (length parallel))
      (insert "\n")
      (stateful-check--render-parallel-executions failing-case parallel))))

(defun stateful-check--failing-case-eval-p (failing-case)
  "Return non-nil if FAILING-CASE is being evaluated, otherwise nil."
  (equal "true" (nrepl-dict-get failing-case "eval?")))

(defun stateful-check--eval-states (failing-case)
  "Return the evaluation state of the FAILING-CASE or nil."
  (nrepl-dict-get-in failing-case '("state-machine" "state")))

(defun stateful-check--render-eval-banner (failing-case)
  "Renders an [EVAL] banner if the failing FAILING-CASE is currently evaluated."
  (when (stateful-check--failing-case-eval-p failing-case)
    (cider-insert " [EVAL]" 'font-lock-comment-face)))

(defun stateful-check--render-first (run)
  "Render the Stateful Check RUN for the first failing case."
  (let* ((failing-case (stateful-check--run-first-case run))
         (executions (nrepl-dict-get failing-case "executions")))
    (cider-propertize-region (list 'stateful-check-first-case executions)
      (cider-insert "First failing test case" 'bold)
      (stateful-check--render-eval-banner failing-case)
      (cider-insert "\n-----------------------\n" 'font-lock-comment-face)
      (stateful-check--render-executions failing-case executions))))

(defun stateful-check--render-smallest (run)
  "Render the Stateful Check RUN for the smallest failing case."
  (let* ((failing-case (stateful-check--run-smallest-case run))
         (executions (nrepl-dict-get failing-case "executions")))
    (cider-propertize-region (list 'stateful-check-smallest-case executions)
      (cider-insert "Smallest case after shrinking" 'bold)
      (stateful-check--render-eval-banner failing-case)
      (cider-insert "\n-----------------------------\n")
      (stateful-check--render-executions failing-case executions))))

(defun stateful-check--render-generation-options (options)
  "Render the Stateful Check generation OPTIONS."
  (nrepl-dbind-response options (max-length max-size threads)
    (cider-insert "  Generation: " 'bold t)
    (insert (format "    Max Length ......... %s\n" max-length))
    (insert (format "    Max Size ........... %s\n" max-size))
    (insert (format "    Threads ............ %s\n" threads))))

(defun stateful-check--render-run-options (options)
  "Render the Stateful Check run OPTIONS."
  (nrepl-dbind-response options (assume-immutable-results max-tries num-tests seed timeout-ms)
    (cider-insert "  Run: " 'bold t)
    (insert (format "    Immutable Results .. %s\n" assume-immutable-results))
    (insert (format "    Max Tries .......... %s\n" max-tries))
    (insert (format "    Num Tests .......... %s\n" num-tests))
    (insert (format "    Seed ............... %s\n" seed))
    (insert (format "    Timeout (ms) ....... %s\n" timeout-ms))))

(defun stateful-check--render-report-options (options)
  "Render the Stateful Check report OPTIONS."
  (nrepl-dbind-response options (command-frequency? first-case?)
    (cider-insert "  Report: " 'bold t)
    (insert (format "    Command Frequency .. %s\n" command-frequency?))
    (insert (format "    First Case ......... %s\n" first-case?))))

(defun stateful-check--render-options (options)
  "Render the Stateful Check OPTIONS."
  (nrepl-dbind-response options (gen report run)
    (cider-propertize-region (list 'stateful-check-options options)
      (cider-insert "Options: " 'bold t)
      (stateful-check--render-generation-options gen)
      (stateful-check--render-run-options run)
      (stateful-check--render-report-options report)
      (insert "\n"))))

(defun stateful-check--render-footer (run)
  "Render the Stateful Check RUN footer."
  (nrepl-dbind-response run (options)
    (cider-propertize-region (list 'stateful-check-footer run)
      (unless stateful-check-render-options
        (when-let (seed (nrepl-dict-get-in run '("seed")))
          (insert "\n")
          (cider-insert "Seed: " 'bold)
          (insert (format "%s\n"seed))))
      (unless (zerop (length (nrepl-dict-get-in run'("result-data" "executions" "parallel"))))
        (insert "\n")
        (cider-insert "Note: " 'bold)
        (cider-insert "Test cases with multiple threads are not deterministic, so using the\n"
                      'font-lock-comment-face)
        (cider-insert "      same seed does not guarantee the same result.\n"
                      'font-lock-comment-face))
      (insert "\n")
      (when stateful-check-render-options
        (stateful-check--render-options options)))))

(defun stateful-check--success-message (run)
  "Return the Stateful Check success message for RUN."
  (nrepl-dbind-response run (num-tests time-elapsed-ms)
    (propertize (format "Specification passed. Ran %s test%s in %s ms."
                        num-tests
                        (if (= 1 num-tests) "" "s")
                        time-elapsed-ms)
                'face 'cider-test-success-face)))

(defun stateful-check--failure-message (run)
  "Return the Stateful Check failure message for RUN."
  (nrepl-dbind-response run (failed-after-ms num-tests shrunk)
    (nrepl-dbind-response shrunk (time-shrinking-ms)
      (propertize (format "Specification failed. Ran %s test%s in %s ms." num-tests
                          (if (= 1 num-tests) "" "s")
                          (+ failed-after-ms time-shrinking-ms))
                  'face 'cider-test-failure-face))))

(defun stateful-check--render-header (run)
  "Render the Stateful Check header for RUN."
  (cider-insert "Stateful Check Debugger" 'bold t)
  (when-let ((ns (stateful-check--run-ns run))
             (var (stateful-check--run-var run)))
    (insert (cider-propertize ns 'ns))
    (cider-insert "/" 'font-lock-comment-face)
    (cider-insert (cider-propertize var 'var))
    (insert "\n"))
  (insert "\n")
  (insert
   (if (stateful-check--run-pass-p run)
       (stateful-check--success-message run)
     (stateful-check--failure-message run)))
  (insert "\n")
  (insert "\n"))

(defun stateful-check--insert-run (run)
  "Insert the Stateful Check RUN into current buffer."
  (cider-propertize-region (list 'stateful-check-run run)
    (unless (stateful-check--run-pass-p run)
      (stateful-check--render-smallest run)
      (when stateful-check-report-first-case-p
        (insert "\n")
        (stateful-check--render-first run)))))

(defun stateful-check--render-run (buffer run)
  "Render the Stateful Check RUN into BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq stateful-check--current-run run)
      (stateful-check--render-header run)
      (stateful-check--insert-run run)
      (stateful-check--render-footer run))))

(defun stateful-check--render-test-event (event)
  "Render the CIDER test EVENT."
  (when-let (run (nrepl-dict-get event "stateful-check"))
    (nrepl-dbind-response event (ns var type)
      (cider-propertize-region (cider-intern-keys (cdr event))
        (let ((run (cider-sync-request:stateful-check-analyze-test (format "%s/%s" ns var)))
              (type-face (cider-test-type-simple-face type)))
          (cider-insert (capitalize type) type-face nil " in ")
          (cider-insert var 'font-lock-function-name-face t)
          (stateful-check--insert-run run)
          (stateful-check--render-footer run)
          (stateful-check-test-report-mode))))))

(defun cider-stateful--show-run (run)
  "Show the Stateful Check RUN in a popup buffer."
  (cider-make-popup-buffer stateful-check-buffer 'stateful-check-mode 'ancillary)
  (with-current-buffer stateful-check-buffer
    (stateful-check--render-run stateful-check-buffer run)
    (cider-popup-buffer-display (current-buffer) stateful-check-auto-select-buffer)
    (goto-char (point-min))
    (stateful-check--next-thing 'stateful-check-execution)))

(defun stateful-check--query-at-point ()
  "Return a NREPL dictionary describing the thing at point."
  (when-let (run (get-text-property (point) 'stateful-check-run))
    (let ((query (nrepl-dict "run" (nrepl-dict-get run "id"))))
      (when (get-text-property (point) 'stateful-check-first-case)
        (nrepl-dict-put query "case" "first"))
      (when (get-text-property (point) 'stateful-check-smallest-case)
        (nrepl-dict-put query "case" "smallest"))
      (when-let (execution (get-text-property (point) 'stateful-check-execution))
        (nrepl-dict-put query "handle" (nrepl-dict-get execution "handle"))
        (when-let (argument (get-text-property (point) 'stateful-check-argument))
          (nrepl-dict-put query "argument" (nrepl-dict-get argument "index")))
        (when-let (argument (get-text-property (point) 'stateful-check-result))
          (nrepl-dict-put query "result" "true"))
        (when-let (failure (get-text-property (point) 'stateful-check-failure))
          (nrepl-dict-put query "failure" (nrepl-dict-get failure "index")))
        (when-let (event (get-text-property (point) 'stateful-check-test-event))
          (nrepl-dict-put query "event" (nrepl-dict-get event "index"))))
      query)))

(defun stateful-check-inspect (query)
  "Inspect the Stateful Check run object described by QUERY."
  (when-let (value (cider-sync-request:stateful-check-inspect query))
    (cider-inspector--render-value value)))

(defun stateful-check-value-at-point ()
  "Return the Stateful Check test run value at point."
  (get-text-property (point) 'stateful-check-value))

(defun stateful-check-print-value-at-point (value)
  "Pretty print the Stateful Check test run VALUE at point."
  (interactive (list (stateful-check-value-at-point)))
  (when (nrepl-dict-p value)
    (nrepl-dbind-response value (cursor)
      (let ((display-value (cider-sync-request:stateful-check-print cursor)))
        (cider-popup-buffer cider-result-buffer 'clojure-mode)
        (with-current-buffer cider-result-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "%s" display-value))
            (goto-char (point-min))))))))

(defun stateful-check-operate-on-point ()
  "Invoke the command for the object at point."
  (interactive)
  (when-let (query (stateful-check--query-at-point))
    (stateful-check-inspect query)))

(defun stateful-check-operate-on-click (event)
  "Move to EVENT's position and operate the part."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point (or (get-text-property point 'cider-value-idx)))
           (goto-char point)
           (stateful-check-operate-on-point))
          (t
           (error "No clickable part here")))))

(defun stateful-check--specifications ()
  "Scan loaded namespaces and test reports for Stateful Check specifications."
  (cider-sync-request:stateful-check-scan)
  (cider-sync-request:stateful-check-specifications))

(defun stateful-check--test-specifications ()
  "Return the Stateful Check specifications from the CIDER test report."
  (seq-filter (lambda (specification)
                (equal "test" (nrepl-dict-get specification "type")))
              (stateful-check--specifications)))

;;;###autoload
(transient-define-suffix stateful-check-run (specification options)
  "Run the Stateful Check SPECIFICATION using OPTIONS."
  :description "Run specification"
  (interactive (list (stateful-check--read-specification (stateful-check--specifications))
                     (stateful-check-options)))
  (nrepl-dbind-response specification (ns var)
    (message "Running specification %s in %s ..." (cider-propertize var 'bold) (cider-propertize ns 'ns))
    (cider-request:stateful-check-run
     specification options
     (lambda (response)
       (nrepl-dbind-response response (status stateful-check/run)
         (cond (stateful-check/run
                (let ((pass? (nrepl-dict-get stateful-check/run "pass?")))
                  (cond ((equal "true" pass?)
                         (if cider-test-show-report-on-success
                             (cider-stateful--show-run stateful-check/run)
                           (stateful-check--render-run stateful-check-buffer stateful-check/run))
                         (message (stateful-check--success-message stateful-check/run)))
                        ((equal "false" pass?)
                         (cider-stateful--show-run stateful-check/run)
                         (stateful-check-next-execution)
                         (message (stateful-check--failure-message stateful-check/run))))))
               ((member "stateful-check/run-error" status)
                (message "Error while running Stateful Check specification."))))))))

(defun stateful-check--rerun-options ()
  "Return the options to re-run a Stateful Check specification."
  (or (and current-prefix-arg (stateful-check-options))
      (with-current-buffer stateful-check-buffer
        (stateful-check--run-options
         stateful-check--current-run))
      (stateful-check-options)))

;;;###autoload
(transient-define-suffix stateful-check-rerun (options)
  "Rerun the Stateful Check specification with OPTIONS."
  :description "Re-run specification"
  (interactive (list (stateful-check--rerun-options)))
  (if (get-buffer stateful-check-buffer)
      (with-current-buffer stateful-check-buffer
        (if-let (specification (stateful-check--run-specification stateful-check--current-run))
            (stateful-check-run specification options)
          (user-error "No Stateful Check specification to re-run")))
    (user-error "No Stateful Check specification to re-run")))

;;;###autoload
(transient-define-suffix stateful-check-scan ()
  "Scan all public vars and test runs for Stateful Check specifications."
  :description "Scan specifications"
  (interactive)
  (let ((old-specs (cider-sync-request:stateful-check-specifications))
        (new-specs (cider-sync-request:stateful-check-scan)))
    (message "[%s/%s] Stateful Check specification scan done."
             (cider-propertize (number-to-string (- (length new-specs) (length old-specs))) 'bold)
             (cider-propertize (number-to-string (length new-specs)) 'bold))))

;;;###autoload
(transient-define-suffix stateful-check-eval-step ()
  "Evaluate the command of a Stateful Check run."
  :description "Evaluate command"
  (interactive)
  (when-let (query (stateful-check--query-at-point))
    (nrepl-dbind-response query (run case)
      (let ((buffer (current-buffer)))
        (cider-request:stateful-check-eval-step
         run case
         (lambda (response)
           (nrepl-dbind-response response (out err status stateful-check/eval-step)
             (cond (err (cider-emit-interactive-eval-err-output err))
                   (out (cider-emit-interactive-eval-output out))
                   (stateful-check/eval-step
                    (let ((run stateful-check/eval-step))
                      (with-current-buffer buffer
                        (setq stateful-check--current-run run)
                        (stateful-check--run-replace run))))
                   ((member "stateful-check/eval-step-error" status)
                    (message "Failed to evaluate Stateful Check command."))))))))))

;;;###autoload
(transient-define-suffix stateful-check-eval-stop ()
  "Stop the evaluation of a Stateful Check run."
  :description "Stop evaluation"
  (interactive)
  (when-let (query (stateful-check--query-at-point))
    (nrepl-dbind-response query (run case)
      (let ((buffer (current-buffer)))
        (cider-request:stateful-check-eval-stop
         run case
         (lambda (response)
           (nrepl-dbind-response response (out err status stateful-check/eval-stop)
             (cond (err (cider-emit-interactive-eval-err-output err))
                   (out (cider-emit-interactive-eval-output out))
                   (stateful-check/eval-stop
                    (let ((run stateful-check/eval-stop))
                      (with-current-buffer buffer
                        (setq stateful-check--current-run run)
                        (stateful-check--run-replace run))))
                   ((member "stateful-check/eval-stop-error" status)
                    (message "Failed to stop the Stateful Check evaluation."))))))))))

;;;###autoload
(defun stateful-check-toggle-first-case ()
  "Toggle the display of the first failing case."
  (interactive)
  (when-let (run stateful-check--current-run)
    (setq stateful-check-report-first-case-p
          (not stateful-check-report-first-case-p))
    (stateful-check--run-replace run)))

;;;###autoload
(defun stateful-check-toggle-render-options ()
  "Toggle the display of the render options."
  (interactive)
  (when-let (run stateful-check--current-run)
    (setq stateful-check-render-options
          (not stateful-check-render-options))
    (stateful-check--run-replace run)))

(defun stateful-check--next-thing (thing)
  "Move point to the next THING, a text property symbol, if one exists."
  (interactive)
  (when-let* ((pos (next-single-property-change (point) thing)))
    (if (get-text-property pos thing)
        (goto-char pos)
      (when-let* ((pos (next-single-property-change pos thing)))
        (goto-char pos)))))

(defun stateful-check--previous-thing (thing)
  "Move point to the previous THING, a text property symbol, if one exists."
  (interactive)
  (when-let* ((pos (previous-single-property-change (point) thing)))
    (if (get-text-property pos thing)
        (goto-char pos)
      (when-let* ((pos (previous-single-property-change pos thing)))
        (goto-char pos)))))

(defun stateful-check-next-execution ()
  "Move point to the next command execution, if one exists."
  (interactive)
  (stateful-check--next-thing 'stateful-check-execution))

(defun stateful-check-previous-execution ()
  "Move point to the previous command execution, if one exists."
  (interactive)
  (stateful-check--previous-thing 'stateful-check-execution))

(defun stateful-check-show-test-report (id)
  "Show the Stateful Check test report ID at point in the debugger."
  (interactive (list (stateful-check--run-id-at-point)))
  (when id
    (when-let (run (cider-sync-request:stateful-check-analyze-test id))
      (cider-stateful--show-run run))))

(defun stateful-check--define-menu (keymap)
  "Define a Stateful Check menu for the KEYMAP."
  (easy-menu-define stateful-check-mode-menu keymap
    "Menu for CIDER's Stateful Check debugger."
    `("CIDER Stateful Check"
      ["Re-run specification" stateful-check-rerun]
      ["Run specification" stateful-check-run]
      ["Scan specifications" stateful-check-scan]
      "--"
      ["Inspect object at point" stateful-check-operate-on-point]
      ["Next Inspectable Object" cider-inspector-next-inspectable-object]
      ["Previous Inspectable Object" cider-inspector-previous-inspectable-object]
      "--"
      ["Quit" cider-popup-buffer-quit-function])))

(defvar stateful-check-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (stateful-check--define-menu map)
    (define-key map "P" #'stateful-check-print-value-at-point)
    (define-key map "\C-i" #'cider-inspector-next-inspectable-object)
    (define-key map "b" #'backward-char)
    (define-key map "d" #'cider-test-ediff)
    (define-key map "e" #'stateful-check-eval-step)
    (define-key map "f" #'forward-char)
    (define-key map "g" #'stateful-check-rerun)
    (define-key map "k" #'stateful-check-eval-stop)
    (define-key map "n" #'stateful-check-next-execution)
    (define-key map "o" #'stateful-check-toggle-render-options)
    (define-key map "p" #'stateful-check-previous-execution)
    (define-key map "t" #'stateful-check-toggle-first-case)
    (define-key map "x" #'stateful-check)
    (define-key map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
    (define-key map (kbd "RET") #'stateful-check-operate-on-point)
    (define-key map [(shift tab)] #'cider-inspector-previous-inspectable-object)
    (define-key map [mouse-1] #'stateful-check-operate-on-click)
    (define-key map [tab] #'cider-inspector-next-inspectable-object)
    ;; Emacs translates S-TAB to BACKTAB on X.
    (define-key map [backtab] #'cider-inspector-previous-inspectable-object)
    map))

(define-derived-mode stateful-check-mode special-mode "Stateful Check"
  "Major mode for debugging Stateful Check specifications."
  (set-syntax-table clojure-mode-syntax-table)
  (setq-local cider-inspector-skip-uninteresting nil)
  (setq-local electric-indent-chars nil)
  (setq-local sesman-system 'CIDER)
  (setq-local truncate-lines t))

(define-minor-mode stateful-check-test-report-mode
  "Minor mode for debugging Stateful Check specifications in the CIDER test report."
  :keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map cider-popup-buffer-mode-map)
            (stateful-check--define-menu map)
            (define-key map "P" #'stateful-check-print-value-at-point)
            (define-key map "\C-i" #'cider-inspector-next-inspectable-object)
            (define-key map "b" #'backward-char)
            (define-key map "d" #'cider-test-ediff)
            (define-key map "e" #'stateful-check-eval-step)
            (define-key map "f" #'forward-char)
            (define-key map "k" #'stateful-check-eval-stop)
            (define-key map "n" #'stateful-check-next-execution)
            (define-key map "o" #'stateful-check-toggle-render-options)
            (define-key map "p" #'stateful-check-previous-execution)
            (define-key map "t" #'stateful-check-toggle-first-case)
            (define-key map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
            (define-key map (kbd "RET") #'stateful-check-operate-on-point)
            (define-key map [(shift tab)] #'cider-inspector-previous-inspectable-object)
            (define-key map [mouse-1] #'stateful-check-operate-on-click)
            (define-key map [tab] #'cider-inspector-next-inspectable-object)
            ;; Emacs translates S-TAB to BACKTAB on X.
            (define-key map [backtab] #'cider-inspector-previous-inspectable-object)
            map))

(defun stateful-check--render-test-event-p (event)
  "Return non-nil if the CIDER test EVENT should be rendered."
  (and (nrepl-dict-p event)
       (nrepl-dict-get-in event '("stateful-check" "result-data" "specification"))))

(defun stateful-check--cider-test-render-assertion (orig-fun &rest args)
  "Advice around `cider-test-render-assertion' using ORIG-FUN and ARGS."
  (let ((event (elt args 1)))
    (if (stateful-check--render-test-event-p event)
        (stateful-check--render-test-event event)
      (apply orig-fun args))))

;; Transient

(transient-define-infix stateful-check:gen-max-length ()
  "The transient option to set `stateful-check-gen-max-length`."
  :class 'transient-lisp-variable
  :description "Max command length"
  :key "-l"
  :transient t
  :variable 'stateful-check-gen-max-length)

(transient-define-infix stateful-check:gen-max-size ()
  "The transient option to set `stateful-check-gen-max-size`."
  :class 'transient-lisp-variable
  :description "Max size for generated values"
  :key "-s"
  :transient t
  :variable 'stateful-check-gen-max-size)

(transient-define-infix stateful-check:gen-threads ()
  "The transient option to set `stateful-check-gen-threads`."
  :class 'transient-lisp-variable
  :description "Number of threads"
  :key "-t"
  :transient t
  :variable 'stateful-check-gen-threads)

(transient-define-infix stateful-check:run-assume-immutable-results-p ()
  "The transient option to set `stateful-check-run-assume-immutable-results-p`."
  :class 'transient-lisp-variable
  :description "Assume immutable results"
  :key "-i"
  :transient t
  :variable 'stateful-check-run-assume-immutable-results-p)

(transient-define-infix stateful-check:run-max-tries ()
  "The transient option to set `stateful-check-run-max-tries`."
  :class 'transient-lisp-variable
  :description "Number of fail attempts"
  :key "-T"
  :transient t
  :variable 'stateful-check-run-max-tries)

(transient-define-infix stateful-check:run-num-tests ()
  "The transient option to set `stateful-check-run-num-tests`."
  :class 'transient-lisp-variable
  :description "Number of tests to run"
  :key "-n"
  :transient t
  :variable 'stateful-check-run-num-tests)

(transient-define-infix stateful-check:run-seed ()
  "The transient option to set `stateful-check-run-seed`."
  :class 'transient-lisp-variable
  :description "Initial seed to use for generation"
  :key "-S"
  :transient t
  :variable 'stateful-check-run-seed)

(transient-define-infix stateful-check:run-timeout-ms ()
  "The transient option to set `stateful-check-run-timeout-ms`."
  :class 'transient-lisp-variable
  :description "Test timeout in ms"
  :key "-x"
  :transient t
  :variable 'stateful-check-run-timeout-ms)

(transient-define-infix stateful-check:report-first-case-p ()
  "The transient option to set `stateful-check-report-first-case-p`."
  :class 'transient-lisp-variable
  :description "Report first failing case"
  :key "-f"
  :transient t
  :variable 'stateful-check-report-first-case-p)

(transient-define-infix stateful-check:report-command-frequency-p ()
  "The transient option to set `stateful-check-report-command-frequency`."
  :class 'transient-lisp-variable
  :description "Report command frequency"
  :key "-F"
  :transient t
  :variable 'stateful-check-report-command-frequency-p)

(transient-define-infix stateful-check:report-render-options ()
  "The transient option to set `stateful-check-report-command-frequency`."
  :class 'transient-lisp-variable
  :description "Render options"
  :key "-o"
  :transient t
  :variable 'stateful-check-render-options)

(transient-define-prefix stateful-check ()
  "A transient menu to set the Stateful Check specification run options."
  [["Stateful Check Debugger\n\nGeneration Options"
    (stateful-check:gen-max-length)
    (stateful-check:gen-max-size)
    (stateful-check:gen-threads)]
   ["Run Options"
    (stateful-check:run-assume-immutable-results-p)
    (stateful-check:run-max-tries)
    (stateful-check:run-num-tests)
    (stateful-check:run-timeout-ms)
    (stateful-check:run-seed)]
   ["Report Options"
    (stateful-check:report-command-frequency-p)
    (stateful-check:report-first-case-p)
    (stateful-check:report-render-options)]]
  [["Specification Actions"
    ("g" stateful-check-rerun)
    ("r" stateful-check-run)
    ("s" stateful-check-scan)]
   ["Eval Actions"
    ("e" stateful-check-eval-step)
    ("S" stateful-check-eval-stop)]])

(advice-add 'cider-test-render-assertion :around #'stateful-check--cider-test-render-assertion)
(define-key cider-test-report-mode-map (kbd "D") #'stateful-check-show-test-report)

(provide 'stateful-check)

;;; stateful-check.el ends here
