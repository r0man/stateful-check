(defproject org.clojars.czan/stateful-check "0.4.5-SNAPSHOT"
  :description "Stateful generative testing in clojure"
  :url "https://github.com/czan/stateful-check"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[cider/cider-nrepl "0.51.1"] ;; How to make this provided? lein test fails
                 [mx.cider/haystack "0.3.3"] ;; This unfortunatly gets renamed in cider
                 [org.clojure/clojure "1.11.1"]
                 [org.clojure/test.check "1.1.0"]]
  :test-selectors {:default #(not (:interactive %))
                   :interactive :interactive}
  :plugins [[jonase/eastwood "1.4.2"]]
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :creds :gpg}]])
