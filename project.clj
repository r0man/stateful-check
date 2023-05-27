(defproject org.clojars.czan/stateful-check "0.4.2"
  :description "Stateful generative testing in clojure"
  :url "https://github.com/czan/stateful-check"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[com.gfredericks/test.chuck "0.2.14"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "1.1.1"]]
  :test-selectors {:default #(not (:interactive %))
                   :interactive :interactive}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :creds :gpg}]])
