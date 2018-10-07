(defproject basically "0.1.1"
  :description "A basic BASIC interpreter, basically"
  :url "https://github.com/soudy/basically"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.cli "0.4.1"]]
  :main ^:skip-aot basically.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
