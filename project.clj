(defproject basically "0.1.0-SNAPSHOT"
  :description "A basic BASIC interpreter, basically"
  :url "https://github.com/soudy/basically"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot basically.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
