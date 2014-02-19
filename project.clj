(defproject name.rumford/clojure-contracts-sugar "0.1.0"
  :description "Some sugar macros for clojure.core.contracts"
  :url "https://github.com/ianrumford/clojure-contracts-sugar"
  :license {:name "MIT" :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/core.contracts "0.0.5"]
                 [name.rumford/clojure-carp "0.1.0"]
                 [org.clojure/core.memoize "0.5.6"]
                 [com.taoensso/timbre "3.0.0"]]
  :source-paths ["src/main/clojure"]
  :test-paths   ["src/test/clojure"])
