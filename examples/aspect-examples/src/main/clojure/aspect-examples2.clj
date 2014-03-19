(ns aspect-examples2
  (:require [clojure.core.contracts :as ccc]
            [clojure-contracts-sugar :as ccs
             :refer (apply-contract-aspects
                     update-contract-aspects
                     configure-contracts-store)]
            [clojure-carp :as carp
             :refer (trace-value-body
                     trace-value-entr
                     trace-value-exit)]
            [taoensso.timbre.profiling :as profiling]
            [clojure-contracts-sugar.utils.utils :as utils]
            [clojure-contracts-sugar.aspects.constraints :as aspect-constraints
             :refer (aspect-assertion-factory
                     aspect-argument-factory
                     aspect-constraint-factory
                     aspect-constraint-merge
                     )]
            ))

(carp/macro-set-trace true *ns* "ENTR")
(carp/trace-configure :first-telltale-format-specification "%-40s")

;; Helper for accessor examples expected to work.  Returns the expected result, else fails

(defn will-workxxx
  [fn-constrained & fn-args]
  (let [actual-result (apply fn-constrained fn-args)]
    (println "will-work" "worked as expected" "actual-result" actual-result "fn-constrained" fn-constrained "fn-args" fn-args)
    actual-result))

;; Helper for accessor examples expected to fail.  Catches the expected AssertionError, else fails.
;; A nil return from the function is ok

(defn will-failxxx
  [fn-constrained & fn-args]
  (try
    (do
      (let [return-value (apply fn-constrained fn-args)]
        (if return-value (assert (println "will-fail" "DID NOT FAIL" "did not cause AssertionError" "fn-constrained" fn-constrained "fn-args" fn-args "RETURN-VALUE" (class return-value) return-value)))))
    (catch AssertionError e
      (println "will-fail" "failed as expected" "fn-constrained" fn-constrained "fn-args" fn-args))))


(defn will-work [& args] (profiling/p :will-work (apply will-workxxx args)))
(defn will-fail [& args] (profiling/p :will-fail (apply will-failxxx args)))


;; Wrapper to run all tests


(utils/define-memoized-functions

  [
   ;;carp/trace-value-body
   ;;   trace-value-body

   ]
  
  )


;; prevent an error from lein run
(defn -main
  [& args]
  (profiling/profile
   :info :Arithmetic
   (dotimes [n 1000] (do
                       ;;(run-all-tests args)

                       (let [
                             assertions (map-indexed
                                         (fn [ndx ass]
                                           (doall (println "new assertion" ndx ass))
                                           (aspect-assertion-factory ass))
                                         '[map? vector? number? string?])

                             _ (doall (println "assertions" assertions))

                             
                             arguments (map-indexed
                                        (fn [ndx ass]
                                          (aspect-argument-factory :suck ndx [ass]))
                                        assertions)

                             _ (doall (println "arguments" arguments))
                             
                             constraints (map-indexed
                                          (fn [ndx arg]
                                            (aspect-constraint-factory [arg]))
                                          arguments)

                             _ (doall (println "constraints" constraints))
                             merged-constraint (apply aspect-constraint-merge constraints)

                             ])
                       
                       )))


  )
