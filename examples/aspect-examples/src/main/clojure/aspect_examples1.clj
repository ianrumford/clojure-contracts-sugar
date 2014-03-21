(ns aspect-examples1
  (:require [clojure-contracts-sugar :as ccs
             :refer (apply-contract-aspects
                     update-contract-aspects
                     configure-contracts-store)]))

;; Helper for accessor examples expected to work.  Returns the expected result, else fails

(defn will-work
  [fn-constrained & fn-args]
  (let [actual-result (apply fn-constrained fn-args)]
    (println "will-work" "worked as expected" "actual-result" actual-result "fn-constrained" fn-constrained "fn-args" fn-args)
    actual-result))

;; Helper for accessor examples expected to fail.  Catches the expected AssertionError, else fails.
;; A nil return from the function is ok

(defn will-fail
  [fn-constrained & fn-args]
  (try
    (do
      (let [return-value (apply fn-constrained fn-args)]
        (if return-value (assert (println "will-fail" "DID NOT FAIL" "did not cause AssertionError" "fn-constrained" fn-constrained "fn-args" fn-args "RETURN-VALUE" (class return-value) return-value)))))
    (catch AssertionError e
      (println "will-fail" "failed as expected" "fn-constrained" fn-constrained "fn-args" fn-args))))

;; Wrapper to run all tests
(defn run-all-tests
 [& args]

;; Example - applying a built-in predicate

;; any-fn is the "base" function

(defn any-fn [x] x)

;; map-fn is the new function constrained to suck a map

(def suck-map-fn1 (apply-contract-aspects any-fn map?))

;; This will work

(will-work suck-map-fn1 {:a 1 :b 2 :c 3})

;; But this will fail since suck-map-fn1 can only suck a map

(will-fail suck-map-fn1 [1 2 3])

;; The original function any-fn is unchanged and not constrained in any way

(will-work any-fn {:a 1 :b 2 :c 3})
(will-work any-fn [1 2 3])
(will-work any-fn :a)
(will-work any-fn 99)

;; Example - suck a vector

(def suck-vector-fn1 (apply-contract-aspects (fn [x] x) vector?))

(will-work suck-vector-fn1 [1 2 3])

(will-fail suck-vector-fn1 99)

;; Example - applying your own custom predicate

;; The custom predicate ensures the argument is a map, its keys are keywords and values are numbers.

(defn is-map-with-keyword-keys-and-numeric-values?
  [x]
  {:pre [(map? x) (every? keyword? (keys x)) (every? number? (vals x))]}
  x)

(def map-keyword-keys-numeric-values-fn1 (apply-contract-aspects any-fn is-map-with-keyword-keys-and-numeric-values?))

;; This will work

(will-work map-keyword-keys-numeric-values-fn1 {:a 1 :b 2 :c 3})

;; But these will fail the contracts

(will-fail map-keyword-keys-numeric-values-fn1 {:a :x :b 2 :c 3})
(will-fail map-keyword-keys-numeric-values-fn1 {"x" 1 :b 2 :c 3})
(will-fail map-keyword-keys-numeric-values-fn1 [1 2 3])

;; As before the original function any-fn is unchanged and not constrained in any way

(will-work any-fn {:a 1 :b 2 :c 3})
(will-work any-fn [1 2 3])
(will-work any-fn :a)
(will-work any-fn 99)

;; Example - updating a function with a built-in predicate

;; any-fn is "changed" to now only suck a map

(update-contract-aspects any-fn map?)

;; This will work

(will-work any-fn {:a 1 :b 2 :c 3})

;; But this will fail as any-fn can now only suck a map

(will-fail any-fn [1 2 3])

;; Example - suck a map and keyword and spit a vector

;; In this example, the assertion constrains the function to suck a map and keyword
;; and spit a vector.  

;; The function looks up the value of the keyword in the map.

(def suck-map-keyword-spit-vector-fn1 (apply-contract-aspects (fn [m k] (k m)) {:suck [map? keyword?] :spit vector?}))

;; This will work as key :c contains a vector

(will-work suck-map-keyword-spit-vector-fn1 {:a 1 :b 2 :c [1 2 3]} :c)

;; But these will fail

(will-fail suck-map-keyword-spit-vector-fn1 {:a 1 :b 2 :c 3} :c)
(will-fail suck-map-keyword-spit-vector-fn1 {:a 1 :b 2 :c 3} :d)

;; Example - suck a map - with keyword keys and numeric values - and keyword and spit a vector

;; In this example, the contract constrains the function to suck a map and keyword, spit a number.

;; The map must have keywords keys and numeric values.

(def suck-map-keyword-spit-number-fn1 (apply-contract-aspects (fn [m k] (k m)) {:suck [[map? (every? keyword? (keys arg0)) (every? number? (vals arg0))] keyword?] :spit number?}))

;; This will work

(will-work suck-map-keyword-spit-number-fn1 {:a 1 :b 2 :c 3} :a)

;; But these will fail their contracts

(will-fail suck-map-keyword-spit-number-fn1 {:a :x :b 2 :c 3} :a)
(will-fail suck-map-keyword-spit-number-fn1 {:a 1 :b 2 :c 3} :d)
(will-fail suck-map-keyword-spit-number-fn1 {"x" 1 :b 2 :c 3} :c)

;; Example - specifying argument order explicitly

;; In this example, the arguments are specified by their explicit position in the argument order

(def explicit-argument-order-fn1 (apply-contract-aspects (fn [k m] (k m)) {:suck {0 :keyword 1 [:map (every? keyword? (keys arg0)) (every? number? (vals arg0))]} :spit :number}))

;; This will work

(will-work explicit-argument-order-fn1 :a {:a 1 :b 2 :c 3})

;; But these will fail their contracts

(will-fail explicit-argument-order-fn1 :a {:a :x :b 2 :c 3})
(will-fail explicit-argument-order-fn1 :d {:a 1 :b 2 :c 3})
(will-fail explicit-argument-order-fn1 :c {"x" 1 :b 2 :c 3})

;; Example - suck map and spit vector using CCC form

(def suck-map-spit-vector-fn1 (apply-contract-aspects (fn [m] (:c m)) [[v] [map? => vector?]]))

(will-work suck-map-spit-vector-fn1 {:a 1 :b 2 :c [1 2 3]})

(will-fail suck-map-spit-vector-fn1 {:a 1 :b 2 :c 1})

;; Example - suck map, spit vector but also all map keys are keywords

(def suck-map-keyword-keys-fn1 (apply-contract-aspects (fn [m] (:c m)) [[v] [map? (every? keyword? (keys v)) => vector?]]))

(will-work suck-map-keyword-keys-fn1 {:a 1 :b 2 :c [1 2 3]})

(will-fail suck-map-keyword-keys-fn1 {"x" 1 :b 2 :c 1})

;; Example - using CCC's format with rich assertions

;; In this example, the assertion constrains the function to suck a map,
;; with keywords keys and numeric values, and a keyword.

;; The returned value is unconstrained

(def map-keyword-keys-numeric-vals-fn2 (apply-contract-aspects (fn [m k] (k m)) [[m k] [(map? m) (every? keyword (keys m)) (every? number? (vals m)) (keyword? k)]]))

;; This will work and return nil as the return value is not constrained

(will-work map-keyword-keys-numeric-vals-fn2 {:a 1 :b 2 :c 3} :d)

(will-fail map-keyword-keys-numeric-vals-fn2 {:a 1 :b 2 :c 3} "d")
(will-fail map-keyword-keys-numeric-vals-fn2 {:a :x :b 2 :c 3} :a)
(will-fail map-keyword-keys-numeric-vals-fn2 {"x" 1 :b 2 :c 3} :d)

;; Example - using CCC's format in a suck definition

;; Not the clearest way of specifying the contract

(def using-ccc-form-in-the-suck-definition-fn1 (apply-contract-aspects (fn [m k s] (k m)) {:suck [:map [[k s] [(keyword? k) (string? s) => map?]]] :spit :number} ))

(will-work using-ccc-form-in-the-suck-definition-fn1 {:a 1 :b 2 :c 3} :a "s2")
(will-fail using-ccc-form-in-the-suck-definition-fn1 {:a 1 :b 2 :c 3} "d" "s2")
(will-fail using-ccc-form-in-the-suck-definition-fn1 {:a :x :b 2 :c 3} :a 1 )
(will-fail using-ccc-form-in-the-suck-definition-fn1 {"x" 1 :b 2 :c 3} :d "s2")

;; Example - using a built-in mnemonic

;; This is a contrived example to show the symmetry when using a buit-in mnemonic.
;; BTW The function hard-codes a map as it return value so will always satisfy the spit constraint.

(def mnemonic-suck-and-spit-map-fn1 (apply-contract-aspects (fn [x] {:x 1 :y 2 :z 3}) :map))

;; This will work because the argument is a map and the (hard-coded) return value is a map

(will-work mnemonic-suck-and-spit-map-fn1 {:a 1 :b 2 :c 3})

;; But this fail sicne the argument is not a map

(will-fail mnemonic-suck-and-spit-map-fn1 [1 2 3])

;; Example - applying built-in mnemonics to individual arguments and the result

;; In this example, built-in mnemonics are used to constrains the
;; function to suck a map and keyword and spit a vector.

(def suck-map-keyword-spit-vector-fn1 (apply-contract-aspects (fn [m k] (k m)) {:suck [:map :keyword] :spit :vector}))

;; This will work as key :c contains a vector

(will-work suck-map-keyword-spit-vector-fn1 {:a 1 :b 2 :c [1 2 3]} :c)

;; But these will fail their contract

(will-fail suck-map-keyword-spit-vector-fn1 {:a 1 :b 2 :c 3} :c)
(will-fail suck-map-keyword-spit-vector-fn1 {:a 1 :b 2 :c 3} :d)

;; Changing a Built-in Mnemonic Contract Definition

;; Change the built-in :map mnemonics to also check the keys are keywords

(configure-contracts-store aspect-mnemonic-definitions {:map {:suck [[map? (every? keyword? (keys arg0))]]}})

;; Example - re-defining the :map built-in mnemonic

;; In this example, the :map built-in mnemonic has been changed to check the keys are keywords.

(def suck-map-keyword-spit-vector-fn1 (apply-contract-aspects (fn [m k] (k m)) {:suck [:map :keyword] :spit :vector}))

;; This will work as key :c contains a vector

(will-work suck-map-keyword-spit-vector-fn1 {:a 1 :b 2 :c [1 2 3]} :c)

;; But this will fail the contract as "x" is not a keyword.

(will-fail suck-map-keyword-spit-vector-fn1 {"x" 1 :b 2 :c 3} :c)

;; Example - add a new mnemonic to the contracts store

;; The new mnemonic - :map-keyword-keys-numeric-vals - constrains an
;; argument to be a map with keyword keys and numeric values.

(configure-contracts-store
 aspect-mnemonic-definitions
 {:map-keyword-keys-numeric-vals {:suck [[map? (every? keyword? (keys arg0)) (every? number? (vals arg0))]]}})

;; Example - using a custom mnemonic

;; In this example, the assertion constrains the function to suck a map and keyword, spit a number.

;; The map must have keywords keys and numeric values.

(def mnemonic-suck-map-keyword-spit-number-fn1 (apply-contract-aspects (fn [m k] (k m)) {:suck [:map-keyword-keys-numeric-vals :keyword] :spit :number}))

;; This will work

(will-work mnemonic-suck-map-keyword-spit-number-fn1 {:a 1 :b 2 :c 3} :a)

;; But these will fail their contracts

(will-fail mnemonic-suck-map-keyword-spit-number-fn1 {:a :x :b 2 :c 3} :a)
(will-fail mnemonic-suck-map-keyword-spit-number-fn1 {:a 1 :b 2 :c 3} :d)
(will-fail mnemonic-suck-map-keyword-spit-number-fn1 {"x" 1 :b 2 :c 3} :c)

;; Using a Custom Mnemonic to package multiple arguments

;; The new mnemonic combines the assertions to ensure the first argument
;; is a map with keyword keys and numerics value and also the requirement
;; for the second argument to be a keyword.

(configure-contracts-store aspect-mnemonic-definitions {:suck-map-keyword-keys-numeric-vals-and-keyword {:suck [[map? (every? keyword? (keys arg0)) (every? number? (vals arg0))] keyword?]}})

;; Example - using a custom multiple argument suck mnemonic

;; In this example, the map assertion uses a mnemonic to ensure keywords keys and numeric values.

(def mnemonic-suck-map-keyword-spit-number-fn2 (apply-contract-aspects (fn [m k] (k m)) {:suck :suck-map-keyword-keys-numeric-vals-and-keyword :spit :number}))

;; Using the same tests as above

(will-work mnemonic-suck-map-keyword-spit-number-fn2 {:a 1 :b 2 :c 3} :a)
(will-fail mnemonic-suck-map-keyword-spit-number-fn2 {:a :x :b 2 :c 3} :a)
(will-fail mnemonic-suck-map-keyword-spit-number-fn2 {:a 1 :b 2 :c 3} :d)
(will-fail mnemonic-suck-map-keyword-spit-number-fn2 {"x" 1 :b 2 :c 3} :c)

;; Using a Custom Mnemonic to package the complete contract

;; The custom mnemonic combines the assertions to ensure the first
;; argument is a map with keyword keys and numerics value and also the
;; requirement for the second argument to be a keywork. It also includes
;; the requirement for the return value to be a number.

(configure-contracts-store
 aspect-mnemonic-definitions
 {:contract-suck-map-keyword-keys-numeric-vals-and-keyword-spit-number 
  {:suck [[map? (every? keyword? (keys arg0)) (every? number? (vals arg0))] keyword?] :spit :number}})

;; Example - using a custom mnemonic to package the whole contract

;; In this example, the a mnemonic packages the complete assertion

(def mnemonic-suck-map-keyword-spit-number-fn3 
  (apply-contract-aspects (fn [m k] (k m)) :contract-suck-map-keyword-keys-numeric-vals-and-keyword-spit-number))

;; Exactly the same tests as above

(will-work mnemonic-suck-map-keyword-spit-number-fn3 {:a 1 :b 2 :c 3} :a)
(will-fail mnemonic-suck-map-keyword-spit-number-fn3 {:a :x :b 2 :c 3} :a)
(will-fail mnemonic-suck-map-keyword-spit-number-fn3 {:a 1 :b 2 :c 3} :d)
(will-fail mnemonic-suck-map-keyword-spit-number-fn3 {"x" 1 :b 2 :c 3} :c)

;; Using Mnemonics in Custom Mnemeonics

;; The first customer mnemonic constrains a map to have keyword keys and numeric values.

;; The second custome mnemonic speficiy the constrained map and a keyword as the second argument.

;; The third custom mnemonic uses the second mnemonic to build a
;; complete contract mnemonic for a two argument function sucking the
;; constrained map and a keyword, and spitting a number.

(configure-contracts-store
 aspect-mnemonic-definitions
 {:suck-map-special {:suck [[map? (every? keyword? (keys arg0)) (every? number? (vals arg0))]]}
  :suck-map-special-and-keyword {:suck [:suck-map-special :keyword]}
  :contract-suck-map-special-and-keyword-spit-number {:suck :suck-map-special-and-keyword :spit :number}})

;; Example - using a mnemonic containing mnemonics

;; In this example, the three level mnemonic packages the complete assertion

(def mnemonic-suck-map-special-keyword-spit-number-fn1 (apply-contract-aspects (fn [m k] (k m)) :contract-suck-map-special-and-keyword-spit-number))

;; Exactly the same tests as above

(will-work mnemonic-suck-map-special-keyword-spit-number-fn1 {:a 1 :b 2 :c 3} :a)
(will-fail mnemonic-suck-map-special-keyword-spit-number-fn1 {:a :x :b 2 :c 3} :a)
(will-fail mnemonic-suck-map-special-keyword-spit-number-fn1 {:a 1 :b 2 :c 3} :d)
(will-fail mnemonic-suck-map-special-keyword-spit-number-fn1 {"x" 1 :b 2 :c 3} :c)

(configure-contracts-store
 aspect-mnemonic-definitions
 {:suck-keyword-and-map-special {:suck [:keyword :suck-map-special]}
  :contract-suck-keyword-and-map-special-spit-number {:suck :suck-keyword-and-map-special :spit :number}})

;; Example - swapping the keyword and map in the three level composed mnemonics

;; In this example, the keyword and map are swapped in the three level mnemonic

(def mnemonic-suck-keyword-map-special-spit-number-fn1 (apply-contract-aspects (fn [k m] (k m)) :contract-suck-keyword-and-map-special-spit-number ))

;; The same tests as above but the arguments swapped

(will-work mnemonic-suck-keyword-map-special-spit-number-fn1 :a {:a 1 :b 2 :c 3})
(will-fail mnemonic-suck-keyword-map-special-spit-number-fn1 :a {:a :x :b 2 :c 3})
(will-fail mnemonic-suck-keyword-map-special-spit-number-fn1 :d {:a 1 :b 2 :c 3})
(will-fail mnemonic-suck-keyword-map-special-spit-number-fn1 :c {"x" 1 :b 2 :c 3})

(configure-contracts-store
 aspect-mnemonic-definitions
 {:keyword-in-first-argument-map {:suck [[:keyword (contains? abs-arg0 arg0)]]}})

;; Example - using absolute arguments in mnemonics

;; This function takes a map, string and keyword, and returns a number.

;; The map must have keyword keys and numberic values.

;; The keyword must exist in the map

(def absolute-argument-mnemonic-fn1 (apply-contract-aspects (fn [m s k] (k m)) {:suck [:suck-map-special :string :keyword-in-first-argument-map] :spit :number}))

;; The same tests as above but the arguments swapped

(will-work absolute-argument-mnemonic-fn1 {:a 1 :b 2 :c 3} "s1" :a)
(will-fail absolute-argument-mnemonic-fn1 {:a :x :b 2 :c 3} "s1" :a)
(will-fail absolute-argument-mnemonic-fn1 {:a 1 :b 2 :c 3} "s1" :d)
(will-fail absolute-argument-mnemonic-fn1 {"x" 1 :b 2 :c 3} "s1" :c)

;; Example - two arities (map => number) and (map,keyword => number)

;; This is the target function with two arities

(defn two-arity-fn1
  ([m] (:a m))
  ([m k] (k m)))

;; The constrained function

(def constrained-two-arity-fn1 (apply-contract-aspects two-arity-fn1 [{:suck :map :spit :number} {:suck [:map :keyword] :spit :vector}]))

;; First Arity Tests

;; This will works as value of key :a is a number

(will-work constrained-two-arity-fn1 {:a 1 :b 2 :c [1 2 3]})

; This will fail as value of key :a is not a number

(will-fail constrained-two-arity-fn1 {:a "x"})

;; Second Arity Tests

;; This will work as value of key :c is a vector

(will-work constrained-two-arity-fn1 {:a 1 :b 2 :c [1 2 3]} :c)

; This will fail as value of key :d is not a vector (its nil)

(will-fail constrained-two-arity-fn1 {:a "x"} :d)

;; Example - multiple arities using mixed CCC form and map form 

;; The same multiple arity example as above but using a mixed contract definition with CCC form and map form.

(def constrained-two-arity-fn1 (apply-contract-aspects two-arity-fn1 [[[m] [map? => number?]]  {:suck [:map :keyword] :spit :vector}]))

;; First Arity Tests

;; This will works as value of key :a is a number

(will-work constrained-two-arity-fn1 {:a 1 :b 2 :c [1 2 3]})

; This will fail as value of key :a is not a number

(will-fail constrained-two-arity-fn1 {:a "x"})

;; Second Arity Tests

;; This will work as value of key :c is a vector

(will-work constrained-two-arity-fn1 {:a 1 :b 2 :c [1 2 3]} :c)

; This will fail as value of key :d is not a vector (its nil)

(will-fail constrained-two-arity-fn1 {:a "x"} :d)

;; Close the wrapper
)

;; prevent an error from lein run
(defn -main
  [& args]
  ;;(profiling/profile :info :Arithmetic (dotimes [n 1] (run-all-tests args)))
  (dotimes [n 1] (run-all-tests args)))
