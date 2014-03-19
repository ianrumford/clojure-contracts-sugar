(ns clojure-contracts-sugar
  (:require [clojure.core.contracts :as ccc]
            [clojure-contracts-sugar.contracts.store :as contracts-store]
            [clojure-contracts-sugar.aspects.contracts :as aspects-contracts :refer (resolve-aspects-contracts)]
            [clojure-carp :as carp :refer (surprise-exception)]))


;; Sugar macros, helpers, etc for clojure.core.contracts

;; "A work in progress"

;; ********************
;; BEG: contracts store
;; ********************

(defmacro configure-contracts-store
  [& opt-args]
  (apply contracts-store/configure-contracts-store opt-args)
  nil)

;; ********************
;; FIN: contracts store
;; ********************

;; ***************************
;; BEG: using aspect contracts
;; ***************************

(defmacro apply-contract-aspects
  "takes a function and a list of contract definitsions
   and applies the contracts constraint
   the return value is a new function with constraint applied"
  [base-fn & aspect-mnemonics]
  (let [aspect-contracts (aspects-contracts/resolve-aspects-contracts aspect-mnemonics)
        apply-form `(ccc/with-constraints ~base-fn ~@(for [[k v] aspect-contracts] v))]
    `(do
        ~apply-form)))

(defmacro update-contract-aspects
  "takes a function and a list of contract mnemonics
   and uses alter-var-root to update the function definition
   with the contracts / constraints applied
   note the return value is the new function as well"
  [base-fn & aspect-mnemonics]
  (let [aspect-contracts (aspects-contracts/resolve-aspects-contracts aspect-mnemonics)
        update-form (list `alter-var-root (list `var base-fn)
                          (list `fn '[f & c] `(ccc/with-constraints ~'f ~@(for [[k v] aspect-contracts] v))))
        mnemonic-to-contract-map (into {} (map (fn [cs rcs] [cs rcs]) aspect-mnemonics aspect-contracts))]
    `(do
       ~update-form)))

;; ***************************
;; FIN: using aspect contracts
;; ***************************

(defn -main
  [& contract-store-init]
  (doall (println "clojures-contracts-sugar -main does nothing")))

