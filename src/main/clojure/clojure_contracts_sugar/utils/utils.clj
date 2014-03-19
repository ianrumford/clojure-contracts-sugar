(ns clojure-contracts-sugar.utils.utils
  (:require [clojure.core.memoize :as memo]
            [clojure-potrubi.utils.names :as potrubi-utils-names :refer (resolve-name-from-keyword)]
            [clojure-potrubi.utils.atoms :as potrubi-utils-atoms]
            [clojure-potrubi.utils.collections :as potrubi-utils-collections]
            [clojure-carp :as carp :refer (surprise-exception)]))

;; Utilities for clojure-contracts-sugar

(def to-collection clojure-potrubi.utils.collections/to-collection)
(def to-vector clojure-potrubi.utils.collections/to-vector)
(def is-atom? clojure-potrubi.utils.atoms/is-atom? )

;; *********************
;; BEG: naming functions
;; *********************

(defn resolve-aspect-name-from-keyword
  [k]
  {:pre [(keyword? k)] :post [(string? %)]}
  (str (resolve-name-from-keyword k) "?"))

(defn resolve-name-from-any
  [any]
  {:post [(string? %)]}
  (cond
   (keyword? any) (resolve-name-from-keyword any)
   :else (str any)))

(defn resolve-aspect-symbol-from-keyword
  [x]
  {:pre [(keyword? x)] :post [(symbol? %)]}
  (symbol (resolve-aspect-name-from-keyword x)))

(defn make-arg-name
  [& args]
  (apply str (flatten args)))

(defn make-fn-arg-name-from-index
  [& arg-prefixes]
  {:post [(fn? %)]}
  (let [arg-prefix (make-arg-name arg-prefixes)
        fn-arg-name (fn [index]
                      {:pre [(number? index)] :post [(string? %)]}
                      (make-arg-name arg-prefix index))
        memo-fn-arg-name (memo/lu fn-arg-name :lu/threshold 20)]
    memo-fn-arg-name))

(defn make-fn-arg-symbol-from-index
  [fn-arg-name]
  {:pre [(fn? fn-arg-name)] :post [(fn? %)]}
  (let [fn-arg-symbol  (fn [index]
                         {:pre [(number? index)] :post [(symbol? %)]}
                         (symbol (fn-arg-name index)))
        memo-fn-arg-symbol (memo/lu fn-arg-symbol :lu/threshold 20)]
    memo-fn-arg-symbol))

;; (defn make-suck-arg-name-from-index
(def make-park-arg-name-from-index (make-fn-arg-name-from-index "park-arg"))

(def make-abs-suck-arg-name-from-index (make-fn-arg-name-from-index "abs-arg"))
(def make-rel-suck-arg-name-from-index (make-fn-arg-name-from-index "arg"))
(def make-fin-suck-arg-name-from-index (make-fn-arg-name-from-index "arg"))

(def make-abs-spit-arg-name-from-index (make-fn-arg-name-from-index "abs-arg"))
(def make-rel-spit-arg-name-from-index (make-fn-arg-name-from-index "arg"))
(defn make-fin-spit-arg-name-from-index [& any] "%")

(def make-park-arg-symbol-from-index (make-fn-arg-symbol-from-index make-park-arg-name-from-index))

(def make-rel-suck-arg-symbol-from-index (make-fn-arg-symbol-from-index make-rel-suck-arg-name-from-index))
(def make-abs-suck-arg-symbol-from-index (make-fn-arg-symbol-from-index make-abs-suck-arg-name-from-index))
(def make-fin-suck-arg-symbol-from-index (make-fn-arg-symbol-from-index make-fin-suck-arg-name-from-index))

(def make-rel-spit-arg-symbol-from-index (make-fn-arg-symbol-from-index make-rel-spit-arg-name-from-index))
(def make-abs-spit-arg-symbol-from-index (make-fn-arg-symbol-from-index make-abs-spit-arg-name-from-index))
(def make-fin-spit-arg-symbol-from-index (make-fn-arg-symbol-from-index make-fin-spit-arg-name-from-index))

(def argument-symbol-from-index-fn-map
  {:spit {:abs make-abs-spit-arg-symbol-from-index
          :rel make-rel-spit-arg-symbol-from-index
          :fin make-fin-spit-arg-symbol-from-index}

   :suck {:abs make-abs-suck-arg-symbol-from-index
          :rel make-rel-suck-arg-symbol-from-index
          :fin make-fin-suck-arg-symbol-from-index}})

(defn find-argument-symbol-from-index-fn
  [arg-type symbol-type]
  {:pre [(keyword? arg-type) (keyword? symbol-type)] :post [(fn? %)]}
  (get-in argument-symbol-from-index-fn-map [arg-type symbol-type]))

(defn make-fn-name-generator
  ([] (fn [x] (str x)))
  ([prefix] (make-fn-name-generator prefix nil))
  ([prefix suffix] (make-fn-name-generator prefix suffix "-"))
  ([prefix suffix separator]
     (if suffix
       (if prefix
         (fn [x] (str prefix separator x separator suffix))
         (fn [x] (str x separator suffix)))
       (fn [x] (str prefix separator x)))))

(defn make-fn-name-keyword-generator
  [& args]
  (let [fn-name (apply make-fn-name-generator args)]
    (fn [x] (keyword (fn-name (resolve-name-from-keyword x))))))

(defn resolve-keyword-aspect-names
  [fn-namer names]
  {:pre [(fn? fn-namer) (coll? names)]}
  (map (fn [token]
         (doall (println "TOKEN" (class token) token))
         (cond
          (keyword? token) (fn-namer token)
          :else token))
       names))

(defmacro define-fn-apply-predicate-to-collection
  [fn-name pred-fn]
  (let [apply-form# nil

        apply-form# `(def ~fn-name (fn [~'ts] {:pre [(coll? ~'ts)]} (every? ~pred-fn ~'ts)))]

    `(do
       ~apply-form#
       nil)))

;; *********************
;; FIN: naming functions
;; *********************

;; *************************
;; BEG: collection functions
;; *************************

(defn assoc-in-multiple
  [src-map & args]
  {:pre [(map? src-map)]}
  (reduce (fn [s [k v]] (assoc-in s k v)) src-map (partition 2 args)))

(defn select-maps-by-keys
  [maps & keys]
  {:pre [(coll? maps) (every? map? maps)]}
  (let [unique-keys (distinct (flatten keys))
        selected-maps (map (fn [map] (select-keys map unique-keys)) maps)]
    selected-maps))

;; *************************
;; FIN: collection functions
;; *************************
