(ns clojure-contracts-sugar.utils.utils
  (:require [clojure.core.memoize :as memo]
            [clojure-carp :as carp :refer (surprise-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]
            [clojure.walk :as walkies]))

;; Utilities for clojure-contracts-sugar

;; *******************
;; BEG: atom functions
;; *******************

(defn atom?
  [a] (instance? clojure.lang.Atom a))

(defn make-fn-find-atom
  ([] (make-fn-find-atom (atom nil)))
  ([value]
       {:pre [(atom? value)] :post [(fn? %)]}
       (fn [] value)))

;; *******************
;; FIN: atom functions
;; *******************

;; *************************
;; BEG: naming functions
;; *************************

(defn resolve-name-from-keyword
  [k]
  {:pre [(keyword? k)] :post [(string? %)]}
  (-> k str (subs 1)))

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

;; *************************
;; FIN: conversion functions
;; *************************

;; *********************
;; BEG: naming functions
;; *********************

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
(defn make-rel-spit-arg-name-from-index [& any] "arg")
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

;; *********************
;; FIN: naming functions
;; *********************

;; *************************
;; BEG: collection functions
;; *************************

(defn to-collection
  [any]
  (cond
   (coll? any) any
   :else (list any)))

(defn to-vector
  [any]
  (cond
   (vector? any) any
   :else [any])
)

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

;; ***************
;; BEG: walk forms
;; ***************

(defn make-begin-finish-number-range
  "make a range of numbers starting from number-bege
  through  number-fin"
  [number-beg number-fin]
  {:pre [(number? number-fin) (>= number-beg 0)
         (number? number-fin) (>= number-fin 0)
         (>= number-fin number-beg )]
   :post [(coll? %) (every? number? %)]}
  (range number-beg (+ 1 number-fin)))

(defn make-begin-count-number-range
  ([number-count] (make-begin-count-number-range 0 number-count))
  ([number-beg number-count]
     {:pre [(number? number-beg) (number? number-count)]}
     (make-begin-finish-number-range number-beg (- (+ number-beg number-count) 1))))

(defn make-walk-argument-symbol-map
  "creates an array-map of argument name mapping e.g. abs-arg0 => arg5
   NOTE the argument-range starts at the HIGHEST to shift rightmost first"
  [arg-no src-offset tgt-offset fn-src-arg fn-tgt-arg]
  {:pre [(number? arg-no)
         (number? src-offset)
         (number? tgt-offset)
         (fn? fn-src-arg)
         (fn? fn-tgt-arg)]
   :post [(map? %) (every? symbol? (keys %)) (every? symbol? (vals %))]}

  (let [argument-range (make-begin-count-number-range arg-no)

        argument-map (apply array-map
                           (apply concat
                            (map
                             (fn [arg-no] [(fn-src-arg (+ src-offset arg-no)) (fn-tgt-arg (+ tgt-offset arg-no))])
                             (reverse argument-range))))]

    argument-map))

(def make-walk-argument-symbol-map (memo/lu make-walk-argument-symbol-map :lu/threshold 20))

(defn walk-map-forms
  "walks a collection of source maps and
   applied the the value of each key in symbol-map to the matching key in the source map"
  [symbol-map source-maps]
  {:pre [(map? symbol-map)
         (every? symbol? (apply concat (apply concat (vals symbol-map))) )
         (every? map? (vals symbol-map))
         (coll? source-maps)]
   :post [(coll? %) (every? map? %)]}
  (let [walked-forms
        (into []
              (map-indexed
               (fn [source-index source-map]
                 (reduce
                  (fn [s [map-key map-form]]
                    (if-let [sig-map (get symbol-map map-key)]
                      (assoc s map-key (walkies/prewalk-replace sig-map map-form))
                      s))
                  {}
                  source-map))

               source-maps))]
    walked-forms))

(defn walk-forms
  "walks a collection of source form and
   applied each symbol-map"
  [symbol-maps source-forms]
  {:pre [
         (coll? symbol-maps)
         (every? map? symbol-maps)
         (coll? source-forms)]
   :post [(coll? %)]}
  (let [walked-forms
        (map
         (fn [form]
           (reduce
            (fn [walked-form symbol-map] (walkies/prewalk-replace symbol-map walked-form))
            form
            symbol-maps))
         source-forms)]
    walked-forms))

;; ***************
;; FIN: walk forms
;; ***************

(defn build-symbol-maps-and-walk-forms
  [source-forms symbol-max src-offset tgt-offset fn-src-symbol fn-tgt-symbol]
  {:post [(map? %) (every? keyword? (keys %))]}
  (let [symbol-map (make-walk-argument-symbol-map symbol-max src-offset tgt-offset fn-src-symbol fn-tgt-symbol)
        walked-forms (walk-forms [symbol-map] source-forms)

        return-state {:source-forms source-forms
                      :walked-forms walked-forms
                      :symbol-map symbol-map}]

    return-state))

(def memo-build-symbol-maps-and-walk-forms (memo/lu build-symbol-maps-and-walk-forms :lu/threshold 20))

