(ns clojure-contracts-sugar.utils.walk-forms
  (:require [clojure.core.memoize :as memo]
            [clojure-carp :as carp :refer (surprise-exception )]
            [clojure.walk :as walkies]
            [clojure-contracts-sugar.utils.makros :as utils-makros :refer (define-fn-validation-from-predicate define-fn-apply-predicate-to-collection)]))

;; Utilities for clojure-contracts-sugar

;; ********************
;; BEG; range functions
;; ********************

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

;; ********************
;; FIN: range functions
;; ********************

;; *************************
;; BEG: validation functions
;; *************************

(defn is-walk-forms-symbol-map?
  [symbol-map]
  (and (map? symbol-map)
       (every? symbol? (keys symbol-map))
       (every? symbol? (vals symbol-map))))

(define-fn-apply-predicate-to-collection is-walk-forms-symbol-maps? is-walk-forms-symbol-map?)

(defn is-walk-forms-replace-map?
  [replace-map]
  (map? replace-map))

(define-fn-apply-predicate-to-collection is-walk-forms-replace-maps? is-walk-forms-replace-map?)

(defn is-walk-forms-keyed-replace-map?
  [keyed-replace-map]
  (and (map? keyed-replace-map)
       (every? is-walk-forms-replace-map? (vals keyed-replace-map))))

(define-fn-apply-predicate-to-collection is-walk-forms-keyed-replace-maps? is-walk-forms-keyed-replace-map?)

(defn is-walk-forms-form?
  [form]
  true)

(define-fn-apply-predicate-to-collection is-walk-forms-forms? is-walk-forms-form?)

(defn is-walk-forms-keyed-form?
  [form]
  (and (map? form)
        (is-walk-forms-forms? (vals form))))

(define-fn-apply-predicate-to-collection is-walk-forms-keyed-forms? is-walk-forms-keyed-form?)

;; *************************
;; FIN: validation functions
;; *************************

;; ******************************
;; BEG: argument symbol functions
;; ******************************

(defn make-walk-forms-symbol-map
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

(def make-walk-forms-symbol-map (memo/lu make-walk-forms-symbol-map :lu/threshold 20))

;; ******************************
;; FIN: argumnet symbol functions
;; ******************************

;; ***************
;; BEG: walk forms
;; ***************

(defn normalise-walk-forms-keyed-replace-maps
  [keyed-replace-maps]
  {:post [(is-walk-forms-keyed-replace-maps? %)]}
  (let [normal-keyed-replace-maps (cond
                             (map? keyed-replace-maps) (list keyed-replace-maps)
                             (coll? keyed-replace-maps) keyed-replace-maps
                             :else (surprise-exception keyed-replace-maps "normalise-walk-forms-keyed-replace-maps" "keyed-replace-maps is wat?"))]
    normal-keyed-replace-maps))

(defn- walk-keyed-forms-with-keyed-replace-map
  "walks a collection of keyed source form maps and
   applied the value of each key in keyed-replace-map to the matching key in each source map"
  [keyed-replace-map keyed-source-forms]
  {:pre [(is-walk-forms-keyed-replace-map? keyed-replace-map) (is-walk-forms-keyed-forms? keyed-source-forms)]
   :post [(is-walk-forms-keyed-forms? %)]}
  (let [keyed-walked-forms
        (into []
              (map-indexed
               (fn [source-index source-form-map]
                 (reduce
                  (fn [s [map-key map-form]]
                    ;; reduce the map-form using all the normal-keyed-replace-maps

                    (if-let [map-key-replace-map (get keyed-replace-map map-key)]
                      (assoc s map-key (walkies/prewalk-replace map-key-replace-map map-form))
                      s))

                  {}
                  source-form-map))

               keyed-source-forms))]
    keyed-walked-forms))

(defn walk-keyed-forms
  "walks a collection of keyed source form maps and
   applied the value of each key in keyed-replace-maps to the matching key in each source map"
  [keyed-replace-maps keyed-source-forms]
  {:pre [(is-walk-forms-keyed-forms? keyed-source-forms)] :post [(is-walk-forms-keyed-forms? %)]}
  (let [normal-keyed-replace-maps (normalise-walk-forms-keyed-replace-maps keyed-replace-maps)

        keyed-walked-forms
        (into []
              (map-indexed
               (fn [source-index source-form-map]
                 (reduce
                  (fn [s [map-key map-form]]
                    ;; reduce the map-form using all the normal-keyed-replace-maps
                    (assoc s map-key
                           (reduce
                            (fn [t keyed-replace-map]
                              (if-let [map-key-replace-map (get keyed-replace-map map-key)]
                                (walkies/prewalk-replace map-key-replace-map t)

                                ;; #_(let [;;       ;; walk-value t
                                ;;       ]
                                ;;   new-t)

                                t))
                            map-form
                            normal-keyed-replace-maps)))

                  {}
                  source-form-map))

               keyed-source-forms))]
    keyed-walked-forms))

(defn normalise-walk-forms-replace-maps
  [replace-maps]
  {:post [(is-walk-forms-replace-maps? %)]}
  (let [normal-replace-maps (cond
                             (map? replace-maps) (list replace-maps)
                             (coll? replace-maps) replace-maps
                             :else (surprise-exception replace-maps "normalise-walk-forms-replace-maps" "replace-maps is wat?"))]
    normal-replace-maps))

(defn walk-forms
  "walks a collection of source forms and
   applied each replace-map"
  [replace-maps source-forms]
  {:pre [
         (is-walk-forms-forms? source-forms)]
   :post [
          (is-walk-forms-forms? %)]
}
  (let [normal-replace-maps (normalise-walk-forms-replace-maps replace-maps)

        ;; (cond
        ;;                     (coll? replace-maps) replace-maps

        walked-forms
        (map
         (fn [form]
           (reduce
            (fn [walked-form replace-map] (walkies/prewalk-replace replace-map walked-form))
            form
            normal-replace-maps))
         source-forms)]
    walked-forms))

;; ***************
;; FIN: walk forms
;; ***************

;; **********************
;; BEG: packing functions
;; **********************

(defn build-symbol-maps-and-walk-forms
  [source-forms symbol-max src-offset tgt-offset fn-src-symbol fn-tgt-symbol]
  {:post [(map? %) (every? keyword? (keys %))]}
  (let [symbol-map (make-walk-forms-symbol-map symbol-max src-offset tgt-offset fn-src-symbol fn-tgt-symbol)
        walked-forms (walk-forms [symbol-map] source-forms)

        return-state {:source-forms source-forms
                      :walked-forms walked-forms
                      :symbol-map symbol-map}]

    return-state))

(def build-symbol-maps-and-walk-forms (memo/lu build-symbol-maps-and-walk-forms :lu/threshold 20))

;; **********************
;; FIN: packing functions
;; **********************


