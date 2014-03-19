(ns clojure-contracts-sugar.utils.makros
  (:require [clojure-potrubi.utils.names :as potrubi-utils-names :refer (resolve-symbol-from-args)]
            [clojure-carp :as carp :refer (surprise-exception)])
)

;; Utilities for clojure-contracts-sugar

;; ******************
;; BEG: fn generation
;; ******************

(defn form-fn-from-name-and-arities-splat
  [fn-name & arities-splat]
  {:pre [(even? (count arities-splat))]}
  (let [;;fn-name# (symbol "self")  ;; needed to self-refer
        fn-name# fn-name

        sig-body-forms#
        (for [[fn-sig fn-body] (partition 2 arities-splat)]
          (let [fn-sig-norm# (cond
                              (vector? fn-sig) fn-sig
                              :else [fn-sig])]
            (list fn-sig-norm# fn-body)))

        fn-form# (if fn-name#
                   `(fn ~fn-name# ~@sig-body-forms#)
                   `(fn ~@sig-body-forms#))]

    fn-form#))

(defn form-fn-from-name-and-arities-collection
  [fn-name arities-collection]
  {:pre [(coll? arities-collection)]}
  (let [fn-form# (apply form-fn-from-name-and-arities-splat fn-name arities-collection)]
    fn-form#))

(defn form-fn-from-name-and-arities-map
  [fn-name fn-arities]
  {:pre [(map? fn-arities)]}
  (let [fn-form# (apply form-fn-from-name-and-arities-splat fn-name (apply concat fn-arities))]
    fn-form#))

(defmacro define-fn-from-name-and-arities-map
  [fn-name fn-arities]
  (let [fn-form# (form-fn-from-name-and-arities-map fn-name fn-arities)]
    `(do
       ~fn-form#)))

(defn form-fn-from-name-and-body
  [fn-name fn-body]
  (let [define-form# `(def ~fn-name ~fn-body)]
    define-form#))

(defmacro define-fn-from-name-and-body
  [fn-name fn-body]
  (let [define-form# (form-fn-from-name-and-body fn-name fn-body)]
    `(do
       ~define-form#
       nil)))

;; ******************
;; FIN: fn generation
;; ******************

;; ****************************
;; BEG: is-something? functions
;; ****************************

(defmacro define-fn-apply-predicate-to-collection
  [fn-name pred-fn]
  (let [apply-form# nil

        apply-form# `(def ~fn-name (fn [~'ts] {:pre [(coll? ~'ts)]} (every? ~pred-fn ~'ts)))]

    `(do
       ~apply-form#
       nil)))

;; ****************************
;; FIN: is-something? functions
;; ****************************

;; ************************
;; BEG: validation function
;; ************************

(defn form-fn-validation-from-predicate
  [pred-fn]
  (let [make-form# `(fn [~'pred-value] (assert (~pred-fn ~'pred-value)))]
    make-form#))

(defmacro define-fn-validation-from-predicate
  [fn-name pred-fn]
  (let [define-form# nil

        define-form# `(def ~fn-name (form-fn-validation-from-predicate ~pred-fn))]

    `(do
       ~define-form#
       nil)))

;; ************************
;; FIN: validation function
;; ************************

;; ************************
;; BEG: try catch functions
;; ************************

(defn- make-form-try-catch-body
  ([body-try body-catch] (make-form-try-catch-body body-try body-catch 'AssertionError))
  ([body-try body-catch assertion-name]
     (let [;; the body for the telltale fn
           assertion-form
           `(try
              ~body-try
              (catch ~assertion-name  ~'e
                (do
                  ~body-catch
                  (throw ~'e))))]

       assertion-form)))

(defn form-fn-try-catch-wrapper
  [ & {:keys [wrapper-name target-form target-name target-arities] :as opt-args}]
  {:pre [
         (or (nil? wrapper-name) (symbol? wrapper-name))
         (map? target-arities) (every? vector? (keys target-arities)) (every? coll? (vals target-arities))]
   :post [(coll? %) (every? coll? %)]}

  (let [[target-fn target-def]
        (cond
         (symbol? target-form) [target-form]
         (coll? target-form) (let [target-fn-symbol (if target-name (resolve-symbol-from-args target-name) (gensym "target-fn-try-catch-"))
                                   target-fn-def `(def ~target-fn-symbol ~target-form)]
                               [target-fn-symbol target-fn-def ])
         :else (surprise-exception target-form "form-fn-try-catch-wrapper" "target-form is wat?"))

        wrapper-sig-body-forms (apply concat
                              (map-indexed
                               (fn [ndx [target-sig catch-body]]

                                 (let [try-body (list* target-fn target-sig)
                                       try-catch-body (make-form-try-catch-body try-body catch-body)]
                                   [target-sig try-catch-body]))

                               target-arities))
        wrapper-form (form-fn-from-name-and-arities-collection wrapper-name wrapper-sig-body-forms)

        try-catch-forms (list target-def wrapper-form)]
    try-catch-forms))

;; ************************
;; FIN: try catch functions
;; ************************


