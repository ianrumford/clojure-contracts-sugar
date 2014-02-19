(ns clojure-contracts-sugar.aspects.forms
  (:require [clojure-contracts-sugar.manifests :as manifests
             :refer (manifest-aspect-form-key-suck
                     manifest-aspect-form-key-spit
                     manifest-aspect-form-keys
                     manifest-aspect-assertion-types
                     manifest-aspect-argument-types)]

            [clojure-contracts-sugar.utils.utils :as utils
             :refer (to-vector
                     to-collection
                     find-argument-symbol-from-index-fn
                     memo-build-symbol-maps-and-walk-forms)]

            [clojure-contracts-sugar.utils.wrapped-functions :as utils-wrapped
             :refer (wrap-functions
                     wrap-functions-with-template)]

            [clojure-carp :as carp :refer (surprise-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]
            [clojure.walk :as walkies]))

;; *************************
;; BEG: validate aspect form
;; *************************

(defn is-aspect-form-vector-form?
  "check if this definition is a clojure.core.contract form
   e.g. [[v] [map?]]"
  [aspect-form]
  (let [is-vector-form
        (and (vector? aspect-form)
             (= 2 (count aspect-form))
             (vector? (first aspect-form))
             (vector? (second aspect-form)))]
    is-vector-form))

;; the map form
(defn is-aspect-form?
  [aspect-form]
  (let [is-aspect-form
        (and
         (map? aspect-form)
         (every? manifest-aspect-form-keys (keys aspect-form))
         (every? true? (for [[form-type form-value]  aspect-form]
                  (do
                    (and (map? form-value)
                         (every? number? (keys form-value))
                         (every? vector? (vals form-value)))))))]
    (if is-aspect-form aspect-form)))

(defn validate-aspect-form
  [aspect-form]
  {:pre [(is-aspect-form? aspect-form)]}
  aspect-form)

;; *************************
;; FIN: validate aspect form
;; *************************

;; *******************************
;; BEG: rewrite aspect form vector
;; *******************************

(defn rewrite-aspect-form-vector
  ([aspect-form] (rewrite-aspect-form-vector aspect-form identity))
  ([aspect-form fn-arg-index] (rewrite-aspect-form-vector aspect-form fn-arg-index utils/make-rel-suck-arg-symbol-from-index))
  ([aspect-form fn-arg-index fn-arg-symbol]
      {:pre [(fn? fn-arg-index) (fn? fn-arg-symbol) (is-aspect-form-vector-form? aspect-form)  ]
       :post [(is-aspect-form-vector-form? aspect-form)]}
      (let [[form-signature form-assertions]  aspect-form

            _ (assert (coll? form-assertions))

            ;; need to re-write any collection assertions to use "normalised" argument name
            ;; e.g. swap [v] for [argo]

            updated-aspect-form
            (if-not (some coll? form-assertions)
              aspect-form
              (let [signature-map
                    (into {}
                          (map-indexed
                           (fn [ndx arg-name] [arg-name (fn-arg-symbol (fn-arg-index ndx))])
                           form-signature))

                    walked-assertions
                    (map (fn [assertion]
                           [(if (list? assertion)
                               (walkies/prewalk-replace signature-map assertion)
                               assertion)])
                         form-assertions)

                    ;; how many args positions have been shuffled up?
                    missing-argument-max (fn-arg-index 0) ;; +1 cos range is exclusive of end
                    missing-arguments (into {} (map
                                                (fn [ndx]
                                                  (let [arg-symbol (fn-arg-symbol ndx)
                                                        ;;arg-assertion [ (list 'true? arg-symbol)]
                                                        ;;arg-assertion [ (list )]
                                                        arg-assertion [nil]]

                                                    [arg-symbol arg-assertion]))
                                                (range missing-argument-max)))
                    all-assertions (into [] (apply concat (concat (vals missing-arguments) walked-assertions)))

                    all-signature (into [] (sort (concat (keys missing-arguments) (vals signature-map))))

                    updated-aspect-form [all-signature all-assertions]]

                updated-aspect-form))]
        updated-aspect-form)))

;; *******************************
;; FIN: rewrite aspect form vector
;; *******************************

;; ************************************
;; BEG: convert vector form to map form
;; ************************************

(defn create-aspect-form-map-entry
  ([form-type form-signature form-assertions] (create-aspect-form-map-entry form-type form-signature form-assertions identity utils/make-rel-suck-arg-symbol-from-index))
  ([form-type form-signature form-assertions fn-arg-index fn-arg-symbol]
     {:pre [(keyword? form-type) (vector? form-signature) (coll? form-assertions) (fn? fn-arg-index) (fn? fn-arg-symbol)] :post [(map? %)]}
     (let [signature-map (if (some coll? form-assertions)
                           (apply array-map (apply concat
                                                   (map-indexed
                                                    (fn [ndx arg-name] [arg-name (fn-arg-symbol (fn-arg-index ndx))])
                                                    form-signature))))

           ;; a map of the signature symbol v its index in the signature
           signature-keys (into {} (map-indexed (fn [ndx key] [key ndx]) (keys signature-map)))

           ;; wanted-assertions (into [] (for [form-assertion form-assertions]
           ;;                              form-assertions))

           wanted-assertions (remove nil? form-assertions)

           updated-assertions
           (let [sorted-assertions
                 (map-indexed
                  (fn [form-index form-assertion]
                    (let [mapped-assertion
                          (cond
                           (or (keyword? form-assertion) (symbol? form-assertion)) [form-index form-assertion]

                           ;; need to associate this assertion with the
                           ;; last argument (index) used in the assertion form
                           ;; e.g. (= x y) associates to argument y
                           (list? form-assertion)
                           (let [argument-index
                                 (reduce
                                  (fn [s [old-sym new-sym]]
                                    (let [walked-assertion (walkies/prewalk-replace {old-sym 99} form-assertion)
                                          sym-found (not= form-assertion walked-assertion)

                                          last-sym (if sym-found
                                                     (get signature-keys old-sym)
                                                     s)]
                                      last-sym))
                                  -1
                                  signature-map)

                                 ;; need to changes to normalised arg names
                                 walked-assertion (walkies/prewalk-replace signature-map form-assertion)]
                             [argument-index walked-assertion])

                           :else (surprise-exception form-assertion "create-aspect-form-map-entry"  "FORM-TYPE" form-type "FORM-INDEX" form-index "form-assertion is wat?"))]
                      mapped-assertion
                      ))

                  wanted-assertions)

                 grouped-assertions (group-by (fn [a] (first a)) sorted-assertions)

                 reduced-assertions (into {}
                                          (map
                                           (fn [[grouped-index grouped-index-assertions]]
                                             (let [reduced-index-assertions (into [] (map (fn [a] (second a)) grouped-index-assertions))]
                                               [grouped-index reduced-index-assertions]))
                                           grouped-assertions))]

             reduced-assertions)

           aspect-form {form-type updated-assertions}]
       aspect-form)))

(defn partition-aspect-form-vector
  [aspect-form]
  {:pre [(is-aspect-form-vector-form? aspect-form) ] :post [(map? %)]}
  (let [aspect-assertions (partition-by #(= % '=>) (second aspect-form))

        aspect-assertions-size (count aspect-assertions)

        partition-form
        (condp = aspect-assertions-size

          1 {manifest-aspect-form-key-suck (first aspect-assertions)}

          3 {manifest-aspect-form-key-suck (nth aspect-assertions 0)
             manifest-aspect-form-key-spit (nth aspect-assertions 2)}

          ;; need to disambiguate trailing '=> from all spits
          2 (let [form
                  (let [first-element (ffirst aspect-assertions)]
                    (if (= first-element '=>)
                      {manifest-aspect-form-key-spit (nth aspect-assertions 1)}
                      {manifest-aspect-form-key-suck (nth aspect-assertions 0)}))]
              form)

          (surprise-exception aspect-assertions "partition-aspect-form-vector" "aspect-assertions is wat?"))]

    partition-form))

(defn convert-aspect-form-vector-to-map
  ([aspect-form-vector] (convert-aspect-form-vector-to-map aspect-form-vector identity utils/make-rel-suck-arg-symbol-from-index))
  ([aspect-form-vector fn-arg-index fn-arg-symbol]
      {:pre [(is-aspect-form-vector-form? aspect-form-vector) (fn? fn-arg-index) (fn? fn-arg-symbol)] :post [(is-aspect-form? %)]}
      (let [aspect-signature (first aspect-form-vector)

            partition-form (partition-aspect-form-vector aspect-form-vector)

            aspect-form-map
            (apply merge
                   (map
                    (fn [[form-type form-assertions]]
                      (create-aspect-form-map-entry form-type  aspect-signature form-assertions fn-arg-index fn-arg-symbol))
                    partition-form))]

        aspect-form-map
        )))

;; ************************************
;; FIN: convert vector form to map form
;; ************************************

;; **************************
;; BEG: normalise aspect form
;; **************************

(defn aspect-form-type
  [aspect-definition]
  {:post [(keyword? %)]}
  (cond
   (map? aspect-definition) :map
   (vector? aspect-definition) :vector
   ;; (symbol? aspect-definition) :symbol
   ;; (keyword? aspect-definition) :keyword
   :else :default))

(defmulti normalise-aspect-form aspect-form-type)

(defmethod normalise-aspect-form :vector
  [nominal-form]
  {:pre [(vector? nominal-form)] :post [(is-aspect-form? %)]}
  (let [aspect-form (convert-aspect-form-vector-to-map nominal-form)]
    aspect-form))

(defn normalise-aspect-form-value
  [form-type form-index form-value]
  {:pre [(number? form-index) (keyword? form-type)] :post [(map? %) (every? number? (keys %)) (every? vector? (vals %))]}
  (let [normal-form (cond

                     ;; pre-built arguments definitions
                     ;; need to rewrite to correct offset though
                     (map? form-value)
                     (let [_ (assert (every? number? (keys form-value)))

                           max-index (apply max (list* 8  (keys form-value)))
                           fn-src-symbol (find-argument-symbol-from-index-fn form-type :rel)
                           fn-tgt-symbol fn-src-symbol

                           normalised-map-value (into {} (map
                                                          (fn [[k v]]
                                                            (let [source-form (to-collection v)

                                                                  walked-form (if (> k 0)
                                                                                (let [walked-map (memo-build-symbol-maps-and-walk-forms
                                                                                                  [source-form]
                                                                                                  max-index
                                                                                                  0
                                                                                                  k
                                                                                                  fn-src-symbol
                                                                                                  fn-tgt-symbol)

                                                                                      walked-form (first (get walked-map :walked-forms))]
                                                                                  walked-form)
                                                                                source-form)]
                                                              [k (into [] walked-form)]))
                                                          form-value))]
                       normalised-map-value)

                     (is-aspect-form-vector-form? form-value)
                     (let [;; need to rewrite the form to offset to this form index
                           rewritten-value (rewrite-aspect-form-vector form-value (fn [ndx] (+ form-index ndx)))
                           map-form (convert-aspect-form-vector-to-map  rewritten-value)

                           map-form-value (get map-form form-type)
                           shifted-value map-form-value]

                       shifted-value)

                     (vector? form-value) {form-index form-value}

                     (or (keyword? form-value) (symbol? form-value) (coll? form-value)) {form-index [form-value]}

                     :else (surprise-exception form-value "normalise-aspect-form-value" "FORM-TYPE" form-type "FORM-INDEX" form-index "form-value is wat?"))]

    normal-form))

(defmethod normalise-aspect-form :map
  [nominal-form]
  {:pre [(map? nominal-form)] :post [(is-aspect-form? %)]}
  (let [aspect-keys manifest-aspect-form-keys

        aspect-form (into {}
                          (map-indexed
                           (fn [form-index form-type]
                             (let [form-value (get nominal-form form-type)

                                   form-update
                                   (if form-value
                                     (let [form-list (if (map? form-value)
                                                       [form-value]
                                                       (to-collection form-value))

                                           normal-list (apply merge-with
                                                              ;;concat
                                                              (fn [x y] (into [] (concat x y)))
                                                              (map-indexed
                                                               (fn [entry-index entry-value]
                                                                 (normalise-aspect-form-value form-type entry-index  entry-value))
                                                               form-list))]
                                       normal-list))]
                               (if form-update
                                 [form-type form-update])))

                           aspect-keys))]

    aspect-form))

(defmethod normalise-aspect-form :default
  [aspect-form]
  (surprise-exception aspect-form "normalise-aspect-form" "DEFAULT" "aspect-form is what?"))

;; **************************
;; FIN: normalise aspect form
;; **************************

;; ************************
;; BEG: select aspect forms
;; ************************

(defn select-aspect-forms-by-keys
  "select the passed aspect forms by keys using select-keys"
  [aspect-forms & keynames]
  {:pre [(coll? aspect-forms) (every? is-aspect-form? aspect-forms)] :post [(or (nil? %) (coll? %))]}
  (let [flatten-keys (flatten keynames)
        nominal-forms (map (fn [form] (select-keys form flatten-keys)) aspect-forms)
        selected-forms (filter not-empty nominal-forms)]
    selected-forms))

;; ************************
;; FIN: select aspect forms
;; ************************

;; *************
;; BEG:  memoize
;; *************

(wrap-functions
  is-aspect-form-vector-form?
  is-aspect-form?
  create-aspect-form-map-entry
  normalise-aspect-form-value
  )

;; *************
;; FIN:  memoize
;; *************

