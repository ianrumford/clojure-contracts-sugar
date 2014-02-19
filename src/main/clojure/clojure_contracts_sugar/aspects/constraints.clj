(ns clojure-contracts-sugar.aspects.constraints
  (:require [clojure-contracts-sugar.manifests :as manifests
             :refer (
                     manifest-aspect-argument-type-spit
                     manifest-aspect-argument-type-suck
                     manifest-aspect-argument-types

                     manifest-aspect-assertion-types)]
            
            [clojure-contracts-sugar.utils.utils :as utils
             :refer (to-vector
                     to-collection
                     walk-forms
                     make-fin-suck-arg-symbol-from-index
                     make-fin-spit-arg-symbol-from-index)]
            

            [clojure-contracts-sugar.utils.wrapped-functions :as utils-wrapped
             :refer (wrap-functions
                     wrap-functions-with-template)]

            [clojure-carp :as carp :refer (surprise-exception missing-exception duplicate-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]
            [clojure.core.memoize :as memo]))

;; Aspect Constraints for clojure-contracts-sugar


;; *********************
;; BEG: aspect assertion
;; *********************

(defprotocol AspectAssertionProtocol
  (aspect-assertion-form [this])
  (aspect-assertion-update [this update-fn]))

(defrecord AspectAssertion [form]
  Object
  (toString [t] (str "AspAst(" (:form t) ")")))

(defn is-aspect-assertion? [t] (instance? AspectAssertion t))
(defn is-aspect-assertions?
  [ts]
  {:pre [(coll? ts)]}
  (let [all-valid (every? is-aspect-assertion? ts)]
    (if all-valid ts)))

(defn aspect-assertion-factory
  [form]
  {:pre [(or (symbol? form) (seq? form))]
   :post [(instance? AspectAssertion %)]}
  (let [aspect-assertion (->AspectAssertion form)]
    aspect-assertion))

(defn aspect-assertion-factory-multiple
  [& forms]
  (map (fn [form] (aspect-assertion-factory form)) forms))

(def aspect-assertion-base-fns
  {

   :aspect-assertion-form (fn [t] (:form t))

   ;; update applies a fn and returns a new assertion
   :aspect-assertion-update
   (fn [t update-fn]
     {:pre [(fn? update-fn)] :post [(is-aspect-assertion? %)]}
     (let [updated-assertion (update-fn t)]
       updated-assertion))

   })

(extend AspectAssertion AspectAssertionProtocol aspect-assertion-base-fns)

;; *********************
;; FIN: aspect assertion
;; *********************

;; ********************
;; BEG: aspect argument
;; ********************

(declare aspect-argument-factory)

(defprotocol AspectArgumentProtocol
  (aspect-argument-add-assertion [this assertion])
  (aspect-argument-add-assertions [this assertions])
  (aspect-argument-index-symbol [this])
  (aspect-argument-index [this])
  (aspect-argument-assertions [this])
  (aspect-argument-new [this index-value assertions])
  (aspect-argument-assert-same-type [this other])
  (aspect-argument-is-same-type? [this other])
  (aspect-argument-express-assertions [this])
  (aspect-argument-type [this])
  (aspect-argument-map-assertions [this map-fn])
  (aspect-argument-filter-assertions [this predicate-fn])
  (aspect-argument-update [this update-fn])
  (say-hello [t]))

(defrecord AspectArgumentSuck [argument-index argument-assertions]
  Object
  (toString [t]
    (let [assertions (:argument-assertions t)
          assertion-values (map #(aspect-assertion-form %1) assertions)
          assertion-values-strings (apply str (interpose "-" assertion-values))
          argument-string (format "AspArgSuck(%d/%d-%s)" (:argument-index t) (count assertions) assertion-values-strings )]
      argument-string)))

(defrecord AspectArgumentSpit [argument-index argument-assertions]
  Object
  (toString [t]
    (let [assertions (:argument-assertions t)
          assertion-values (map #(aspect-assertion-form %1) assertions)
          assertion-values-strings (apply str (interpose "-" assertion-values))
          argument-string (format "AspArgSpit(%d/%d-%s)" (:argument-index t) (count assertions) assertion-values-strings )]
      argument-string)))

;; set lookup
(defn is-aspect-argument-type? [argument-type] (manifest-aspect-argument-types argument-type))
(defn is-aspect-argument-types?
  [ts]
  {:pre [(coll? ts)]}
  (if (every? is-aspect-argument-type? ts) ts))

(defn is-aspect-argument-index? [argument-index]
  {:pre [(number? argument-index) (>= argument-index 0)]}
  argument-index)

(defn is-aspect-argument? [t] (satisfies? AspectArgumentProtocol t))
(defn is-aspect-arguments?
  [ts]
  {:pre [(coll? ts)]}
  (let [all-valid (every? is-aspect-argument? ts)]
    (if all-valid ts)))

(defn is-suck-aspect-argument? [t] (instance? AspectArgumentSuck t))
(defn is-spit-aspect-argument? [t] (instance? AspectArgumentSpit t))

(defn assert-aspect-argument [t] (assert (is-aspect-argument? t)))
(defn assert-suck-aspect-argument [t] (assert (is-suck-aspect-argument? t)))
(defn assert-spit-aspect-argument [t] (assert (is-spit-aspect-argument? t)))

(def aspect-argument-base-fns
  {

   :aspect-argument-add-assertion (fn [t a] (aspect-argument-add-assertions t [a]))

   :aspect-argument-add-assertions  (fn [t a]
                                      {:pre [(coll? a) (every? is-aspect-assertion?  a)]}
                                      (aspect-argument-new t (:argument-index t) (into [] (concat (:argument-assertions t) a))))

   :aspect-argument-is-same-type? (fn [t o]
                                    (= (class t) (class 0)))

   :aspect-argument-assert-same-type (fn [t o] (assert (aspect-argument-is-same-type? t o)))

   :aspect-argument-index (fn [t] (:argument-index t))
   :aspect-argument-assertions (fn [t] (:argument-assertions t))

   :aspect-argument-express-assertions
   (memo/lu
    (fn [t]
      {:post [(coll? %) (every? coll? %)]}
      (let [;;;;;arg-index (:argument-index t)
            arg-symbol (aspect-argument-index-symbol t)
            arg-assertions (aspect-argument-assertions t)

            expressed-assertions (into []
                                       (map
                                        (fn [assertion]
                                          (let [form (aspect-assertion-form assertion)]
                                            (cond
                                             (symbol? form) (list form arg-symbol)
                                             :else form)))
                                        arg-assertions))]
        expressed-assertions))
    :lu/threshold 20)

   ;; map returns a collection of the assertions
   :aspect-argument-map-assertions
   (fn [t map-fn]
     {:pre [(fn? map-fn)] :post [(is-aspect-assertions? %)]}
     (let [maped-assertions (map map-fn (aspect-argument-assertions t))]
       maped-assertions))

   ;; filter returns a collection of the assertions
   :aspect-argument-filter-assertions
   (fn [t predicate-fn]
     {:pre [(fn? predicate-fn)] :post [(is-aspect-assertions? %)]}
     (let [filtered-assertions (filter predicate-fn (aspect-argument-assertions t))]
       filtered-assertions))

   ;; update applies a fn and returns a new argument
   :aspect-argument-update
   (fn [t update-fn]
     {:pre [(fn? update-fn)] :post [(is-aspect-argument? %)]}
     (let [updated-argument (update-fn t)]
       updated-argument))

   }

  )

(def suck-fns-specific-aspect-argument
  {
   :aspect-argument-index-symbol (fn [t] (make-fin-suck-arg-symbol-from-index (:argument-index t)))

   :aspect-argument-new (fn [t argument-index assertions] (AspectArgumentSuck. argument-index assertions))
   :aspect-argument-type (fn [t] manifest-aspect-argument-type-suck)
   }

  )

(def spit-fns-specific-aspect-argument
  {
   :aspect-argument-index-symbol (fn [t] (make-fin-spit-arg-symbol-from-index (:argument-index t)))

   :aspect-argument-new (fn [t argument-index assertions] (AspectArgumentSpit. argument-index assertions))
   :aspect-argument-type (fn [t] manifest-aspect-argument-type-spit)
   }

  )

(def aspect-argument-suck-fns (merge  aspect-argument-base-fns suck-fns-specific-aspect-argument))
(def aspect-argument-spit-fns (merge  aspect-argument-base-fns spit-fns-specific-aspect-argument))

(extend AspectArgumentSuck AspectArgumentProtocol aspect-argument-suck-fns)
(extend AspectArgumentSpit AspectArgumentProtocol aspect-argument-spit-fns)

(defn aspect-argument-factory
  [argument-type argument-index argument-assertions]
  {:pre [(keyword? argument-type) (number? argument-index) (coll? argument-assertions) (every? is-aspect-assertion? argument-assertions)] :post [(is-aspect-argument? %)]}
  (let [aspect-argument
        (cond
         (= :suck argument-type) (->AspectArgumentSuck argument-index (into [] argument-assertions))
         (= :spit argument-type) (->AspectArgumentSpit argument-index (into [] argument-assertions))
         :else (carp/surprise-exception argument-type "aspect-argument-factory" "argument-type not :suck or :spit"))]
    aspect-argument))

(defn aspect-argument-merge
  [& aspect-arguments]
  {:pre [(is-aspect-arguments? aspect-arguments)] :post [(is-aspect-argument? %)]}
  (let [aspect-argument
        (if (= 1 (count aspect-arguments))
          (first aspect-arguments)
          (let [argument-first (first aspect-arguments)
                argument-type (aspect-argument-type argument-first)
                argument-index (aspect-argument-index argument-first)

                ;; just check same type; not index
                _ (for [aspect-argument aspect-arguments]
                    (aspect-argument-assert-same-type argument-first aspect-argument))

                ;; make assertions distinct
                aspect-assertions (distinct (apply concat (map #(aspect-argument-assertions %1) aspect-arguments)))

                new-argument (aspect-argument-factory argument-type argument-index aspect-assertions)]

            new-argument

            ))]

    aspect-argument))

;; ********************
;; FIN: aspect argument
;; ********************

;; **********************
;; BEG: aspect constraint
;; **********************

(defprotocol AspectConstraintProtocol
  (aspect-constraint-arguments [this])
  (aspect-constraint-argument-keys [this])
  (aspect-constraint-argument-values [this])
  (aspect-constraint-add-argument [this argument])
  (aspect-constraint-add-arguments [this arguments])
  (aspect-constraint-new [this arguments])
  (aspect-constraint-assert-same-type [this other])
  (aspect-constraint-is-same-type? [this other])
  (aspect-constraint-express-contract [this])
  (aspect-constraint-express-assertions [this])
  (aspect-constraint-express-signature [this])
  (aspect-constraint-arity [this])
  (aspect-constraint-map-arguments [this map-fn])
  (aspect-constraint-filter-arguments [this predicate-fn])
  (aspect-constraint-select-arguments [this predicate-fn])
  (aspect-constraint-filter-arguments-by-type [this argument-type])
  (aspect-constraint-select-arguments-by-type [this argument-type])
  (aspect-constraint-update [this update-fn]))

(defrecord AspectConstraint [arguments]
  Object
  (toString [t]
    (let [argument-values (aspect-constraint-argument-values t)

          arguments-string (apply str (interpose "-" argument-values))

          constraint-string (format "AspCon(%d/[%s])" (count argument-values) arguments-string)]
      constraint-string)))

(defn is-aspect-constraint? [t] (instance? AspectConstraint t))
(defn is-aspect-constraints?
  [ts]
  {:pre [(coll? ts)]}
  (let [all-valid (every? is-aspect-constraint? ts)]
    (if all-valid ts)))

(defn aspect-constraint-factory
  [arguments]
  { :post [(is-aspect-constraint? %)]}
  (let [normalised-arguments (cond
                              (map? arguments) (vals arguments)
                              (coll? arguments) arguments
                              :else (surprise-exception arguments "aspect-constraint-factory" "argumenst are wat?"))

        regularised-arguments (reduce
                               (fn [s argument]
                                 (if-not (is-aspect-argument? argument)
                                   (surprise-exception argument  "aspect-constraint-factory" "argument is wat?"))
                                 (let [arg-index (aspect-argument-index argument)
                                       arg-type (aspect-argument-type argument)
                                       arg-sig [arg-type arg-index]
                                       _ (if (contains? s arg-sig)
                                           (duplicate-exception arguments "aspect-constraint-factory" "ARG-SIG" arg-sig "seen twice"))]
                                   (assoc s arg-sig argument)))
                               {}
                               normalised-arguments)

        aspect-constraint (->AspectConstraint regularised-arguments)]
    aspect-constraint))

(def aspect-constraint-base-fns
  {

   :toString (fn [t] "AspConstrainttoString")

   :aspect-constraint-arguments (fn [t]
                                  {:post [(map? %)]}
                                  (:arguments t))

   :aspect-constraint-argument-keys (fn [t] (keys (aspect-constraint-arguments t)))
   :aspect-constraint-argument-values (fn [t] (into [] (vals (aspect-constraint-arguments t))))

   :aspect-constraint-add-argument (fn [t a] (aspect-constraint-add-arguments t [a]))

   :aspect-constraint-add-arguments  (fn [t a]
                       {:pre [(coll? a) (every? is-aspect-argument?  a)]}
                       (aspect-constraint-new t (concat (aspect-constraint-argument-values t) a)))

   :aspect-constraint-new (fn [t arguments] (aspect-constraint-factory arguments))

   :aspect-constraint-is-same-type? (fn [t o]
                                      (= (class t) (class 0)))

   :aspect-constraint-assert-same-type (fn [t o] (assert (aspect-constraint-is-same-type? t o)))

   :aspect-constraint-express-contract
   (fn [t]
     {:post [(vector? %) (every? vector? %)]}
     (let [expressed-assertions (aspect-constraint-express-assertions t)
           expressed-signature (aspect-constraint-express-signature t)
           expressed-contract [expressed-signature expressed-assertions]]
       expressed-contract))

   :aspect-constraint-express-assertions
   (fn [t]
     {:post [(vector? %)]}
     (let [arguments (aspect-constraint-argument-values t)

           suck-arguments (sort-by
                           (fn [t] (aspect-argument-index t))
                           (filter is-suck-aspect-argument? arguments))

           suck-assertions  (apply concat (map aspect-argument-express-assertions suck-arguments))

           spit-arguments (filter is-spit-aspect-argument? arguments)

           spit-assertions  (apply concat (map aspect-argument-express-assertions spit-arguments))

           expressed-assertions (into []
                                      (cond
                                       (empty? suck-assertions) (list* '=> spit-assertions)
                                       (empty? spit-assertions)  suck-assertions
                                       :else (concat suck-assertions (list* '=> spit-assertions))))]

       expressed-assertions))

   ;; constraints can be sparse so use max argument index
   ;; to find argument number
   :aspect-constraint-express-signature
   (fn [t]
     {:post [(vector? %)]
}
     (let [suck-arguments (filter is-suck-aspect-argument? (aspect-constraint-argument-values t))

           suck-argument-range (range  (+ 1  (apply max (map (fn [t] (aspect-argument-index t)) suck-arguments))))
           expressed-signature (into [] (sort (map make-fin-suck-arg-symbol-from-index suck-argument-range)))

           _ (if (empty? expressed-signature)
               (missing-exception t "aspect-constraint-express-signature" "expressed-signature is empty"))]
       expressed-signature))

   :aspect-constraint-arity
   (fn [t]
     {:post [(number? %) (< 0 %)]}
     (let [constraint-arity (+ 1 (apply max (map #(second %1) (aspect-constraint-argument-keys t))))]
       constraint-arity))

   ;; map returns a collection of the arguments
   :aspect-constraint-map-arguments
   (fn [t map-fn]
     {:pre [(fn? map-fn)] :post [(is-aspect-arguments? %)]}
     (let [maped-arguments (map map-fn (aspect-constraint-argument-values t))]
       maped-arguments))

   ;; filter returns a collection of the arguments
   :aspect-constraint-filter-arguments
   (fn [t predicate-fn]
     {:pre [(fn? predicate-fn)] :post [(is-aspect-arguments? %)]}
     (let [filtered-arguments (filter predicate-fn (aspect-constraint-argument-values t))]
       filtered-arguments))

   ;; select returns a constraint with only the filtered arguments
   :aspect-constraint-select-arguments
   (fn [t predicate-fn]
     {:pre [(fn? predicate-fn)] :post [(is-aspect-constraint? %)]}
     (let [filtered-arguments (aspect-constraint-filter-arguments t predicate-fn)]
       (aspect-constraint-new t filtered-arguments)))

   :aspect-constraint-filter-arguments-by-type
   (fn [t argument-type]
     {:pre [(is-aspect-argument-type? argument-type)]}
     (aspect-constraint-filter-arguments t (fn [argument] (= argument-type (aspect-argument-type argument)))))

   :aspect-constraint-select-arguments-by-type
   (fn [t argument-type]
     {:pre [(is-aspect-argument-type? argument-type)]}
     (aspect-constraint-select-arguments t (fn [argument] (= argument-type (aspect-argument-type argument)))))

   ;; update applies a fn and returns a new constraint
   :aspect-constraint-update
   (fn [t update-fn]
     {:pre [(fn? update-fn)] :post [(is-aspect-constraint? %)]}
     (let [updated-constraint (update-fn t)]
       updated-constraint))

   }

  )

(extend AspectConstraint AspectConstraintProtocol aspect-constraint-base-fns)

;; **********************
;; FIN: aspect constraint
;; **********************

;; *****************************
;; BEG: merge aspect constraints
;; *****************************

(defn aspect-constraint-merge
  [& aspect-constraints]
  {:pre [(is-aspect-constraints? aspect-constraints)] :post [(is-aspect-constraint? %)]}
  (let [aspect-constraint
        (if (= 1 (count aspect-constraints))
          (first aspect-constraints)
          (let [merged-arguments
                (for [argument-type manifest-aspect-argument-types]
                  (let [arguments-by-type (apply concat (map (fn [t] (aspect-constraint-filter-arguments-by-type t argument-type)) aspect-constraints))

                        new-arguments
                        (if (not-empty arguments-by-type)
                          (if (> (count arguments-by-type) 1)
                            (let [grouped-arguments-by-index (group-by (fn [t] (aspect-argument-index t)) arguments-by-type)

                                  merged-arguments-by-index
                                  (map
                                   (fn [[ndx arguments]]
                                     (if (> (count arguments) 1)
                                       (apply aspect-argument-merge arguments)
                                       (first arguments)))
                                   grouped-arguments-by-index)]
                              merged-arguments-by-index)
                            arguments-by-type

                            ))]

                    (if new-arguments new-arguments)))
                merged-constraint (aspect-constraint-factory (apply concat merged-arguments))]
            merged-constraint
            ))]
    aspect-constraint))

;; *****************************
;; BEG: merge aspect constraints
;; *****************************

;; ******************************
;; BEG: transform aspect argument
;; ******************************

(defn aspect-argument-transform
  ([aspect-argument] (aspect-argument-transform aspect-argument 0))
  ([aspect-argument argument-offset-index] (aspect-argument-transform aspect-argument argument-offset-index {}))
  ([aspect-argument argument-offset-index assertion-maps]
     {:pre [(is-aspect-argument? aspect-argument)
            (is-aspect-argument-index? argument-offset-index)
            (map? assertion-maps)]
      :post [(is-aspect-argument? %)]}
     (let [argument-type (aspect-argument-type aspect-argument)

           ;; be lazy
           updated-argument (if (or (not= 0 argument-offset-index) (contains? assertion-maps argument-type))
                              (let [;; may need to "shift" the index
                                    updated-argument-index (+ argument-offset-index (aspect-argument-index aspect-argument))

                                    ;; no change to argument type
                                    updated-argument-type argument-type

                                    assertions (aspect-argument-assertions aspect-argument)

                                    updated-assertions (if (contains? assertion-maps argument-type)
                                                         (let [assertions-forms (map (fn [assertion] (aspect-assertion-form assertion)) assertions)

                                                               argument-type-assertion-maps (to-vector (get assertion-maps argument-type))

                                                               ;; need to walk the assertion to change argument names (symbols)?
                                                               walked-assertions-forms (walk-forms argument-type-assertion-maps assertions-forms)

                                                               ;; remake the assertions
                                                               walked-assertions (apply aspect-assertion-factory-multiple  walked-assertions-forms)]
                                                           walked-assertions)
                                                         assertions)

                                    updated-argument (aspect-argument-factory updated-argument-type updated-argument-index updated-assertions)]

                                updated-argument)
                              aspect-argument)]

       updated-argument)))

;; ******************************
;; FIN: transform aspect argument
;; ******************************

;; ********************************
;; BEG: transform aspect constraint
;; ********************************

(defn aspect-constraint-transform
  ([aspect-constraint argument-type] (aspect-constraint-transform aspect-constraint argument-type 0))
  ([aspect-constraint argument-type argument-offset-index] (aspect-constraint-transform aspect-constraint argument-type argument-offset-index {}))
  ([aspect-constraint argument-type argument-offset-index assertion-maps]
     {:pre [(is-aspect-constraint? aspect-constraint)
            (is-aspect-argument-index? argument-offset-index)
            (map? assertion-maps)
            (every? is-aspect-argument-type? (keys assertion-maps))]
      :post [(is-aspect-constraint? %)]}
     (let [argument-types (to-collection argument-type)
           _ (assert (every? is-aspect-argument-type? argument-types))

           updated-constraint
           (aspect-constraint-update
            aspect-constraint
            (fn [constraint]
              (let [;; select the constraints by argument-type (suck or spit); may only be one
                    constraint-arguments-by-type
                    (map (fn [argument-type]
                           (let [;; ;; filter arguments of correct type

                                 arguments-by-type (aspect-constraint-filter-arguments-by-type constraint argument-type)

                                 updated-arguments-by-type
                                 (map
                                  (fn [argument]
                                    (let [updated-argument (if (or (not= 0 argument-offset-index) (contains? assertion-maps (aspect-argument-type argument)))
                                                              (aspect-argument-transform argument argument-offset-index assertion-maps)
                                                              argument)]

                                       updated-argument))
                                  arguments-by-type)]

                             ;; updated-argument-type-constraint

                             updated-arguments-by-type

                             ))
                         argument-types)
                    updated-constraint (aspect-constraint-factory (apply concat constraint-arguments-by-type))]

                updated-constraint)))]

       updated-constraint)))

(defn aspect-constraints-transform
  ([aspect-constraints argument-type] (aspect-constraints-transform aspect-constraints argument-type 0))
  ([aspect-constraints argument-type argument-index] (aspect-constraints-transform aspect-constraints argument-type argument-index {}))
  ([aspect-constraints argument-type argument-index assertion-maps]
     {:pre [(is-aspect-constraints? aspect-constraints)  (is-aspect-argument-index? argument-index)]
      :post [(is-aspect-constraints? %)]}

     (let [argument-types (to-collection argument-type)
           _ (assert (every? is-aspect-argument-type? argument-types))

           wanted-constraints aspect-constraints

           ;; need to map absolute and relative argument names and also offset by argument-index
           transformed-constraints (map-indexed
                                  (fn [constraint-index wanted-constraint]
                                    {:pre [(is-aspect-constraint? wanted-constraint)] :post [(is-aspect-constraint? %)]}
                                    (let [updated-wanted-constraint (aspect-constraint-transform wanted-constraint argument-type argument-index assertion-maps)]
                                      updated-wanted-constraint))
                                  wanted-constraints)]

       ;;nominal-definition
       transformed-constraints)))

(defn aspect-constraints-transform-by-type
  [aspect-constraints argument-type]
  (aspect-constraints-transform aspect-constraints argument-type))

(defn aspect-constraints-transform-by-index
  [aspect-constraints argument-index]
  (aspect-constraints-transform aspect-constraints manifest-aspect-assertion-types argument-index))

(defn aspect-constraints-transform-by-type-index
  [aspect-constraints argument-type argument-index]
  (aspect-constraints-transform aspect-constraints argument-type argument-index))

(defn aspect-constraints-transform-by-assertions
  [aspect-constraints assertion-maps]
  (aspect-constraints-transform aspect-constraints manifest-aspect-assertion-types 0 assertion-maps))

;; ********************************
;; FIN: transform aspect constraint
;; ********************************

;; ************
;; BEG: memoize
;; ************

(wrap-functions

 ;;is-aspect-argument-index?
 ;;is-aspect-argument-type?

 aspect-argument-merge
 aspect-constraint-merge

 aspect-constraint-transform
 aspect-constraints-transform

 aspect-assertion-factory
 aspect-argument-factory
 aspect-constraint-factory
 )

;; ************
;; FIN: memoize
;; ************

