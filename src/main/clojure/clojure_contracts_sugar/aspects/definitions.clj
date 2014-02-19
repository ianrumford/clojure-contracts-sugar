(ns clojure-contracts-sugar.aspects.definitions
  (:require [clojure-contracts-sugar.manifests :as manifests
             :refer (
                     manifest-aspect-argument-type-suck
                     manifest-aspect-argument-type-spit
                     manifest-aspect-assertion-types
                     manifest-aspect-argument-types)]
            
            [clojure-contracts-sugar.contracts.store :as contracts-store
             :refer (find-aspect-mnemonic-definition
                     find-aspect-mnemonic-constraints
                     validate-aspect-mnemonic
                     is-aspect-mnemonic?
                     snapshot-aspects-store
                     snapshot-constraints-store
                     snapshot-contracts-store
                     configure-contracts-store)]
            
            [clojure-contracts-sugar.aspects.forms :as aspect-forms
             :refer (validate-aspect-form
                     is-aspect-form?
                     is-aspect-form-vector-form?)]
            
            [clojure-contracts-sugar.aspects.constraints :as aspect-constraints
             :refer (is-aspect-constraint?
                     is-aspect-constraints?
                     is-aspect-argument-type?
                     is-aspect-argument-types?
                     is-aspect-argument-index?
                     is-aspect-assertion?
                     is-aspect-assertions?
                     aspect-assertion-update
                     is-aspect-argument?
                     is-aspect-arguments?

                     aspect-assertion-form
                     aspect-assertion-factory
                     aspect-assertion-factory-multiple
                     aspect-argument-factory
                     aspect-argument-map-assertions
                     aspect-argument-update
                     aspect-constraint-factory
                     aspect-constraint-select-arguments-by-type
                     aspect-constraint-arguments
                     aspect-constraint-argument-values
                     aspect-argument-index
                     aspect-argument-type
                     aspect-argument-assertions
                     aspect-constraint-merge
                     aspect-constraint-filter-arguments-by-type
                     aspect-constraint-map-arguments
                     aspect-constraint-select-arguments

                     aspect-constraint-arity
                     aspect-constraint-update

                     aspect-constraints-transform-by-type-index
                     aspect-constraints-transform-by-assertions
                     aspect-constraints-transform)]
            
            [clojure-contracts-sugar.utils.utils :as utils
             :refer (to-collection
                     to-vector
                     atom?
                     make-walk-argument-symbol-map
                     find-argument-symbol-from-index-fn
                     walk-forms)]

            [clojure-contracts-sugar.utils.memoized :as utils-memo
             :refer (is-memoized?
                     snapshot-memoized
                     evict-memoized)]

            [clojure-contracts-sugar.utils.wrapped-functions :as utils-wrapped
             :refer (wrap-functions
                     wrap-functions-with-template)]

            [clojure-carp :as carp :refer (surprise-exception missing-exception duplicate-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]))


(declare find-aspect-constraints-for-mnemonic)
(declare find-aspect-constraints-for-symbol)
(declare find-aspect-constraints-for-expression)
(declare find-aspect-constraints-for-assertion)
(declare aspect-contraint-rewrites-by-type-index-assertions)

;; *******************************
;; BEG: validate aspect definition
;; *******************************

(defn aspect-definition-type
  [aspect-definition]
  {:post [(keyword? %)]}
  (cond
   (map? aspect-definition) :map
   (vector? aspect-definition) :vector
   (symbol? aspect-definition) :symbol
   (keyword? aspect-definition) :keyword
   (coll? aspect-definition) :coll
   :else :default))

(defn is-aspect-definition?
  [aspect-definition]
  (let [is-aspect-definition
        (and
         (coll? aspect-definition)
         (not-any? nil?
                   (for [aspect-entry aspect-definition]
                     (cond
                      (is-aspect-constraint? aspect-entry) aspect-entry
                      :else (is-aspect-form? aspect-entry)))))]
    (if is-aspect-definition aspect-definition)))

;; aspect-definition is a vector of aspect-forms and/or aspect-constraints
(defn validate-aspect-definition
  [aspect-definition]
  {:pre [(is-aspect-definition? aspect-definition)]}
  aspect-definition)

;; *******************************
;; FIN: validate aspect definition
;; *******************************

;; *************************************
;; BEG: transform constraints symbol map
;; *************************************

(defn make-rewrite-assertion-map
  ([argument-max src-type src-fluke src-offset tgt-type tgt-fluke tgt-offset]
     {:pre [(is-aspect-argument-type? src-type)
            (is-aspect-argument-type? tgt-type)
            (is-aspect-argument-index? argument-max)]}
     (let [;; tgt-fn (get-in utils/make-argument-symbol-from-index-fn-map [tgt-type tgt-fluke

           src-fn (find-argument-symbol-from-index-fn src-type src-fluke)
           tgt-fn (find-argument-symbol-from-index-fn tgt-type tgt-fluke)

           rewrite-assertion-map (make-walk-argument-symbol-map argument-max src-offset tgt-offset src-fn tgt-fn)]

       rewrite-assertion-map)))

(defn final-transform-aspect-constaints-symbol-arguments
  [aspect-constraints]
  {:pre [(is-aspect-constraints? aspect-constraints)]
   :post [(is-aspect-constraints? %)]}
  (let [argument-no (apply max (list* 8 (map (fn [constraint] (count (aspect-constraint-arguments constraint))) aspect-constraints)))
        assertion-maps {:spit [(make-rewrite-assertion-map argument-no :spit :abs 0 :spit :fin 0)
                               (make-rewrite-assertion-map argument-no :spit :rel 0 :spit :fin 0)]
                        :suck (make-rewrite-assertion-map argument-no :suck :abs 0 :suck :fin 0)}
        transformed-constraints (aspect-constraints-transform-by-assertions aspect-constraints assertion-maps)]
    transformed-constraints))

;; *************************************
;; FIN: transform constraints symbol map
;; *************************************

;; ********************************
;; BEG: normalise aspect definition
;; ********************************

(defmulti normalise-aspect-definition aspect-definition-type)

(defmethod normalise-aspect-definition :keyword
  [aspect-mnemonic]
  {:pre [(validate-aspect-mnemonic aspect-mnemonic)] :post [(validate-aspect-definition %)]}
  (let [aspect-definition (find-aspect-constraints-for-mnemonic aspect-mnemonic)

        _ (when-not aspect-definition
            (missing-exception aspect-mnemonic "normalise-aspect-definition" "KEYWORD" "aspect-mnemonic not found in contracts store"))]
    aspect-definition))

(defmethod normalise-aspect-definition :symbol
  [aspect-symbol]
  {:pre [(symbol? aspect-symbol)] :post [(validate-aspect-definition %)]}
  (let [;;aspect-definition [{:suck aspect-symbol :spit aspect-symbol}]
        ;; symbol is applied to suck only
        aspect-definition (normalise-aspect-definition {:suck aspect-symbol})]
    aspect-definition))

;; the definition is actually one (a) single form
(defmethod normalise-aspect-definition :map
  [aspect-definition]
  {:pre [(map? aspect-definition)] :post [(is-aspect-definition? %)]}

  (let [normalised-aspect-definition [(aspect-forms/normalise-aspect-form aspect-definition)]]
    normalised-aspect-definition))

;; the most complicated one - need to disambiguate clojure.core.contract vector
;; from the multiple arity vector of aspect forms
(defmethod normalise-aspect-definition :vector
  [aspect-definition]
  {:pre [(vector? aspect-definition)] :post [(is-aspect-definition? %)]}
  (let [normalised-aspect-definition  (if (aspect-forms/is-aspect-form-vector-form? aspect-definition)
                                        [(aspect-forms/normalise-aspect-form aspect-definition)]
                                        (normalise-aspect-definition (seq aspect-definition)))]
    normalised-aspect-definition))

;; each entry must be an aspect form
;; representing a differnt arity for *same* aspect
(defmethod normalise-aspect-definition :coll
  [aspect-definition]
  {:pre [(coll? aspect-definition)] :post [(is-aspect-definition? %)]}
  (let [normalised-aspect-definition
        (into []
              (map-indexed
               (fn [aspect-index aspect-entry]
                 (cond
                  (is-aspect-form-vector-form? aspect-entry) (aspect-forms/normalise-aspect-form aspect-entry)
                  (map? aspect-entry) (aspect-forms/normalise-aspect-form aspect-entry)
                  :else (surprise-exception aspect-entry "normalise-aspect-definition" "COLL" "ASPECT-INDEX" aspect-index "aspect-entry is wat?")))

               aspect-definition))]
    normalised-aspect-definition))

(defmethod normalise-aspect-definition :default
  [aspect-definition]
  (surprise-exception aspect-definition "normalise-aspect-definition" "DEFAULT" "aspect-definition is what?"))

(defn normalise-aspect-definitions
  [aspect-definitions]
  (let [normalised-definitions (map normalise-aspect-definition aspect-definitions)]
    normalised-definitions))

;; ********************************
;; FIN: normalise aspect definition
;; ********************************

;; ****************************************
;; BEG: create aspect forms from definition
;; ****************************************

(defn reduce-aspect-definition-to-forms
  "create the aspect forms for each entry in the definition"
  [aspect-definitions]
  {:pre [(coll? aspect-definitions) (every? is-aspect-definition? aspect-definitions)] :post [(coll? %) (every? is-aspect-form? %)]}
  (let [aspect-forms (apply concat aspect-definitions) ]
    aspect-forms))

;; ****************************************
;; FIN: create aspect forms from definition
;; ****************************************

(defn validate-aspect-constraint-arities
  [max-arity aspect-constraints]
  {:pre [(number? max-arity) (pos? max-arity) (is-aspect-constraints? aspect-constraints)] :post [(number? %)]}
  (let [constraint-arities (map #(aspect-constraint-arity %1) aspect-constraints)

        ;; ensure arities <= max arity
        _ (doall
           (for [constraint-arity constraint-arities]
             (if-not (>= max-arity constraint-arity) (surprise-exception constraint-arities "validate-aspect-constraint-arities" "MAX-ARITY" max-arity "ASPECT-CONSTRAINTS" aspect-constraints "have some arities greater than max"))))]
    max-arity))

(defn validate-aspect-constraint-arguments-same-type
  [argument-type aspect-constraints]
  {:pre [(is-aspect-argument-type? argument-type) (is-aspect-constraints? aspect-constraints)] :post [(is-aspect-argument-type? %)]}
  (let [;; ensure arguments are all same type
        constraint-arguments  (apply concat (map #(aspect-constraint-argument-values %1) aspect-constraints))
        constraint-argument-types (map #(aspect-argument-type %1) constraint-arguments)
        _ (if-not (apply = constraint-argument-types)
            (surprise-exception constraint-argument-types "validate-aspect-constraint-arguments-same-type" "ARGUMENT-TYPE" argument-type  "CONSTRAINT-ARGUMENTS" constraint-arguments "have unexpetced types"))]
    argument-type))

;; **********************************************
;; BEG: create aspect constraint from aspect form
;; **********************************************

;; NOTE: cacf = create-aspect-constraints-from

(defn cacf-argument-value
  [argument-value argument-type argument-index]
  {:pre [(is-aspect-argument-type? argument-type)
         (number? argument-index)]
   :post [(is-aspect-constraints? %)]}
  (let [mnemonic-constraints
        (cond

         (is-aspect-mnemonic? argument-value)
         (let [aspect-mnemonic argument-value

               nominal-constraints (find-aspect-constraints-for-mnemonic aspect-mnemonic)
               ;; need to rewrite

               argument-no (apply max (map (fn [constraint] (count (aspect-constraint-arguments constraint))) nominal-constraints))
               assertion-maps {
                               :suck (make-rewrite-assertion-map argument-no :suck :rel 0 :suck :fin argument-index)
                               }
               transformed-constraints (aspect-constraints-transform nominal-constraints argument-type argument-index assertion-maps)]

           transformed-constraints)

         (symbol? argument-value)
         (find-aspect-constraints-for-symbol argument-value argument-type argument-index)
         (coll? argument-value)
         (find-aspect-constraints-for-expression argument-value argument-type argument-index)

         :else (surprise-exception argument-value "cacf-argument-value" "argument-value is wat?"))

        argument-constraints mnemonic-constraints]

    argument-constraints
    ))

;; nominal-assertion e.g. :map, (map? arg3), etc
;; assertion-type is suck, spit, etc
(defn cacf-argument-spec-entry
  [argument-value argument-type argument-index]
  {:pre [(is-aspect-argument-type? argument-type) (number? argument-index)] :post [(is-aspect-constraints? %)]}

  (let [nominal-constraints (cacf-argument-value argument-value argument-type argument-index)

        ;; argument-constraints-by-type
        ;; (map
        ;;  nominal-constraints)

        ;; argument-constraints argument-constraints-by-type

        argument-constraints nominal-constraints]

    argument-constraints))

(defn cacf-argument-spec
  [argument-type argument-index argument-spec]
  {:pre [(keyword? argument-type) (number? argument-index)] :post [(is-aspect-constraints? %)]}
  (let [argument-values (to-collection argument-spec)

        ;; max no. of arguments for this assertion
        max-arity (count argument-values)

        nominal-constraints
        (apply concat
               (map-indexed
                (fn [entry-index entry-value]
                  ;; NOTE - the index is the *argument* *not* the entry

                  (cacf-argument-spec-entry entry-value argument-type argument-index))

                argument-values))

        _ (validate-aspect-constraint-arguments-same-type argument-type nominal-constraints)

        reduced-constraints (if (> (count nominal-constraints) 1)
                              [(apply aspect-constraint-merge nominal-constraints)]
                              nominal-constraints)

        argument-constraints reduced-constraints]

    argument-constraints))

(defn create-aspect-constraint-from-form
  [aspect-index aspect-form]
  {:pre [(number? aspect-index) (is-aspect-form? aspect-form)] :post [(is-aspect-constraint? %)]}
  (let [;;reduction-ctrl aspect-forms-reduction-ctrl

        argument-constraints
        (apply concat
               (map
                (fn [[argument-type arguments-spec]]
                  (let [_ (assert (coll? arguments-spec))

                        argument-type-constraints
                        (apply concat
                               (map
                                (fn [[argument-index argument-spec]]
                                  (assert (coll? argument-spec))
                                  (let [;; ;; ... and create the aspect-constraint
                                        spec-constraints (cacf-argument-spec argument-type argument-index argument-spec)]

                                    spec-constraints))
                                arguments-spec))]

                    argument-type-constraints

                    ))

                aspect-form))

        aspect-constraint (apply aspect-constraint-merge argument-constraints)]
    aspect-constraint))

;; **********************************************
;; FIN: create aspect constraint from aspect form
;; **********************************************

;; *********************************************************
;; BEG: create aspect constraints from normalised definition
;; *********************************************************

(defn create-aspect-constraints-from-normalised-definition
  "create the aspect constraint for each form (entry) in the definition.
   remember each form represents *one* arity.
   each form is reduced first to resolve the entries in the e.g. suck & spit keys.
   each resolved entry will itself generate one or more forms"
  [aspect-definition]
  {:pre [(is-aspect-definition? aspect-definition)] :post [(is-aspect-constraints? %)]}
  (let [aspect-constraints (map-indexed
                            (fn [aspect-index aspect-something]

                              (cond
                               ;; already a constraint?
                               (is-aspect-constraint? aspect-something) aspect-something
                               ;; an aspect-form to convert?
                               (map? aspect-something) (create-aspect-constraint-from-form aspect-index aspect-something)
                               :else (surprise-exception aspect-something "create-aspect-constraints-from-normalised-definition" "aspect-something is wat?")))

                            aspect-definition)]

    aspect-constraints))

;; *********************************************************
;; FIN: create aspect constraints from normalised definition
;; *********************************************************

;; **********************************************
;; BEG: create aspect constraints from definition
;; **********************************************

(defn create-aspect-constraints-from-definition
  "create the contract constraints from its definition.
   note the contract may have multiple arities
   depending on what the definition is exactly
   e.g. it may have multipl suck/spit maps for the different arities"
  [aspect-definition]
  {:post [(is-aspect-constraints? %)]}
  (let [;; the normalised aspect definition will be a vector of
        ;; aspect-forms or aspect-constraints
        normalised-aspect-definition (normalise-aspect-definition aspect-definition)

        aspect-constraints (create-aspect-constraints-from-normalised-definition normalised-aspect-definition)

        constraint-arities (map aspect-constraints/aspect-constraint-arity aspect-constraints)

        _ (if-not (apply distinct? constraint-arities)
            (duplicate-exception aspect-constraints "create-aspect-constraints-from-definition" "constraint with same arities" constraint-arities))]

    aspect-constraints))

;; **********************************************
;; FIN: create aspect constraints from definition
;; **********************************************

;; ******************************
;; BEG: create aspect constraints
;; ******************************

(defn find-aspect-constraints-for-assertion
  [aspect-assertion argument-type argument-index]
  {:pre [(is-aspect-argument-index? argument-index)] :post [(is-aspect-constraints? %)]}
  (let [argument-types (to-collection argument-type)

        _ (assert (every? is-aspect-argument-type? argument-types))

        ;; _ (for [arg-type argument-types]

        aspect-constraints
        (let [assertion (if (is-aspect-assertion? aspect-assertion)
                          aspect-assertion
                          (aspect-assertion-factory aspect-assertion))

              arguments (map
                         (fn [arg-type]
                           (is-aspect-argument-type? arg-type)
                           (aspect-argument-factory arg-type argument-index [assertion]))
                         argument-types)

              constraint (aspect-constraint-factory arguments)]
          [constraint])]
    aspect-constraints))

(defn find-aspect-constraints-for-symbol
  ([aspect-mnemonic] (find-aspect-constraints-for-symbol aspect-mnemonic manifest-aspect-argument-type-suck  0))
  ([aspect-symbol argument-type argument-index]
     {:pre [(symbol? aspect-symbol)]}
     (let [aspect-constraints (find-aspect-constraints-for-assertion aspect-symbol argument-type argument-index)]
       aspect-constraints)))
(defn find-aspect-constraints-for-expression
  [aspect-expression argument-type argument-index]
  {:pre [(coll? aspect-expression)]}
  (let [aspect-constraints (find-aspect-constraints-for-assertion aspect-expression argument-type argument-index)]
    aspect-constraints))

(defn find-aspect-constraints-for-built-in-mnemonic
  [aspect-mnemonic argument-index]
  {:pre [(is-aspect-mnemonic? aspect-mnemonic)] :post [(is-aspect-constraints? %)]}
  (let [;; see if this is a standard / known predicate e.g. :map => map?

        aspect-symbol (utils/resolve-aspect-symbol-from-keyword aspect-mnemonic)

        aspect-var (resolve aspect-symbol)
        aspect-constraints (find-aspect-constraints-for-symbol
                            aspect-symbol
                            manifest-aspect-argument-types
                            argument-index)]
    aspect-constraints))

;; DO NOT MEMOIZE
(defn find-aspect-constraints-for-mnemonic
  ([aspect-mnemonic]
     {:pre [ (is-aspect-mnemonic? aspect-mnemonic)] :post [(is-aspect-constraints? %)]}
     (let [aspect-constraints
           (if-let [constraints (find-aspect-mnemonic-constraints aspect-mnemonic)]
             constraints
             (let [mnemonic-definition (find-aspect-mnemonic-definition aspect-mnemonic)

                   aspect-definition nil

                   argument-index 0

                   mnemonic-constraints
                   (cond

                    (nil? mnemonic-definition) (find-aspect-constraints-for-built-in-mnemonic aspect-mnemonic argument-index)
                    (symbol? mnemonic-definition) (find-aspect-constraints-for-symbol mnemonic-definition)

                    (keyword? mnemonic-definition)
                    (let [aspect-symbol (utils/resolve-aspect-symbol-from-keyword mnemonic-definition)
                          constraints (find-aspect-constraints-for-assertion
                                       aspect-symbol
                                       manifest-aspect-argument-types
                                       argument-index)]
                      constraints)

                    (map? mnemonic-definition) (create-aspect-constraints-from-definition mnemonic-definition)
                    (vector? mnemonic-definition) (create-aspect-constraints-from-definition mnemonic-definition)

                    :else (surprise-exception mnemonic-definition "find-aspect-constraints-for-mnemonic" "mnemonic-definition is wat?"))]
               ;; remember the constraints
               (configure-contracts-store 'aspect-mnemonic-constraints {aspect-mnemonic mnemonic-constraints})
               mnemonic-constraints))]
       aspect-constraints)))

;; ******************************
;; FIN: create aspect constraints
;; ******************************

;; *************
;; BEG:  memoize
;; *************

(wrap-functions
 is-aspect-definition?
 make-rewrite-assertion-map
 cacf-argument-value
 cacf-argument-spec-entry
 cacf-argument-spec
 create-aspect-constraint-from-form

 find-aspect-constraints-for-symbol
 find-aspect-constraints-for-expression

 ;; DO NOT MEMOIZE find-aspect-constraints-for-mnemonic
 find-aspect-constraints-for-built-in-mnemonic

 )

;; *************
;; FIN:  memoize
;; *************

