(ns clojure-contracts-sugar.aspects.specifications
  (:require [clojure-contracts-sugar.manifests :as manifests
             :refer (manifest-aspect-specification-key-name
                     manifest-aspect-specification-key-form-def
                     manifest-aspect-specification-key-form-ctx
                     manifest-aspect-specification-keys)]

            [clojure-contracts-sugar.aspects.definitions :as aspect-definitions
             :refer (normalise-aspect-definition
                     create-aspect-constraints-from-definition
                     final-transform-aspect-constaints-symbol-arguments)]

            [clojure-contracts-sugar.aspects.constraints :as aspect-constraints :refer (aspect-constraint-express-contract)]

            [clojure-contracts-sugar.utils.utils :as utils]
            [clojure-carp :as carp :refer (surprise-exception missing-exception duplicate-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]))

;; **********************************
;; BEG: validate aspect specification
;; **********************************

(defn is-aspect-specification?
  [aspect-specification]
  (let [is-aspect-specification
        (and
         (map? aspect-specification)
         (every? manifest-aspect-specification-keys (keys aspect-specification)))]
    (if is-aspect-specification aspect-specification)))

(defn validate-aspect-specification
  [aspect-specification]
  {:pre [(is-aspect-specification? aspect-specification)]}
  aspect-specification)

;; **********************************
;; FIN: validate aspect specification
;; **********************************

;; ***************************************
;; BEG: aspect definition to specification
;; ***************************************

(defn create-aspect-specification-from-definition
  "create the contract specification from its definition.
   note the contract may have multiple arities
   depending on what the definition is exactly
   e.g. it may have multipl suck/spit maps for the different arities"
  [aspect-definition]
  (let [nominal-constraints (create-aspect-constraints-from-definition aspect-definition)

        aspect-constraints (final-transform-aspect-constaints-symbol-arguments nominal-constraints)

        constraint-arities (map aspect-constraints/aspect-constraint-arity aspect-constraints)

        _ (if-not (apply distinct? constraint-arities)
            (duplicate-exception aspect-constraints "create-aspect-specification-from-definition" "constraint with same arities" constraint-arities))

        contract-forms (apply concat (map aspect-constraint-express-contract aspect-constraints))

        ctx-name (gensym "ctx-aspect" )
        def-name (symbol (str "def-" ctx-name))
        ctx-doc (str \" (str ctx-name)  \")

        ctx-form (list* 'clojure.core.contracts/contract ctx-name ctx-doc contract-forms)

        def-form (list 'def def-name ctx-form)

        aspect-specification {manifest-aspect-specification-key-name def-name
                              manifest-aspect-specification-key-form-def def-form
                              manifest-aspect-specification-key-form-ctx ctx-form
                              }]

    (validate-aspect-specification aspect-specification)))

;; *************************************************
;; BEG: resolving aspect specification and contracts
;; *************************************************

(defn resolve-aspect-specifications
  "create a map of aspect specification from the soemthings
   which may be mnemonics, definitions, whatever"
  [aspect-somethings]
  {:pre [(coll? aspect-somethings)] :post [(map? %)]}
  (let [aspect-specifications
        (into {}
              (map (fn [aspect-somthing]

                     (let [aspect-definition aspect-somthing

                           _ (or aspect-definition (missing-exception aspect-somthing "resolve-aspect-specifications" "aspect-somthing not found"))
                           aspect-specification (create-aspect-specification-from-definition aspect-definition)]

                       ;; create the return vector for this soemthing
                       ;; with its specification - these become key &
                       ;; value in the specification map
                       [aspect-somthing aspect-specification]))

                   aspect-somethings))]
    aspect-specifications))

;; *************************************************
;; FIN: resolving aspect specification and contracts
;; *************************************************

