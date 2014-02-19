(ns clojure-contracts-sugar.aspects.contracts
  (:require [clojure-contracts-sugar.manifests :as manifests :refer (manifest-aspect-specification-key-form-ctx)]
            [clojure-contracts-sugar.aspects.specifications :as aspects-specifications]
            [clojure-carp :as carp :refer (surprise-exception missing-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]))

;; *******************************
;; BEG: resolving aspect contracts
;; *******************************

(defn resolve-aspects-contracts
  ([aspect-somethings] (resolve-aspects-contracts aspect-somethings (aspects-specifications/resolve-aspect-specifications aspect-somethings)))
  ([aspect-somethings aspect-specifications]
     (let [;; find the contract specifications
           aspect-contracts
           (into {}
                 (for [aspect-somthing aspect-somethings]
                   (let [aspect-specification (get aspect-specifications aspect-somthing)

                         _ (if-not aspect-specification
                             (missing-exception aspect-somthing "resolve-aspect-contracts" "aspect-specification not found for mnemonic"))

                         aspect-contract (get aspect-specification manifest-aspect-specification-key-form-ctx)

                         _ (if-not aspect-contract
                             (missing-exception aspect-somthing "resolve-aspect-contracts" "aspect-contract not found for mnemonic"))]

                     [aspect-somthing aspect-contract])))]
       aspect-contracts)))

;; *******************************
;; BEG: resolving aspect contracts
;; *******************************

