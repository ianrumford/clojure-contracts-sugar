(ns clojure-contracts-sugar.contracts.store
  (:require [clojure-contracts-sugar.manifests :as manifests
             :refer (manifest-contracts-store-key-aspects-store
                     manifest-contracts-store-key-constraints-store)]
            [clojure-contracts-sugar.utils.memoized :as utils-memo :refer (new-memoized is-memoized? update-memoized edit-memoized-snapshot lookup-memoized snapshot-memoized evict-memoized)]

            [clojure-contracts-sugar.utils.utils :as utils :refer (to-collection)]
            [clojure-carp :as carp :refer (surprise-exception)]
            [clojure.core.memoize :as memo]))

;; Store Support for clojure-contracts-sugar

;; *****************************************
;; BEG: standard aspect mnemonic definitions
;; *****************************************

(def not-nil? (complement nil?))
(defn pred-true [& x] true)
(defn pred-false [& x] false)

(def standard-aspect-mnemonics {})

;; *****************************************
;; FIN: standard aspect mnemonic definitions
;; *****************************************

;; ******************************
;; BEG: aspect mnemonic functions
;; ******************************

(defn is-aspect-mnemonic?
  [aspect-mnemonic]
  (if (keyword? aspect-mnemonic) aspect-mnemonic))

(defn is-aspect-mnemonics?
  [ts]
  {:pre [(or (nil? ts) (coll? ts))]}
  (if (every? is-aspect-mnemonic? ts) ts))

;; mostly a placeholder
(defn validate-aspect-mnemonic
  [aspect-mnemonic]
  {:pre [(is-aspect-mnemonic? aspect-mnemonic)]}
  aspect-mnemonic)

;; be lazy - dont truely validate until use
(defn validate-aspect-mnemonic-definitions
  [mnemonic-definitions]
  {:post [(map? %) (every? is-aspect-mnemonic? (keys %))]}
  mnemonic-definitions)

;; ******************************
;; FIN: aspect mnemonic functions
;; ******************************

;; ********************************
;; BEG: constraints store functions
;; ********************************

(defn- update-constraints-store
  [constraints-store & {:keys [mnemonics] :as opt-args}]
  {:pre [(is-memoized? constraints-store)
         (map? mnemonics) (every? is-aspect-mnemonic? (keys mnemonics))]
   :post [(is-memoized? %)]}
  (if mnemonics (update-memoized constraints-store (validate-aspect-mnemonic-definitions mnemonics)))
  constraints-store)

;; ********************************
;; FIN: constraints store functions
;; ********************************

;; ****************************
;; BEG: aspects store functions
;; ****************************

(defn- update-aspects-store
  [aspects-store & {:keys [mnemonics] :as opt-args}]
  {:pre [(is-memoized? aspects-store)
         (map? mnemonics) (every? is-aspect-mnemonic? (keys mnemonics))]
   :post [(is-memoized? %)]}
  (if mnemonics (do
                  (update-memoized aspects-store (validate-aspect-mnemonic-definitions mnemonics))))
  aspects-store)

;; ****************************
;; FIN: aspects store functions
;; ****************************

;; ******************************
;; BEG: contracts store functions
;; ******************************

(defn- update-contracts-store
  [contracts-store & {:keys [aspects-store constraints-store] :as opt-args}]
  {:pre [(is-memoized? contracts-store)] :post [(is-memoized? %)]}
  (let [edit-fn (fn [snapshot]
                  (let [normal-opt-args
                        {manifest-contracts-store-key-aspects-store (:aspects-store opt-args)
                         manifest-contracts-store-key-constraints-store (:constraints-store opt-args)}
                        updated-snapshot (merge snapshot normal-opt-args)]
                    updated-snapshot
                    ))]
    (edit-memoized-snapshot contracts-store edit-fn)
    contracts-store))

(defn- make-fn-contracts-store-find-aspect-mnemonic-definition
  [contracts-store]
  {:pre [(is-memoized? contracts-store)] :post [(fn? %)]}
  (let []

    (fn [aspect-mnemonic]
      {:pre [(is-aspect-mnemonic? aspect-mnemonic)]}
      (let [aspects-store (contracts-store manifest-contracts-store-key-aspects-store)

            aspect-definition (lookup-memoized aspects-store aspect-mnemonic)]
        aspect-definition))))

(defn- make-fn-contracts-store-find-aspect-mnemonic-constraints
  [contracts-store]
  {:pre [(is-memoized? contracts-store)] :post [(fn? %)]}
  (let []

    (fn [aspect-mnemonic]
      {:pre [(is-aspect-mnemonic? aspect-mnemonic)]}
      (let [constraints-store (contracts-store manifest-contracts-store-key-constraints-store)

            aspect-constraints (lookup-memoized constraints-store aspect-mnemonic)]
        aspect-constraints))))

(defn make-fn-configure-contracts-store
  [contracts-store]
  {:pre [(is-memoized? contracts-store)]}
  (fn [& {:syms [aspect-mnemonic-definitions aspect-mnemonic-constraints evict-aspect-mnemonic-definitions-constraints] :as opt-args}]
    {:post [(is-memoized? %)]}
    (let [;; any unexpected arguments?
          _ (assert (every? #{'aspect-mnemonic-definitions 'aspect-mnemonic-constraints 'evict-aspect-mnemonic-definitions-constraints} (keys opt-args)))

          normal-aspect-mnemonic-definitions
          (if aspect-mnemonic-definitions (validate-aspect-mnemonic-definitions aspect-mnemonic-definitions) {})

          normal-evict-aspect-mnemonic-definitions-constraints
          (if evict-aspect-mnemonic-definitions-constraints (validate-aspect-mnemonic-definitions evict-aspect-mnemonic-definitions-constraints) [])

          aspects-store (contracts-store manifest-contracts-store-key-aspects-store)
          _ (assert (is-memoized? aspects-store))
          constraints-store (contracts-store manifest-contracts-store-key-constraints-store)
          _ (assert is-memoized? constraints-store)

          evict-mnemonics (distinct (concat (keys normal-aspect-mnemonic-definitions) normal-evict-aspect-mnemonic-definitions-constraints))

          _ (assert (is-aspect-mnemonics? evict-mnemonics))

          ;; any evictions from the constraints-store?
          _ (if (not-empty evict-mnemonics)
              (let [_ (apply evict-memoized constraints-store evict-mnemonics)]))

          ;; any additions to the aspects-store
          _ (if aspect-mnemonic-definitions
            (let [_ (update-aspects-store aspects-store :mnemonics aspect-mnemonic-definitions)]))

          _ (if aspect-mnemonic-constraints
            (let [_ (update-constraints-store constraints-store :mnemonics aspect-mnemonic-constraints)]))]
      contracts-store)))

;; ******************************
;; FIN: contracts store functions
;; ******************************

;; ******************
;; BEG: aspects store
;; ******************

(def aspects-store (new-memoized :memoizer :memo :init-fn  update-aspects-store  :init-args [:mnemonics standard-aspect-mnemonics]))

(def constraints-store (new-memoized :memoizer :lu :memoizer-args [:lu/threshold 40]))

(def contracts-store (new-memoized :memoizer :memo :init-fn update-contracts-store :init-args [:aspects-store aspects-store :constraints-store constraints-store]))

;; ********************
;; FIN: contracts store
;; ********************

;; **************************************************
;; BEG: dynamically defined contracts store functions
;; **************************************************

(def find-aspect-mnemonic-definition
  (make-fn-contracts-store-find-aspect-mnemonic-definition contracts-store))

(def find-aspect-mnemonic-constraints
  (make-fn-contracts-store-find-aspect-mnemonic-constraints contracts-store))

(def configure-contracts-store
  (make-fn-configure-contracts-store  contracts-store))

(def snapshot-aspects-store (utils-memo/make-fn-snapshot-memoized aspects-store))
(def snapshot-constraints-store (utils-memo/make-fn-snapshot-memoized constraints-store))
(def snapshot-contracts-store (utils-memo/make-fn-snapshot-memoized contracts-store))

;; **************************************************
;; FIN: dynamically defined contracts store functions
;; **************************************************

