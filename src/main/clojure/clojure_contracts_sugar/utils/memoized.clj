(ns clojure-contracts-sugar.utils.memoized
  (:require [clojure.core.memoize :as memo]
            [clojure.core.cache :as cache]
            [clojure-carp :as carp :refer (surprise-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]

            [clojure-contracts-sugar.utils.utils :as utils
             :refer (to-collection
                     to-vector
                     atom?

                     )]))

;; Memoization for clojure-contracts-sugar

;; *********************
;; BEG: common functions
;; *********************

(defn- normalise-memoized-cache-key [key] (to-vector key))

;; *********************
;; FIN: common functions
;; *********************

;; ***********************
;; BEG: memoized functions
;; ***********************

(defn is-memoized?
  [memo-fn]
  (and (fn? memo-fn) (memo/memoized? memo-fn)))

(defn is-memoized-cache?
  [cache-value]
  (instance? clojure.core.memoize.PluggableMemoization cache-value))

(defn find-memoized-cache
  [memo-fn]
  {:pre [(is-memoized? memo-fn)]}
  (let [memo-meta (meta memo-fn)
        _ (assert (map? memo-meta))

        cache-atom (get memo-meta :clojure.core.memoize/cache)
        _ (assert (atom? cache-atom))

        cache-value @cache-atom
        _ (assert (is-memoized-cache?  cache-value) )]
    cache-value))

(defn snapshot-memoized
  [memo-fn]
  {:pre [(is-memoized? memo-fn)] :post [(map? %)]}
  (memo/snapshot memo-fn))

(defn edit-memoized
  "general function to apply an edit fn
   to the current snapshot of a cache
   and update the cache with the result"
  [memo-fn fn-edit & edit-args]
  {:pre [(fn? fn-edit) (is-memoized? memo-fn)] :post [(is-memoized? %)]}
  (let [updated-memo-fn (apply fn-edit (list* memo-fn edit-args))]
    updated-memo-fn))

(defn- normalise-memoized-snapshot-keys
  [snapshot]
  {:pre [(map? snapshot)] :post [(map? %) (every? vector? (keys %))]}
  (into {} (map (fn [[k v]] [(to-vector k) v]) snapshot)))

(defn edit-memoized-snapshot
  "general function to apply an edit fn
   to the current snapshot of a cache
   and update the cache with the result"
  [memo-fn fn-edit & edit-args]
    {:pre [(fn? fn-edit) (is-memoized? memo-fn)] :post [(is-memoized? %)]}
    (let [current-snapshot (memo/snapshot memo-fn)
          updated-snapshot (normalise-memoized-snapshot-keys  (apply fn-edit (list* current-snapshot edit-args)))
          _ (assert (map? updated-snapshot))]
      (memo/memo-swap! memo-fn updated-snapshot)
    memo-fn))

(defn evict-memoized
  [memo-fn & cache-keys]
  (let [edit-fn (fn [snapshot]
                  {:pre [(map? snapshot)] :post [(map? %)]}
                  (reduce
                   (fn [snap key] (dissoc snap (normalise-memoized-cache-key key)))
                   snapshot
                   cache-keys))]
    (edit-memoized-snapshot memo-fn edit-fn)))

(defn lookup-memoized
  [memo-fn cache-key]
  {:pre [(is-memoized? memo-fn)]}
  (memo-fn cache-key))

(defn update-memoized
  [memo-fn new-items]
  {:pre [(map? new-items)]}
  (let [edit-fn (fn [snapshot]
                  {:pre [(map? snapshot)] :post [(map? %)]}
                  (reduce
                   (fn [snap [key value]] (assoc snap (normalise-memoized-cache-key key) value))
                   snapshot
                   new-items))]
    (edit-memoized-snapshot memo-fn edit-fn)))

(def map-memoizer-type-to-factory
  {:memo [memo/memo ]
   :fifo [memo/fifo]
   :lru [memo/lru]
   :lu [memo/lu [:lu/threshold 20]]
   :ttl [memo/ttl]})

(defn find-memoizer-factory-from-type
  [memo-type]
  {:pre [(or (fn? memo-type) (keyword? memo-type) (nil? memo-type))]
   :post [(vector? %) (fn? (first %)) (or (nil? (second %)) (vector? (second %)))]}
  (get map-memoizer-type-to-factory (or memo-type :memo)))

(defn new-memoized
  ([& {:keys [src-fn memoizer memoizer-args init-fn init-args] :as opt-args}]
     { :post [(is-memoized? %)]}
     (let [;; fake up a function if none supplied
           normal-src-fn (or src-fn (fn [& args] nil))

           _ (assert (fn? normal-src-fn))

           memoizer-factory (find-memoizer-factory-from-type memoizer)
           normal-memoizer (first memoizer-factory)

           normal-memoizer-args (or memoizer-args (second memoizer-factory))

           _ (assert (or (nil? normal-memoizer-args) (coll? normal-memoizer-args)))

           memo-fn (apply normal-memoizer (list* normal-src-fn normal-memoizer-args))

           _ (if init-fn (apply edit-memoized (list* memo-fn init-fn init-args)))]

       memo-fn)))

;; ***********************
;; FIN: memoized functions
;; ***********************

(defn make-fn-evict-memoized [memo-fn] (fn [& cache-keys] (apply evict-memoized (list*  memo-fn cache-keys))))

(defn make-fn-snapshot-memoized [memo-fn] (fn [& args] (snapshot-memoized memo-fn)))

(defn make-fn-update-memoized [memo-fn] (fn [new-items] (update-memoized memo-fn new-items)))

