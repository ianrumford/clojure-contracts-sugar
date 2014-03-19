(ns clojure-contracts-sugar.utils.manifests
  (:require [clojure-potrubi.utils.names :as potrubi-utils-names :refer (resolve-symbol-from-args resolve-name-from-any resolve-symbol-from-any resolve-name-from-keyword)]
            [clojure-carp :as carp :refer (surprise-exception)])
  (:import (java.lang Exception Class)))

;; **************
;; BEG: manifests
;; **************

(def manifest-namer-form '(fn [x] (str "manifest-" (clojure-potrubi.utils.names/resolve-name-from-any x))))
(def manifest-namer-fn (fn [x] (str "manifest-" (`resolve-name-from-any x))))
(def default-source-namer manifest-namer-fn) ;; called by other fns
(def manifest-spec-key-source :source)
(def manifest-spec-key-source-namer :source-namer) ;; the key to use if passed
(def manifest-spec-key-target :target)
(def manifest-spec-key-target-namer :target-namer) ;; the key to use if passed

(def manifest-ctrl-key-source manifest-spec-key-source)
(def manifest-ctrl-key-target manifest-spec-key-target)
(def manifest-ctrl-key-name :name)
(def manifest-ctrl-key-form :form)
(def manifest-ctrl-key-def :def)

(def default-manifest-ctrl
  {manifest-ctrl-key-target {manifest-ctrl-key-form  manifest-namer-form}})

(def manifest-spec-to-ctrl-mapping
  {manifest-spec-key-source {manifest-ctrl-key-form manifest-spec-key-source-namer}
   manifest-spec-key-target {manifest-ctrl-key-form manifest-spec-key-target-namer}})

(defn validate-manifest-vector
  [manifest-spec]
  {:pre [(vector? manifest-spec)  ] :post [(vector? %)]}
  (or  (= 2 (count manifest-spec)) (surprise-exception manifest-spec "manifest-spec wrong size - must be 2"))
  (for [manifest-entry manifest-spec]
    (or (keyword? manifest-entry) (symbol? manifest-entry) (surprise-exception manifest-entry "manifest-entry not keyword or symbol")))
  manifest-spec)

(defn normalise-manifest-spec-to-map
  [manifest-spec]
  {:post [(map? %)]}
  (let [manifest-spec-vector
        (cond
         (keyword? manifest-spec) [manifest-spec manifest-spec]
         (vector? manifest-spec) manifest-spec
         :else (surprise-exception manifest-spec "manifest-spec is wat?"))

        _ (validate-manifest-vector manifest-spec-vector)

        manifest-norm {manifest-spec-key-source (first manifest-spec-vector)  manifest-spec-key-target (second manifest-spec-vector)}]

    manifest-norm))

(defn- define-manifest-resolve-namer-var
  [namer]
  (let [namer# namer
        namer-var#
        (cond
         (nil? namer#) nil
         (symbol? namer#) (var-get (resolve namer#))
         :else (surprise-exception namer# "define-manifest-resolve-namer-var" "namer is wat?"))]
    namer-var#))

;; *****************************
;; BEG: resolve manifest control
;; *****************************

(defn resolve-manifest-ctrl
  ([ctrl-spec] (resolve-manifest-ctrl ctrl-spec {}))
  ([ctrl-spec base-spec]
     {:pre [(map? ctrl-spec) (map? base-spec)] :post [(map? %)]}
     (let [;;ctrl-keys# [manifest-spec-key-source-namer-form manifest-spec-key-target-namer-form]

           spec-to-ctrl-mapping# manifest-spec-to-ctrl-mapping

           rslv-spec#
           (into {}
                 (for [[mapper-key#  mapper-map#] spec-to-ctrl-mapping#]
                   (let [;; any spec for this key?
                         mapper-spec# (get base-spec mapper-key# {})
                         work-spec#
                         (merge mapper-spec#
                                (reduce (fn [s [tgt-key src-key]]
                                          (if-let [src-value# (get ctrl-spec src-key)]
                                            (assoc s tgt-key src-value#)
                                            s))
                                        {}
                                        mapper-map#))

                         work-form# (get work-spec# manifest-ctrl-key-form)

                         name-spec# (cond
                                     (nil? work-form#) nil
                                     (symbol? work-form#) {manifest-ctrl-key-name work-form#}

                                     ;; if a coll assume a fn definition
                                     :else
                                     (let [ctrl-name (gensym (str "fn-" (resolve-name-from-keyword mapper-key#) "-namer"))
                                           ctrl-def `(def ~ctrl-name ~work-form#)]
                                       {manifest-ctrl-key-name ctrl-name manifest-ctrl-key-def ctrl-def}))

                         done-spec# (merge work-spec# name-spec#)]

                     (if done-spec# [mapper-key# done-spec#]))))]

        rslv-spec#)))

;; *****************************
;; FIN: resolve manifest control
;; *****************************

(defn form-manifest
  [manifest-ctrl manifest-norm]
  {:pre [(map? manifest-ctrl) (map? manifest-norm)] :post [(coll? %)]}
  (let [fn-source-namer# (get-in manifest-ctrl [manifest-ctrl-key-source manifest-ctrl-key-name])
        fn-target-namer# (or (get-in manifest-ctrl [manifest-ctrl-key-target manifest-ctrl-key-name])
                             (surprise-exception manifest-spec-key-target-namer  "no target namer fn"))

        fn-source-namer-var# (define-manifest-resolve-namer-var fn-source-namer#)
        fn-target-namer-var# (define-manifest-resolve-namer-var fn-target-namer#)

        source-nom# (get manifest-norm manifest-spec-key-source)
        source-nrm# (if fn-source-namer-var# (fn-source-namer-var# source-nom#) source-nom#)

        target-nom# (get manifest-norm manifest-spec-key-target)
        target-nrm# (cond
                     (symbol? target-nom#) target-nom# ;; leave it alone
                     :else (resolve-symbol-from-any (if fn-target-namer-var# (fn-target-namer-var# target-nom#) target-nom#)))

        target-sym# (cond
                     (symbol? target-nrm#) target-nrm#
                     (list? target-nrm#) target-nrm#
                     (instance? clojure.lang.Cons target-nrm#) target-nrm#
                     (string? target-nrm#) (symbol target-nrm#)
                     :else (surprise-exception target-nrm# "form-manifest" "target-nrm# not string, list or symbol"))

        manifest-form# `(def ~target-sym# ~source-nrm#)]

    manifest-form#))

(defn form-manifests
  [ctrl & args]
  {:pre [(map? ctrl)] :post [(map? %)]}
  (let [norm-ctrl (resolve-manifest-ctrl ctrl default-manifest-ctrl)

        ;; have to eval the namer fns!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        namer-defs (remove nil?  (map manifest-ctrl-key-def (vals norm-ctrl)))

        _ (doall (for [namer-def namer-defs]
                 (do
                   (eval namer-def))))

        manifests args

        manifest-forms
        (into {}
              (for [manifest manifests]
                (let [;;manifest-norm# (resolve-manifest-norm
                      ;;ctrl manifest)

                      manifest-norm (normalise-manifest-spec-to-map manifest)

                      manifest-form# (form-manifest norm-ctrl manifest-norm)]

                  [manifest manifest-form#])))]
    manifest-forms))

(defmacro define-canonical-manifests
  [ctrl & args]
  {:pre [(map? ctrl)]}
  (let [forms# (apply form-manifests ctrl args)]
    `(do
       ~@(for [[_ def#] forms#] def#))))

(defmacro define-manifests
  [& args]
  (let [manifest-first# (first args)
        manifest-ctrl# (if (map? manifest-first#) manifest-first# {})
        manifest-args# (cond
                       (map? manifest-first#) (rest args)
                       (keyword? manifest-first#) args
                       (vector? manifest-first#) args
                       :else (surprise-exception manifest-first# "define-manifests" "manifest-first is what?"))

        form-manifest# `(define-canonical-manifests ~manifest-ctrl# ~@manifest-args#)]

    `(do
       ;;~@define-ctrl-forms#
       ~form-manifest#)))

;; **************
;; FIN: manifests
;; **************
