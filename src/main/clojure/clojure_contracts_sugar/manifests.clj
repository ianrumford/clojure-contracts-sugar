(ns clojure-contracts-sugar.manifests)

;; Manifest for clojure-contracts-sugar

;; **************
;; BEG: manifests
;; **************
(def manifest-contracts-store-key-aspects-store :aspects-store)
(def manifest-contracts-store-key-constraints-store :constraints-store)
(def manifest-contracts-store-key-naming-functions :naming-functions)
(def manifest-contracts-store-key-naming-function-aspect :aspect)
(def manifest-contracts-store-key-naming-function-mustbe :mustbe)

(def manifest-key-suck :suck)
(def manifest-key-spit :spit)
(def manifest-key-both :both)

(def manifest-aspect-assertion-type-suck manifest-key-suck)
(def manifest-aspect-assertion-type-spit manifest-key-spit)
(def manifest-aspect-assertion-type-both manifest-key-both)

;; do not include both
(def manifest-aspect-assertion-types
  #{manifest-aspect-assertion-type-suck
    manifest-aspect-assertion-type-spit})

(def manifest-aspect-argument-type-suck manifest-key-suck)
(def manifest-aspect-argument-type-spit manifest-key-spit)

(def manifest-aspect-argument-types manifest-aspect-assertion-types)

(def manifest-aspect-form-key-suck manifest-key-suck)
(def manifest-aspect-form-key-spit manifest-key-spit)

(def manifest-aspect-form-keys
  #{manifest-aspect-form-key-suck
    manifest-aspect-form-key-spit})

(def manifest-aspect-specification-key-name :def-name)
(def manifest-aspect-specification-key-form-def :form-def)
(def manifest-aspect-specification-key-form-ctx :form-ctx)

(def manifest-aspect-specification-keys
  #{manifest-aspect-specification-key-name
    manifest-aspect-specification-key-form-def
    manifest-aspect-specification-key-form-ctx})

;; **************
;; FIN: manifests
;; **************

