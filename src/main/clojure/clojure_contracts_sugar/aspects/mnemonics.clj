(ns clojure-contracts-sugar.aspects.mnemonics
  (:require [clojure-contracts-sugar.manifests :as manifests
             :refer (
                     manifest-aspect-assertion-type-both
                     manifest-aspect-assertion-type-suck
                     manifest-aspect-assertion-type-spit

                     )]

            [clojure-contracts-sugar.utils.utils :as utils
             :refer (make-fn-name-keyword-generator
                     resolve-aspect-symbol-from-keyword)]
            [clojure-carp :as carp :refer (surprise-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]))

;; Aspect Mnemonics Support for clojure-contracts-sugar

;; Enable tracing during macro expansion

;; ;; *********************
;; ;; BEG: aspect mnemonics
;; ;; *********************

;; (def aspects-template
;;   [

;;    ;; {:map-test1 '[[v] [(map? v) => (map? %)]]}
;;    ;; {:vector-test1 '[[v] [(vector? v) => (vector? %)]]}
;;    ;; {:suck-map-spit-vector-test1 '[[v] [(map? v) => (vector? %)]]}
;;    ;; {:suck-map-spit-vector-test2 '[[v] [(map? v) => vector? ]]}

;;    ;; {:suck-map-string-test1 {:suck [:map :string] }}
;;    ;; {:suck-map-string-spit-vector-test1 {:suck [:map :string] :spit :vector}}

;;    ;;{:not-nil '[[x] [((complement nil?) x)]]}

;;    ]

;;   )

;; ;; *********************
;; ;; FIN: aspect mnemonics
;; ;; *********************

;; ;; *********************
;; ;; BEG: naming functions
;; ;; *********************

;; ;; *********************
;; ;; FIN: naming functions
;; ;; *********************

;; ;; ********************************************
;; ;; BEG: compile aspect mnemonics to definitions
;; ;; ********************************************

;; (defn validate-aspect-mnemonics-definitions
;;   (let [aspect-norm1
;;                 (do
;;                   (cond
;;                    (keyword? a) [a manifest-aspect-assertion-type-both
;;                                  ;;manifest-aspect-assertion-type-suck
;;                                  ;;manifest-aspect-assertion-type-spit
;;                                  ]
;;                    (vector? a) a
;;                    (map? a) a

;;         aspect-norm2
;;         (apply merge
;;                  (do
;;                    (cond
;;                     (map? aspect-def) aspect-def
;;                           actxs (rest aspect-def)]
;;                       (apply hash-map
;;                              (apply concat
;;                                       (cond
;;                                        ;;[(aspect-mnemonic-both aname) ['[v] [aname-symbol '=> aname-symbol]]]
;;                                        ;;[(aspect-mnemonic-both aname) {:suck aname-symbol  :spit aname-symbol}]

;;                                        ;;[(aspect-mnemonic-both aname) {:suck [[ aname-symbol]]  :spit [[ aname-symbol]]}]
;;                                        ;;[(aspect-mnemonic-both aname) aname-symbol]
;;                                        [
;;                                         ;;(aspect-constraint-suck aname) [ aname-symbol]
;;                                         ]
;;                                        [
;;                                         ;;(aspect-constraint-spit aname) [ aname-symbol]
;;                                         ]
;;                                        ;;(= manifest-aspect-assertion-type-spit actx) [(aspect-mnemonic-spit aname) ['[v] [aname-symbol]]]

;;         ;; some validation
;;         _ (doall (for [[k v :as kv] aspect-norm2]
;;                    (do
;;                      #_(cond
;;                         (vector? v) (do
;;                         ;;(keyword? v) v
;;                         (symbol? v) v
;;                      )))

;;         ;;

;;         ]

;;     aspect-definitions
;;     ))

;; ;; ********************************************
;; ;; FIN: compile aspect mnemonics to definitions
;; ;; ********************************************

