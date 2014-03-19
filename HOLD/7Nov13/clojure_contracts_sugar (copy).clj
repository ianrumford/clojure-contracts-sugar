(ns clojure-contracts-sugar
  (:require [clojure.core.contracts :as ccc]))

;; Sugar macros, helpers, etc for clojure.core.contracts

;; "A work in progress"

;; *******************
;; BEG: misc functions
;; *******************

(defn atom?
  [a] (instance? clojure.lang.Atom a))

(defn make-fn-find-atom
  ([] (make-fn-find-atom (atom nil)))
  ([value]
       {:pre [(atom? value)] :post [(fn? %)]}
       (fn [] value)))


(defn make-fn-find-store
  "check if store has been initialised first,
   if not does so, and returns the store"
  [store fn-initialize]
  {:pre [(atom? store)] :post [(fn? %)]}
  (fn []
    (if (nil? @store)
      (swap! store fn-initialize))
    (doall (println "SSSSSSSSSSSSSSSSSSSSSSSSSS find-store" "STORE" (class store) ))
    store))


(defn raise-exception
  "throw an exception"
  [& args]
  (throw (Exception. (apply str (interpose " " (flatten args))))))


(defn surprise-exception
  [value & args]
  (raise-exception "Surprise Exception" (class value) value args))


(defn make-fn-name-generator
  ([] (fn [x] (str x)))
  ([prefix] (make-fn-name-generator prefix nil))
  ([prefix suffix] (make-fn-name-generator prefix suffix "-"))
  ([prefix suffix separator]
     (if suffix   
       (fn [x] (str prefix separator x separator suffix))
       (fn [x] (str prefix separator x)))))


(defn make-fn-name-keyword-generator
  [& args]
  (let [fn-name (apply make-fn-name-generator args)]
    (fn [x] (keyword (fn-name (-> x str (subs 1))))))
  )

(def aspect-name-suck (make-fn-name-generator "aspect" "suck"))
(def aspect-name-spit (make-fn-name-generator "aspect" "spit"))
(def aspect-name (make-fn-name-generator "aspect" "default"))


(def aspect-mnemonic-suck (make-fn-name-keyword-generator "suck"))
(def aspect-mnemonic-spit (make-fn-name-keyword-generator "spit"))
(def aspect-mnemonic-both (make-fn-name-keyword-generator))



(defn symbolise-any
  [x]
  (cond
   (symbol? x) x
   (string? x) (-> x symbol)
   (keyword? x) (-> x str (subs 1) symbol)))


(defn keyword-to-string
  [x] (-> x str (subs 1)))


(defn resolve-any-as-symbol
  [x]
  (cond
   (symbol? x) x
   (string? x) (-> x symbol resolve)
   (keyword? x) (-> x str (subs 1) symbol resolve)))


(defn resolve-keyword-aspect
  [x]
  (-> x (str "?") (subs 1) resolve-any-as-symbol))

(defn symbol-keyword-aspect
  [x]
  (-> x (str "?") (subs 1) symbol))

(defmacro report-args
  [& args]
  (do
    (println "report-args:"  "ARGS" (class args) args)
    (doall  (map (fn [v]  (println "report-args:"   "V" (class v) v)) args))
    nil))

(defn report-arg
  [arg]
  (do
    (doall (println "REPORT-ARG" (class arg) arg))
    arg))


(def report-arg1-1 (fn [arg]
                   (do
                     (doall (println "REPORT-ARG1-1" "ARG" (class  arg) arg))
                     arg
                     )))


(def report-arg1-2 (fn [arg]
                   (do
                     (doall (println "REPORT-ARG1-2" "ARG" (class  arg) arg))
                     arg
                     )))

(def report-argn-1 (fn [& args]
                   (do
                     (doall (println "REPORT-ARGN-1" "ARG" (class  args) args))
                     args
                     )))

(defmacro report-contract-args
  [& {:syms [mnem type sig pre post constraints test test-map1 test-vec1] :as args}]
  (do
    (println "report-contract-args:"  "ARGS" (class args) args)
    (doall  (map (fn [[k v]]  (println "report-contract-args:"  "K" (class k) k "V" (class v) v)) args))
    nil))


;; *******************
;; FIN: misc functions
;; *******************



;; ******************
;; BEG test contracts
;; ******************



;;(def test-ctx1 (ccc/contract test-ctx "test-ctx" [x] [map?]))
(def test-ctx1 (fn [& args]
                    (do
                      (doall (println "TEST-CTX1" "ARGS" (count args ) args))
                      (doall (for [arg args] (doall (println "TEST-CTX1" "ARG" (class arg) arg))))
                      ;;args
                      ;;(second args)
                      ((first args) (second args))
                      )))


(def test-ctx2 (fn [& args]
                    (do
                      (doall (println "TEST-CTX2" "ARGS" (count args ) args))
                      (doall (for [arg args] (doall (println "TEST-CTX2" "ARG" (class arg) arg))))
                      ;;args
                      (second args)
                      ;;((first args) (second args))
                      ;;true
                      )))

(def test-ctx-args (fn [& args]
                    (do
                      (doall (println "TEST-CTX-ARGS" "ARGS" (count args ) args))
                      (doall (for [arg args] (doall (println "TEST-CTX-ARGS" "ARG" (class arg) arg))))
                      true
                      )))

;; ******************
;; FIN test contracts
;; ******************


;; *********************
;; BEG: aspect mnemonics
;; *********************




(def ctx-argn (ccc/contract ccccargnnnn "c1" [arg1 arg2] [(report-argn-1 arg1)]))


(def aspects-template
  [
   :map
   :keyword
   :vector
   :fn
   :set
   :coll
   :list
   :char
   :seq
   :string


      {:map-accessor-get-with-default  '[[src-map default] [(map? src-map)]]}
   
   {:report-arg '[[x] [clojure-contracts-sugar/report-arg]]}

   ;;{:report-fail '[[x'] :this-will-fail]}
   
   ]
  
  )






;; (System/exit 0)
;; (raise-exception "HOLD HERE RESOLVED TEMPLATE")



;; *********************
;; BEG: aspect mnemonics
;; *********************




;; **************************************
;; BEG: contracts store function builders
;; **************************************
  


;;(def contracts-store-key-aspect-definitions :aspect-definitions)
(def manifest-contracts-store-key-aspects-store :aspects-store)
;;(def contracts-store-key-aspect-contracts :aspect-contracts)
(def manifest-contracts-store-key-naming-functions :naming-functions)
(def manifest-contracts-store-key-naming-function-aspect :aspect)
(def manifest-contracts-store-key-naming-function-mustbe :mustbe)


(defn make-fn-contracts-store-find-keys
  "make a fn to find keys recursively - i.e. get-in -
  in a contracts store
  raises exception if a key is not found"
  [store]
  {:pre [(atom? store)] :post [(fn? %)]}
  (do
    (println "make-fn-contracts-store-find-keys:"  "STORE" (class store))
    (fn [& keys]
      (reduce (fn [s k]
                (if (contains? s k)
                  (get s k)
                  (surprise-exception k "contracts-store-find-relative-key: key not found")))
              @store
              (flatten keys)))))


(defn- make-fn-contracts-store-find-keys-or-nil
  "make a fn to find keys recursively - i.e. get-in -
  in a contracts store
  returns nil if a key is not found"
  [store]
  {:pre [(atom? store)] :post [(fn? %)]}
  (do
    (println "make-fn-contracts-store-find-keys:"  "STORE" (class store))
    (fn [& keys]
      (reduce (fn [s k]
                (if (and s (contains? s k))
                  (get s k)))
              @store
              (flatten keys)))))


(defn- make-fn-contracts-store-find-relative-key
  [store & parent-keys]
  {:pre [(atom? store)] :post [(fn? %)]}
  (do
    (println "make-fn-contracts-store-find-relative-key:" "PARENT-KEYS" parent-keys "STORE" (class store) store)

    ;; does not allow for parent-keys not yet existing
    ;; (contracts-store-find-keys store parent-keys)
    
    (let [;;parent-keys-flatten (flatten parent-keys)
          parent-keys-vector (into [] (flatten parent-keys))]
      (fn [key]
        (println "CTX STORE REL KEY FINDER FN:" "PARENT-KEYS" parent-keys-vector "KEY" key "CONJ" (conj parent-keys-vector key) )
                (println "CTX STORE REL KEY FINDER FN:" "STORE" store)
        ;;(if-not (keyword? key) (surprise-exception key "key not keyword"))
        (let [value (get-in @store (conj parent-keys-vector key))]
          (do
            (doall (println "CTX STORE REL KEY FINDER FN:" "PARENT-KEYS" parent-keys-vector "KEY" key "CONJ" (conj parent-keys-vector key)  "VALUE" (class value) value))
            value))))))


(defn- make-fn-contracts-store-update-relative-entry
  [store & parent-keys]
  {:pre [(atom? store)] :post [(fn? %)]}
  (do
    (println "make-fn-contracts-store-update-relative-entry:" "PARENT-KEYS" parent-keys "STORE" (class store))

    ;; does not allow for parent-keys not yet existing
    ;; (contracts-store-find-keys store parent-keys)
    
    (let [parent-keys-vector (into [] (flatten  parent-keys))]
      (fn [key value]
        (do
          (println "CXT STORE REL ENTRY UPDATE FN:" "KEY" key "VALUE" value "PARENT-KEYS" parent-keys-vector "CONJ" (conj parent-keys-vector key))
          ;;(if-not (keyword? value) (surprise-exception value "value not keyword"))
          (swap! store assoc-in (conj parent-keys-vector key) value))))))




(defn- make-fn-contracts-store-add-relative-entry
  "make fn to add new entry to contracts store hash-map
   if key already exist raises an exception"
  [store & parent-keys]
  {:pre [(atom? store)] :post [(fn? %)]}
  (do
    (println "make-fn-contracts-store-add-relative-entry:" "PARENT-KEYS" parent-keys "STORE" (class store))
    (let [fn-update-entry (make-fn-contracts-store-update-relative-entry store parent-keys)
          fn-find-collection (make-fn-contracts-store-find-keys-or-nil store)]
      (fn [key value]
        (let [contracts-store-collection (fn-find-collection parent-keys)]
          (if (and contracts-store-collection (contains? contracts-store-collection key))
            (surprise-exception key "fn-contracts-store-add-relative-entry:" parent-keys "key already exists")
            (fn-update-entry key value)))))))







;; **************************************
;; FIN: contracts store function builders
;; **************************************

;; *******************************
;; BEG: aspect mnemonics functions
;; *******************************


(defn- resolve-aspects-template
  [aspects]
  {:pre [(vector? aspects)] :post [(map? %)]}
  (let [aspect-norm1 (into []
                           (for [a aspects]
                             (do
                               ;;(doall (println "resolve-aspects-template" "A" (class a) a))
                               (cond
                                (keyword? a) [a :both :suck :spit]
                                (vector? a) a
                                (map? a) a
                                :else (surprise-exception a "aspect is what?")))))

        ;;_ (doall (println "resolve-aspects-template" "NORM1" (class aspect-norm1) aspect-norm1))
        
        aspect-norm2 (apply merge
                            (for [aspect-def aspect-norm1]
                              (do
                                ;;(doall (println "resolve-aspects-template" "ASPECT_DEF" (class aspect-def) aspect-def))                              
                                  (cond
                                   (map? aspect-def) aspect-def
                                   (vector? aspect-def)
                                   (let [aname (first aspect-def)
                                         aname-symbol (symbol-keyword-aspect aname)
                                         actxs (rest aspect-def)
                                         ;;_ (doall (println "resolve-aspects-template" "ANAME-SYMBOL" (class aname-symbol) aname-symbol "ANAME" (class aname) aname "ACTXS" (class actxs) actxs))
                                         ]
                                     (apply hash-map
                                            (apply concat
                                                   (for [actx actxs]
                                                     (cond
                                                      (or (nil? actx) (= :both actx)) [(aspect-mnemonic-both aname) ['[v] [aname-symbol '=> aname-symbol]]]
                                                      (= :suck actx) [(aspect-mnemonic-suck aname) ['[v] [aname-symbol]]]
                                                      (= :spit actx) [(aspect-mnemonic-spit aname) ['[v] ['=> aname-symbol]]]
                                                      :else (surprise-exception actx "actx is what?"))))))))))

        ;;_ (doall (println "resolve-aspects-template" "NORM2" (class aspect-norm2) aspect-norm2))

        ;; some validation
        _ (doall (for [[k v] aspect-norm2]
                   (do
                     (doall (println "resolve-aspects-template" "VAL" "k" (class k) k "v" (class v) v))
                     (or (keyword? k) (surprise-exception k "aspect tag is not keyword"))
                     (or (vector? v) (surprise-exception v "aspect def is not vector"))
                     (or (vector? (first v)) (surprise-exception v "aspect def first is not vector"))
                     (or (vector? (second v)) (surprise-exception v "aspect def second is not vector"))
                     )
                   ))
        
        ;; Aspect-norm3 (apply hash-map aspect-norm2)

        ;;         _ (doall (println "resolve-aspects-template" "NORM3" (class aspect-norm3) aspect-norm3))
        ]
    
    aspect-norm2
    ;;aspect-norm3
    ;;{}

    ))


;; *******************************
;; FIN: aspect mnemonics functions
;; *******************************


;; ******************
;; BEG: aspects store
;; ******************

;;(def aspects-resolved-template (resolve-aspects-template aspects-template))

(def aspects-store-initialize-value (resolve-aspects-template aspects-template))
;;(def aspects-store (atom aspects-store-initialize-value))
;;(def find-aspects-store (make-fn-find-store aspects-store initialize-aspect-store))
(def find-aspects-store (make-fn-find-atom (atom aspects-store-initialize-value)))

(defn update-aspects-store
  [aspects-template]
  (let [resolved-aspects-template (resolve-aspects-template  aspects-template)
        aspects-store (find-aspects-store)
        ]
    (doall (println "update-aspects-template" "ASPECTS-TEMPLATE" (class aspects-template) aspects-template))
    (doall (println "update-aspects-template" "RESOLVED-ASPECTS-TEMPLATE" (class resolved-aspects-template) resolved-aspects-template))
    (doall (println "update-aspects-store" "ASPECTS-STORE" (class aspects-store) aspects-store))
    (swap! aspects-store merge resolved-aspects-template)
    aspects-store))


;; ******************
;; FIN: aspects store
;; ******************


;; ********************
;; BEG: contracts store
;; ********************


;; define but don't initialize to anything useful - init when called upon
(def contracts-store-initialize-value nil)
(def contracts-store (atom contracts-store-initialize-value))


;; the fn created will be called by swap!
(defn make-fn-initialize-contracts-store
  [aspects-store]
  {:pre [(atom? aspects-store)] :post [(fn? %)]}

  (fn [& args]

    (doall (println "MMMMMMMMMMMMMMMMMMMMMinitialize-contracts-store" "ARGS" (class args) args))
    (doall (println "MMMMMMMMMMMMMMMMMMMMMinitialize-contracts-store" "ASPECT_STORE" (class aspects-store) aspects-store))

    (let [contract-store-init
          {manifest-contracts-store-key-naming-functions
           {
            manifest-contracts-store-key-naming-function-mustbe (make-fn-name-generator "mustbe" "or-croak") 
            manifest-contracts-store-key-naming-function-aspect (make-fn-name-generator "aspect" "contract")
            }
           
           ;;contracts-store-key-aspect-definitions aspects-resolved-template
           manifest-contracts-store-key-aspects-store aspects-store
           }]

          (doall (println "MMMMMMMMMMMMMMMMMMMMMinitialize-contracts-store" "CONTRACT-STORE-INIT" (class contract-store-init) contract-store-init))
contract-store-init
      )

    
    )
  )


(defmacro configure-contracts-store
  [& {:keys [aspects] :as contract-store-init}]
  (doall (println "configure-contracts-store" "CONTRACT-STORE-INIT" (class contract-store-init) contract-store-init))
  (doall  (for [[k v] contract-store-init] (doall (println "configure-contracts-store" "ARG" "k" (class k) k "v" (class v) v))))
  ;;(doall  (for [[k v] contract-store-init] (doall (println "configure-contracts-store" "VAR" "k" (class k) k "v"  (var (resolve v))))))
  ;;(doall (for [[k v] contract-store-init] (doall (println "configure-contracts-store" "VAR-GET" "k" (class k) k "v"  (var-get (resolve v))))))

  (if aspects
    (let [
          aspects-template (cond
                                    (vector? aspects) aspects
                                    (symbol? aspects) (var-get (resolve aspects))
                                    :esle (surprise-exception aspects "aspects is what?")
                                    )

      _ (doall (println "configure-contracts-template-var-get" "ASPECTS-TEMPLATE" (class aspects-template) aspects-template))
          
          aspects-store (update-aspects-store aspects-template)
          ]
      
      ;;(doall (println "configure-contracts-store" "ASPECTS-STORE" (class aspects-store) aspects-store))
      
      ))

  nil
  )

(def initialize-contracts-store (make-fn-initialize-contracts-store (find-aspects-store)))


;;(configure-contracts-store :aspects-template [:vector])

(def find-contracts-store (make-fn-find-store contracts-store initialize-contracts-store))

;;(doall  (println "CONTRACTS_STORE" (find-contracts-store)))



;;(System/exit 0)


;; ********************
;; FIN: contracts store
;; ********************


;; ******************************
;; BEG: contracts store functions
;; ******************************


;; (def contracts-store-find-aspect-definition
;;   (make-fn-contracts-store-find-relative-key (find-contracts-store) contracts-store-key-aspect-definitions))


(defn make-fn-contracts-store-find-aspect-definition
  [store]
  {:pre [(atom? store)] :post [(fn? %)]}
  (let [

        ;;fn-find-aspect-store (make-fn-contracts-store-find-relative-key store manifest-contracts-store-key-aspects-store)
        fn-find-keys (make-fn-contracts-store-find-keys store)
        fn-find-aspect-store (fn [] (fn-find-keys manifest-contracts-store-key-aspects-store))
        
        ]

    (fn [aspect]
      (get (deref (fn-find-aspect-store)) aspect nil)
      )

    ))

(def contracts-store-find-aspect-definition
  (make-fn-contracts-store-find-aspect-definition (find-contracts-store)))


;; ******************************
;; FIN: contracts store functions
;; ******************************


;; *********************
;; BEG: aspect contracts
;; *********************

;; used to provide constraints to other functions


(defn create-aspects-contracts
  [aspect-definitions]
  (let [
        ;; pre-make any explicitly given  or mnemonic contracts e.g. [[v] [map?]
        aspect-contracts
        (into {}
              (for [contract-aspect aspect-definitions]
                [contract-aspect

                 (let [contract-definition (cond
                                            (keyword? contract-aspect) (or (contracts-store-find-aspect-definition contract-aspect)
                                                                        (surprise-exception contract-aspect "create-aspect-contracts" (format  "contract-definition is nil for aspect >%s<" contract-aspect)))
                                            (vector? contract-aspect) contract-aspect)
                       
                       ;;ctx-name (symbol (gensym "app-ctx-aspect" ))
                       ctx-name (gensym "app-ctx-aspect" )
                       def-name (symbol (str "def-" ctx-name))
                       ctx-doc (str \" (str ctx-name)  \")

                       def-ctx (list* 'clojure.core.contracts/contract ctx-name ctx-doc contract-definition)

                       def-def (list 'def def-name def-ctx)
                       ;;def-ctx-quote (map #(quote %) def-ctx)
                       
                       ]

                   ;;(doall (println "create-aspect-contracts" "DEF-CTX" "CONTRACT_ASPECT" contract-aspect "DEF-CTX" def-ctx))
                   ;;(doall (println "create-aspect-contracts" "DEF-CTX-QUOTE" "CONTRACT_ASPECT" contract-aspect "DEF-CTX-QUOTE" def-ctx-quote))
                   
                   [def-name def-def def-ctx]

                   )]))
        
        ]
    
    ;;(doall (map (fn [cs rcs] (println "create-aspect-contracts" "ASPECT" (class cs) cs "CONTRACT" (class rcs) rcs)) aspect-definitions aspect-contracts))
    
    aspect-contracts

    ))


(defn resolve-aspects-contracts
  ([aspect-definitions] (resolve-aspects-contracts aspect-definitions (create-aspects-contracts aspect-definitions)))
  ([aspect-definitions contract-definitions]
      (let [
            ;; find the contact definition
            aspect-contracts
             (into {}
                  (for [aspect-definition aspect-definitions]
                    [aspect-definition (do

                                         ;;(doall (println "apply-aspect-definitions" "ASPECT-DEFINITION" (class aspect-definition) aspect-definition))
                                         
                                         (cond
                                          
                                          (symbol? aspect-definition) aspect-definition ;; have to assume its an existing contract

                                          ;;(keyword? aspect-definition) (get (get contract-definitions aspect-definition) 0)
                                          (keyword? aspect-definition) (get (get contract-definitions aspect-definition) 2)

                                          (vector? aspect-definition) (first (get contract-definitions aspect-definition))
                                          
                                          :else (surprise-exception aspect-definition "apply-aspect-definitions" "CONTRACT ASPECT is what?")))]))

            ]
        
        ;;(doall (map (fn [cs rcs] (println "resolve-aspect-contracts" "ASPECT" (class cs) cs "CONTRACT" (class rcs) rcs)) aspect-definitions aspect-contracts))
        
        aspect-contracts

        )))


(defn local-with-constraints
  "A contract combinator.

   Takes a target function and a number of contracts and returns a function with the contracts
   applied to the original.  This is the preferred way to apply a contract previously created
   using `contract` as the use of `partial` may not work as implementation details change.
  "
  ([f] f)
  ([f c]
     (do
       (doall (println "#############################>>>>>>>>>>>>>>>>>>>>>>>>>>LOCAL_WITH_CONSTRAINT" "f" f "c" c))
       (partial c f)))
  ([f c & more]

     (do
       (doall (println ">>>>>>>>>>>>>>>>>>>>>>>>>>LOCAL_WITH_CONSTRAINT" "f" f "c" c "more" (count more) more))
       (apply local-with-constraints (local-with-constraints f c) more))))


(defmacro apply-contract-aspects
  "takes a function and a list of contract definitsions
   and applies the contracts constraint
   the return value is a new function with constraint applied"
  [base-fn & aspect-definitions]
  (let [resolved-aspect-contracts (resolve-aspects-contracts aspect-definitions)

        ;;apply-form `(local-with-constraints ~base-fn test-ctx1 ~@(for [[k v] resolved-aspect-contracts] v))
        apply-form `(ccc/with-constraints ~base-fn ~@(for [[k v] resolved-aspect-contracts] v))
        ]

    (doall (map (fn [cs rcs] (println "apply-contract-aspects" "ASPECT" (class cs) cs "CONTRACT" (class rcs) rcs)) aspect-definitions resolved-aspect-contracts))

    (println "update-contract-aspects:" "APPLY-FORM" (class apply-form) apply-form)
    
    `(do
       ;; insert the made contracts
       ;;(report-contract-store-init  "MADE CONTRACT ASPECTS" ~@(for [[_ [_ c#]] aspect-contracts] c#))
       ;;~@(for [[_ [_ c# _]] aspect-contracts] c#)
 
       ;;~(list 'println ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>hello mum from apply 1")
       ;;~(list 'println ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>hello mum from apply 2")

        ;;~@(for [[_ [_ def-ctx#]] resolved-aspect-contracts] def-ctx#)

        ~apply-form)))


(defmacro update-contract-aspects
  "takes a function and a list of contract definitsions
   and uses alter-var-root to update the definition of the function
   with the contracts constraint applied
   note the return value is the new function as well"
  [base-fn & aspect-definitions]
  (let [
        ;; pre-make any explcitly given  or mnemonic contracts e.g. [[v] [map?]

        aspect-contracts (create-aspects-contracts aspect-definitions)
        resolved-aspect-contracts (resolve-aspects-contracts aspect-definitions aspect-contracts)

        ;; update-form (list `alter-var-root (list `var base-fn)
        ;;                   (list `fn '[f & c] `(local-with-constraints ~'f test-ctx1 ~@(for [[k v] resolved-aspect-contracts] v))))

        update-form (list `alter-var-root (list `var base-fn)
                          (list `fn '[f & c] `(ccc/with-constraints ~'f ~@(for [[k v] resolved-aspect-contracts] v))))
        
        
        ]
    
    (doall (map (fn [cs rcs] (println "update-contract-aspects:" "ASPECT" (class cs) cs "CONTRACT" (class rcs) rcs)) aspect-definitions resolved-aspect-contracts))

    (println "update-contract-aspects:" "UPDATE-FORM" (class update-form) update-form)
    
    `(do
       ;; insert the made contracts
       ;;(report-contract-store-init  "MADE CONTRACT ASPECTS" ~@(for [[_ [_ c#]] aspect-contracts] c#))
       ;;~@(for [[_ [_ c# _]] aspect-contracts] c#)
 
       (report-contract-store-init "UPDATE-FORM" ~update-form)
       
       ;;~(list 'println ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>hello mum from update 1")
       ;;~(list 'println ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>hello mum from update 2")

       ~update-form)))


;; (defmacro apply-contacts-aspects
;;   [& contract-store-init]
;;   nil
;;   ))


;; *********************
;; FIN: aspect contracts
;; *********************




(defn -main
  [& contract-store-init]
  (doall (println "clojures-contracts-sugar -main does nothing")))























;; PARK

;; *********************
;; BEG: mustbe contracts
;; *********************

;; (defmacro define-mustbe-contracts
;;   "Sugar to generate mustbe contract"
;;   [kontract-seq]
;;   (let [_ (println "\n\n\ndefine-mustbe-contracts: MUSTBE CONTRACTS\n\n")
;;         kontract (for [[k-mn k-fn & k-cx] kontract-seq]
;;                     (let [k-na (str "mustbe-" (name k-mn) "-or-croak")
;;                           k-sm (symbol k-na)
;;                           ;;k-ds (str "ds-" (name k-mn))
;;                           k-ct (list* `ccc/with-constraints k-fn k-cx)
;;                           k-df (list 'def k-sm k-ct)
;;                           _ (println "define-mustbe-contracts: k-na" k-na "k-df" k-df)
;;                           ]
;;                       [k-na k-df]))]
;;     `(do
;;        ~@(for [[_ c#] kontract] c#)
;;        nil)))


;; (defmacro define-mustbe-contracts
;;   "Sugar to generate mustbe contract"
;;   [kontract-seq]
;;   (let [_ (println "\n\n\ndefine-mustbe-contracts: MUSTBE CONTRACTS\n\n")
;;         kontract (for [[k-mn k-fn & k-cx] kontract-seq]
;;                    (let [k-na (str "mustbe-" (name k-mn) "-or-croak")
;;                          k-sm (symbol k-na)
;;                          ;;k-ds (str "ds-" (name k-mn))
;;                          k-ct (list* `ccc/with-constraints k-fn k-cx)
;;                          k-df (list 'def k-sm k-ct)
;;                          _ (println "define-mustbe-contracts: k-na" k-na "k-df" k-df)
;;                          ]
;;                      [k-na k-df]))]
;;     `(do
;;        ~@(for [[_ c#] kontract] c#)
;;        nil)))


;; *********************
;; FIN: mustbe contracts
;; *********************


;; *********************
;; BEG: inline contracts
;; *********************

;; (defn- inline-contract-return-the-rest
;;   "Return the rest of the contract-store-init"
;;   [& contract-store-init]
;;   (let [
;;         ;;[f & rest] (first contract-store-init)

;;         f (first contract-store-init)
;;         rest (first  (rest contract-store-init))

;;         ;;first-contract-store-init (first contract-store-init)
;;         ;;rest-contract-store-init (rest contract-store-init)

;;         ;;_ (println "inline-contract-return-the-rest: contract-store-init" (class contract-store-init) contract-store-init)
;;         ;;_ (println "inline-contract-return-the-rest: > f" (class f) f "rest" (class rest) rest)
;;         ;;(println "inline-contract-return-the-rest: first contract-store-init" (class first-contract-store-init) first-contract-store-init)
;;         ;;(println "inline-contract-return-the-rest: rest contract-store-init" (class rest-contract-store-init) rest-contract-store-init)
;;         ]
;;     (println "inline-contract-return-the-rest: <> rest" (class rest) rest "f" (class f) f "contract-store-init" (class contract-store-init) contract-store-init)

;;     rest
;;     ))


;; (defn- inline-contract-execute-function
;;   "Execute the function specified in the call"
;;   [contract-store-init]
;;   (let [[f & rest] (first contract-store-init)
;;         ret (apply f rest)]
;;     (println "inline-contract-execute-function: <> ret" (class ret) ret "f" (class f) f "rest" (class rest) rest)
;;     ret
;;     ))

;; (defmacro define-inline-contracts
;;   "Sugar to generate inline contracts

;;    For example:

;;      (enforce-this-contract my-function my-function-contract-store-init)

;;    Contract-Store-Init to this macro:  a vector of vectors, each internal vector

;;     1. k-mn - name of the contrcat function to create e.g. enforce-this-contract
;;     2. k-cx - contracts to apply (as in with-constraints)

;;     Generates an anonymous function to call my-function with parmaters my-function-contract-store-init
;;     Then applies the contracts using with-consttraints
;;    "
;;   [kontracts-seq]
;;   (let [_ (println "\n\n\nINLINE CONTRACTS\n\n")
;;         kontracts (for [[k-mn & k-cx] kontracts-seq]
;;                     (let [k-na  (name k-mn)
;;                           k-sm (symbol k-na)
;;                           ;;k-ds (str "the contract for " (name k-mn))
;;                           k-fa (symbol (str k-na "-fn"))
;;                           ;;k-fn identity
;;                           k-fn (list `fn k-fa '[& contract-store-init] (list `inline-contract-execute-function 'contract-store-init))
;;                           k-ct (list* `ccc/with-constraints k-fn k-cx)
;;                           k-df (list 'def k-sm k-ct)
;;                           _ (println "define-inline-contracts: k-na" k-na "k-df" k-df)
;;                           ]
;;                       [k-na k-df]))]
;;     `(do
;;        ~@(for [[_ c#] kontracts] c#)
;;        nil)))


;; *********************
;; FIN: inline contracts
;; *********************































;; ;; ********************
;; ;; BEG: basic contracts
;; ;; ********************

;; ;; basic contracts expect a fn as the argument

;; (defn define-basic-contract-make-constraint
;;   [& {:syms [pre post]}]

;;   (println "define-basic-contract-make-constraint:"  "PRE" (class pre) pre)
;;   (println "define-basic-contract-make-constraint:"  "POST" (class post) post)

;;   (let [pre-constraint (cond
;;                         (keyword? pre) (resolve-keyword-aspect pre)
;;                         (symbol? pre) (resolve pre)
;;                         :else (raise-exception "pre" pre)
;;                         )

;;         post-constraint (cond
;;                          (:keyword? post) (resolve-keyword-aspect post)
;;                          (symbol? post) (resolve post)
;;                          :else (raise-exception "post" post)
                         
;;                          )
        
;;         constraint-vec [pre-constraint '=> post-constraint]

;;         ]

;;     (println "define-basic-contract-make-constraint:"  "CONSTRAINT-VEC" (class constraint-vec) constraint-vec)
;;     constraint-vec))

;; (defmacro define-basic-contract-with-named-arguments
;;   [& {:syms [name type sig pre post test test-map1 test-vec1] :as contract-store-init}]

;;   (println "define-basic-contract-with-named-arguments:"  "CONTRACT-STORE-INIT" (class contract-store-init) contract-store-init)
  
;;   (let [contract-constraints  (define-basic-contract-make-constraint 'pre pre 'post post)
;;         contract-name (gensym (str name)) 
;;         contract-doc-string (str \"   contract-name \")
;;         contract-signature   sig
;;         def-name (symbolise-any name)]

;;     (println "define-basic-contract-with-named-arguments:" "DEF-NAME" (class def-name) def-name)
;;     (println "define-basic-contract-with-named-arguments:" "CONTRACT-NAME" (class contract-name) contract-name)
;;     (println "define-basic-contract-with-named-arguments:" "CONTRACT-DOC-STRING" (class contract-doc-string) contract-doc-string)
;;     (println "define-basic-contract-with-named-arguments:" "CONTRACT-SIGNATURE" (class contract-signature) contract-signature)
;;     (println "define-basic-contract-with-named-arguments:" "CONSTRAINT" (class contract-constraints) contract-constraints)
    
;;     `(do
;;        (report-contract-contract-store-init name ~contract-name type ~contract-doc-string sig ~contract-signature constraints ~contract-constraints)
;;        (report-contract-store-init ~contract-name ~contract-doc-string ~contract-signature ~contract-constraints)
;;        (def ~def-name (ccc/contract ~contract-name ~contract-doc-string ~contract-signature ~contract-constraints))
;;        nil)))


;; (defmacro define-basic-contracts
;;   "Sugar to generate basic contracts

;;     k-mn - name of the contract function
;;     k-sg - signature of k-mn
;;     k-ct - constraints for the contracts

;;   "
;;   [kontracts-seq]
;;   (let [_ (println "\n\n\nDEFINE BASIC CONTRACTS\n\n")
;;         _ (println "define-basic-contracts: KON SEQ" kontracts-seq)
        
;;         kontracts (for [[k-mn k-sg k-ct] kontracts-seq]
;;                     (let [k-na (name k-mn)
;;                           k-sm (symbol k-na)
;;                           ;;k-sc (symbol (str k-na "-cx"))
;;                           ;;k-sc (symbol (str k-na ))
;;                           k-sc (gensym  k-na )
;;                           k-ds (str \" "ds-" (name k-mn) \")
;;                           k-cx (list `ccc/contract k-sc k-ds k-sg k-ct)
;;                           k-df (list 'def k-sm k-cx)
;;                           _ (println "define-basic-contracts: k-na" k-na "k-df" k-df)
;;                           ;;_ (println "define-basic-contracts: k-na" k-na "k-cs" k-cx)
;;                           ]
;;                       [k-na k-df]
;;                       ;;[k-na k-cx]

;;                       ))]
;;     `(do
;;        ~@(for [[_ c#] kontracts] c#)
;;        nil)))



;; (defmacro define-simple-contract
;;   [& contract-store-init]
;;   (do
;;     (println "define-simple-contract:" "CONTRACT-STORE-INIT" (class contract-store-init) contract-store-init)

;;     (let [kontracts-defs (for [[signature constraint] (partition 2 contract-store-init)]
;;                            (do
;;                              (println "define-simple-contract:" "SIGNATURE" (class signature) signature)
;;                              (println "define-simple-contract:" "CONSTRAINT" (class constraint) constraint)
;;                              )
;;                            )

;;           ]

;;       )

;;     ))

;; (defmacro defmacro-anonymous-contract
;;   [& contract-store-init]
;;   (do
;;     (doall (println "defmacro-anonymous-contract"  "CONTRACT-STORE-INIT" contract-store-init ))
;;     (report-contract-store-init contract-store-init)
;;     (let [ctx-name (gensym "ctx-anon")
;;           ctx-doc (str \" ctx-name \")
;;           ctx-definition contract-store-init
;;           ctx-signature (first ctx-definition)
;;           ctx-constraints (second ctx-definition)
;;           ]
;;       `(do
;;          (report-contract-store-init "defmacro-anonymous-contract" ccc/contract ~ctx-name ~ctx-doc ~@ctx-definition)
;;          (report-contract-store-init "defmacro-anonymous-contract" ~@ctx-signature)
;;          (report-contract-store-init "defmacro-anonymous-contract" ~@ctx-constraints)
;;          (ccc/contract ~ctx-name ~ctx-doc ~@ctx-definition)
;;          ))))

;; ;; (defn define-anonymous-contract
;; ;;   [& contract-store-init]
;; ;;   (do
;; ;;     (doall (println "define-anonymous-contract"  "CONTRACT-STORE-INIT" contract-store-init ))
;; ;;     (let [ctx-name (gensym "ctx-anon")
;; ;;           ctx-doc (str \" ctx-name \")
;; ;;           ctx-constraints contract-store-init
;; ;;           ctx-fn `(ccc/contract ~ctx-name ~ctx-doc ~@ctx-constraints)
;; ;;           _ (doall (println "define-anonymous-contract"  "CONTRACT-STORE-INIT" contract-store-init "CTX" (class ctx-fn) ctx-fn))
;; ;;           ]
;; ;;       ctx-fn)))

;; (defn defn-anonymous-contract
;;   [contract-store-init]
;;   (do
;;     (doall (println "defn-anonymous-contract"  "CONTRACT-STORE-INIT" contract-store-init ))
;;     ;;(report-contract-store-init contract-store-init)
;;     (let [ctx-name (gensym "ctx-anon")
;;           ctx-doc (str \" ctx-name \")
;;           ctx-constraints contract-store-init
;;           ]
;;       (do
;;             (doall (println "defn-anonymous-contract"  "CTX-NAME" (class ctx-name) ctx-name "CTX-DOC" ctx-doc "CTX-CONSTRAINTS" (class ctx-constraints) ctx-constraints))
;;          ;;(report-contract-store-init ccc/contract ~ctx-name ~ctx-doc ~@ctx-constraints)
;;             ;;(ccc/contract ctx-name "hi mum" ctx-constraints)
;;             (ccc/contract ctx-name "hi mum" (first ctx-constraints) (second ctx-constraints))
;;             ;;nil
;;          ))))

;;   ;; ********************
;;   ;; BEG: basic contracts
;;   ;; ********************







;; (defmacro apply-contract-aspects
;;   [base-fn & contract-aspects]
;;   (let [

;;         resolved-contract-aspects
;;         (into []
;;               (for [contract-aspect contract-aspects]
;;                 (do

;;                   (doall (println "apply-contract-aspects" "CONTRACT-ASPECT" (class contract-aspect) contract-aspect))
                  
;;                   (cond
                   
;;                    (symbol? contract-aspect) contract-aspect ;; have to assume its an existing contract

;;                    ;;(keyword? contract-aspect) (symbolise-any (aspect-name (keyword-to-string contract-aspect)))
;;                    (keyword? contract-aspect)
;;                    (let [contract-fn (contracts-store-find-aspect-contract contract-aspect)]
;;                      (or contract-fn
;;                          (surprise-exception contract-fn "apply-contract-aspects" (format  "contract-fn is nil for aspect >%s<, contract-aspect"))))
                   
;;                    :else (surprise-exception  contract-aspect "apply-contract-aspects" "CONTRACT ASPECT is what?")))))

;;         ]
    
;;     ;;;;;;;;(println "apply-contract-aspects:" "CONSTRAIN-FN" (class constrain-fn) constrain-fn)
;;     (println "apply-contract-aspects:" "BASE-FN" (class base-fn) base-fn)
;;     (println "apply-contract-aspects:" "CONTRACT-ASPECTS" (class contract-aspects) contract-aspects)
;;     #_(doall (for [contract-aspect contract-aspects] (println "apply-contract-aspects:" "CONTRACT-ASPECT" (class contract-aspect) contract-aspect) ))

;;     (println "apply-contract-aspects:" "RESOLVED-CONTRACT-ASPECTS" (class resolved-contract-aspects) resolved-contract-aspects)
;;     #_(doall (for [resolved-contract-aspect resolved-contract-aspects]
;;              (println "apply-contract-aspects:" "RESOLVED-CONTRACT-ASPECT" (class resolved-contract-aspect) resolved-contract-aspect) ))

;;     (doall (map (fn [cs rcs] (println "apply-contract-aspects:" "CONTRACT-ASPECT" (class cs) cs "RESOLVED" (class rcs) rcs)) contract-aspects resolved-contract-aspects))
    
;;     `(do
;;        ;;(def ~constrain-fn (ccc/with-constraints ~base-fn ~@resolved-contract-aspects))

;;        ;;(ccc/with-constraints ~base-fn ~@resolved-contract-aspects)
;;        (report-contract-store-init ~base-fn ~@resolved-contract-aspects)
;;        (report-contract-store-init ccc/provide [~base-fn "\"providing aspects to base-fn\"" ~@resolved-contract-aspects])
;;        (ccc/provide [~base-fn "\"providing aspects to base-fn\"" ~@resolved-contract-aspects])
;;        ;;(first ~@resolved-contract-aspects)
       
;;        ))



;;   )

































































;; PARK

;; (defmacro define-aspect-contracts
;;   "define aspect contracts
;;    uses define-basic-contract but allows for a fn to name the aspect contract
;;    "
;;   ;;([kontracts-seq] (define-aspect-contracts kontracts-seq identity))
;;   ([kontracts-seq fn-contract-name]
;;      (let [
;;            _ (doall (println "define-aspect-contracts: NAME FN" (class fn-contract-name) fn-contract-name))

;;            kontracts-seq-aspect (into []
;;                                       (for [[k-mn k-sg k-ct] kontracts-seq]
;;                                         [((->  fn-contract-name symbol resolve) k-mn) k-sg k-ct]))

;;            _ (println "define-aspect-contracts: ASPECT DEFS" (class kontracts-seq-aspect) kontracts-seq-aspect)

;;            ]

;;        ;;(eval (list `define-basic-contracts  kontracts-seq-aspect)) ;; THIS WORKS

;;        `(define-basic-contracts ~kontracts-seq-aspect)

;;        )))


;; (defn defined-named-aspects-create-fn-name
;;   ([type] (defined-named-aspects-create-fn-name type "aspect"))
;;   ([type prefix]
;;       (fn [name] (str prefix "-" type "-" name))))

;; (defmacro define-named-aspects
;;   [type & names]
;;   (let [

;;         _ (println "define-named-aspects: TYPE" (class type) type)

;;         fn-constraint (cond
;;                        (= type :suck) (fn [x] [(resolve-keyword-aspect x) ]  )
;;                        (= type :spit) (fn [x] ['=> (resolve-keyword-aspect x) ]  )
;;                        (= type :both) (fn [x] [(resolve-keyword-aspect x) '=> (resolve-keyword-aspect x) ]  )

;;                        ;;:else (list :fred)
;;                        )

;;         fn-name (defined-named-aspects-create-fn-name type "aspect")

;;         kontracts-seq (into []
;;                             (for [n names]

;;                               ;;[n '[x] :fred]
;;                               ;;[n '[x] (fn-constraint n)]
;;                               [(fn-name n) '[x] (fn-constraint n)]

;;                               )
;;                             )

;;         _ (println "define-named-aspects: KON SEQ" kontracts-seq)

;;         ]

;;     `(define-basic-contracts  ~kontracts-seq)


;;   ))


;; (def aspect-mnemonics-contracts
;;   {

;;    :report-arg (ccc/contract ccccarg "c1" [v] [report-arg])
;;    ;;:report-argn (ccc/contract ccccargn "c1" [arg1 arg2] [report-argn-1])
;;    :report-argn ctx-argn 
;;    :report-contract-store-init test-ctx-contract-store-init
;;    :report-arg1-1 (ccc/contract cccc1 "c1" [v] [report-arg1-1])
;;    :report-arg1-2 (ccc/contract cccc2 "c1" [v] [report-arg1-2])

;;    }


;;   )



;; (defn make-fn-contracts-store-find-aspect-contract
;;   [store]
;;   {:pre [(atom? store)] :post [(fn? %)]}
;;   (do
;;     (doall (println "make-fn-contracts-store-find-aspect-contract:" "STORE" (class store)))
;;     (let [fn-find-aspect-definition (make-fn-contracts-store-find-relative-key store contracts-store-key-aspect-definitions)
;;           fn-find-aspect-contract (make-fn-contracts-store-find-relative-key store contracts-store-key-aspect-contracts)
;;           fn-add-aspect-contract (make-fn-contracts-store-add-relative-entry store contracts-store-key-aspect-contracts)
;;           ]
;;       (fn [aspect]
;;         (let [

;;               _ (doall (println "contracts-store-find-aspect-contract:" "ASPECT" aspect))

;;               contract-found (or (fn-find-aspect-contract aspect)
;;                                  (let [contract-definition (fn-find-aspect-definition aspect)]
;;                                    (if contract-definition
;;                                      (let [
;;                                            _ (println "contracts-store-find-aspect-contract:" "ASPECT" aspect "CONTRACT-DEFINITION" contract-definition)
;;                                            ;;contract-fn (define-basic-contracts contract-definition)
;;                                            ;;contract-fn (define-basic-contracts [[v] [map?]])
                                           
;;                                            ;;contract-fn1 `(do (define-anonymous-contract ~@contract-definition))
;;                                            ;;contract-fn1  (defmacro-anonymous-contract ~@contract-definition)
;;                                            contract-fn1  (defn-anonymous-contract contract-definition)

;;                                            _ (doall (println "contracts-store-find-aspect-contract:" "DEFN " "ASPECT" aspect "CONTRACT-FN1" contract-fn1))
;;                                            contract-fn2 test-ctx1
;;                                            _ (doall (println "contracts-store-find-aspect-contract:" "TEST " "ASPECT" aspect "CONTRACT-FN2" contract-fn2))

;;                                            ;;contract-fn3  (defmacro-anonymous-contract [v] [report-arg])
;;                                            ;;contract-fn3  `(defmacro-anonymous-contract ~@contract-definition)
;;                                            contract-fn3 (let [[contract-signature contract-constraints] contract-definition

;;                                                               contract-str (format "(do (clojure-contracts-helpers/defmacro-anonymous-contract %s %s))" contract-signature contract-constraints)

;;                                                           _ (doall (println "contracts-store-find-aspect-contract:" "MACRO" "ASPECT" aspect "CONTRACT-STR" contract-str))
;;                                                               ]

;;                                                           (-> contract-str read-string eval)
;;                                                           ;;nil
;;                                                           )


;;                                            _ (doall (println "contracts-store-find-aspect-contract:" "MACRO" "ASPECT" aspect "CONTRACT-FN3" contract-fn3))
                                           
;;                                            contract-fn contract-fn3

;;                                            ]
;;                                        (do
;;                                          (fn-add-aspect-contract aspect contract-fn)
;;                                          contract-fn)))))

;;               ;;contract-found (fn [x] x)
;;               contract-found-test test-ctx1

;;               _ (doall (println "contracts-store-find-aspect-contract:" "ASPECT" aspect "CONTRACT-FOUND-TEST" contract-found-test))
;;               _ (doall (println "contracts-store-find-aspect-contract:" "ASPECT" aspect "CONTRACT-FOUND" contract-found))
              
;;               ]
;;           ;; could be nil
;;           contract-found-test
;;           contract-found
;;           )))))


;; (defmacro local-provide
;;   "Provides the Var manipulation macro offering ex post facto application of contracts
;;    to existing functions."
;;   [& kontracts]
;;   (let [
;;         _ (doall (println "LOCAL_PROVIDE" "BEG KONTRACTS" kontracts))
;;         fn-names  (map first kontracts)
;;         kontracts (for [[n ds & more] kontracts]
;;                     (do
;;                       ;;(doall (println "LOCAL_PROVIDE" "PRO KONTRACTS" "n" n "ds" ds "more" (class more) more))
;;                       (if (vector? (first more))
;;                         (list* `contract n ds more)

;;                         (first more)
;;                         ;;more

;;                         )))

;;         _ (doall (println "LOCAL_PROVIDE" "FIN KONTRACTS" kontracts))
;;         ]
;;     `(do

;;        (report-contract-store-init
;;         "LOCAL_PROVIDE CTXSSSSSSSSSSSSSSSSSSSSSSSSSSSSs"
;;         ~@(for [[n# c#] (zipmap fn-names kontracts)]
;;             (list `alter-var-root (list `var n#)
;;                   (list `fn '[f c] `(list ccc/with-constraints ~'f ~'c)) c#)))
       
;;        ;; ~@(for [[n# c#] (zipmap fn-names kontracts)]
;;        ;;     (list `alter-var-root (list `var n#)
;;        ;;           (list `fn '[f c] (list `ccc/with-constraints 'f 'c)) c#))

;;        ~@(for [[n# c#] (zipmap fn-names kontracts)]
;;            (list `alter-var-root (list `var n#)
;;                  (list `fn '[f c] (list `ccc/with-constraints 'f 'c)) c#))
       
       
;;        nil)))

;; (def aspect-mnemonics-template
;;   {
;;    :map nil
;;    :keyword nil
;;    :vector nil
;;    :string nil
;;    :seq nil
;;    :fn nil
;;    :ifn nil

;;    :suck-map :suck
;;    :spit-map :spit

;;    :map-test1 '[[x] [map? => map?]]
   
;;    ;;:suck-map '[[x] [map?]]
;;    ;;:spit-map '[[x] [=> map?]]
   
;;    :report-map '[[x] [clojure-contracts-helpers/report-arg => map?]]
;;    :report-arg '[[x] [clojure-contracts-helpers/report-arg]]
;;    :report-argn '[[x] [(clojure-contracts-helpers/report-argn-1)]]
   
;;    ;;:report-arg '[[x] ['report-arg]]
;;    ;;:report-arg ['[x] [(partial println ( str \"  "REPORT_ARG PRINTLN" \"))]]
   
;;    :report-ctx1-1 '[[x] [clojure-contracts-helpers/report-arg1-1]]
;;    :report-ctx1-2 '[[x] [clojure-contracts-helpers/report-arg1-2]]


;;    }
;;   )




;; (defn resolve-aspect-mnemonic-definitions
;;   [aspects]
;;   {:pre [(map? aspects)] :post [(map? %)]}
;;   (into {} (for [[k v] aspects]
;;              (do
;;                ;;(doall (println "resolve-aspect-mnemonic-definitions" "K" (class k) k "V" (class v) v))
;;                (cond
;;                 (or (nil? v) (= :both v)) [k ['[v] [(symbol-keyword-aspect k) '=> (symbol-keyword-aspect k)]]]
;;                 (= :suck v) [k ['[v] [(symbol-keyword-aspect k)]]]
;;                 (= :spit v) [k ['[v] ['=> (symbol-keyword-aspect k)]]]
;;                 :else [k v])))))


        ;; made-aspect-definitions
        ;; (into {}
        ;;       (for [contract-aspect aspect-definitions]
        ;;         [contract-aspect

        ;;          (let [contract-definition
        ;;                (cond
        ;;                 (keyword? contract-aspect) (or (contracts-store-find-aspect-definition contract-aspect)
        ;;                                                (surprise-exception contract-aspect "apply-aspect-definitions" (format  "contract-definition is nil for aspect >%s<" contract-aspect)))
        ;;                 (vector? contract-aspect) contract-aspect)
                       
        ;;                ctx-name (symbol (gensym "app-ctx-aspect" ))
        ;;                def-name (symbol (str "def-" ctx-name))
        ;;                ctx-doc (str \" (str ctx-name)  \")

        ;;                ctx-def (list* 'clojure.core.contracts/contract ctx-name ctx-doc contract-definition)

        ;;                def-def (list 'def def-name ctx-def)
        ;;                ctx-def-quote (map #(quote %) ctx-def)
                       
        ;;                ]

        ;;            (doall (println "apply-aspect-definitions" "CTX-DEF" "CONTRACT_ASPECT" contract-aspect "CTX-DEF" ctx-def))
        ;;            (doall (println "apply-aspect-definitions" "CTX-DEF-QUOTE" "CONTRACT_ASPECT" contract-aspect "CTX-DEF-QUOTE" ctx-def))
                   
        ;;            ;;[def-name (list 'def def-name (list* 'clojure.core.contracts/contract ctx-name ctx-doc contract-definition))]
        ;;            ;;[def-name (list* 'clojure.core.contracts/contract ctx-name ctx-doc contract-definition)]
        ;;            ;;[def-name `(list* ccc/contract ~ctx-name ~ctx-doc '~@contract-definition)]

        ;;            [def-name def-def ctx-def]

;;            )]))


        
        ;; resolved-aspect-definitions
        ;; (into {}
        ;;       (for [contract-aspect aspect-definitions]
        ;;         [contract-aspect (do

        ;;                            (doall (println "apply-aspect-definitions" "CONTRACT-ASPECT" (class contract-aspect) contract-aspect))
                                   
        ;;                            (cond
                                    
        ;;                             (symbol? contract-aspect) contract-aspect ;; have to assume its an existing contract

        ;;                             (keyword? contract-aspect) (get (get aspect-contracts contract-aspect) 0)

        ;;                             (vector? contract-aspect) (first (get aspect-contracts contract-aspect))
                                    
        ;;                             :else (surprise-exception contract-aspect "apply-aspect-definitions" "CONTRACT ASPECT is what?")))]))


;; (defmacro configure-contracts-store
;;   [& {:keys [aspects-template] :as contract-store-init}]
;;   (doall (println "configure-contracts-store" "CONTRACT-STORE-INIT" (class contract-store-init) contract-store-init))
;;   (doall  (for [[k v] contract-store-init] (doall (println "configure-contracts-store" "ARG" "k" (class k) k "v" (class v) v))))
;;   ;;(doall  (for [[k v] contract-store-init] (doall (println "configure-contracts-store" "VAR" "k" (class k) k "v"  (var (resolve v))))))
;;   (doall (for [[k v] contract-store-init] (doall (println "configure-contracts-store" "VAR-GET" "k" (class k) k "v"  (var-get (resolve v))))))

;;   (doall

;;    (doall (for [[k v] contract-store-init]
;;             ;;(doall (println "HDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDd"))
;;             (let [resolve-v (resolve v)
;;                   var-get-v (var-get resolve-v)

;;                   ]


;;               (doall (println "configure-contracts-store" "RESOLV V" "k" (class k) k "v" (class resolve-v) resolve-v))
;;               (doall (println "configure-contracts-store" "VAR_GET V" "k" (class k) k "v" (class var-get-v) var-get-v))

;;               )

;;             ;;(doall (println "configure-contracts-store" "RESOLV ARG" "k" (class k) k "v" (class (resolve v)) (resolve v)))

;;             ;;(doall (println "configure-contracts-store" "VAR" "k" (class k) k "v" (class (var v)) (var v)))


;;             ))


   
   
;;    (println "configure-contracts-store" "ASPECTS-TEMPLATE" (class aspects-template) aspects-template)
;;    ;;(println "configure-contracts-store" "ZZZZZZZZZZZZZZZZASPECTS-TEMPLATE" (class aspects-template) aspects-template)

;;    )

  
;;   (let [
;;         aspects-template-var-get (var-get (resolve aspects-template))
;;         aspects-store (update-aspects-store aspects-template-var-get)
;;         ]
    
;;     (doall (println "configure-contracts-store" "ASPECTS-STORE" (class aspects-store) aspects-store))
    
;;     )
  
;;   )



;;(configure-contracts-store :aspects-template aspects-template)


;; (def contracts-store-template
;;   {manifest-contracts-store-key-naming-functions {
;;             manifest-contracts-store-key-naming-function-mustbe (make-fn-name-generator "mustbe" "or-croak") 
;;             manifest-contracts-store-key-naming-function-aspect (make-fn-name-generator "aspect" "contract")

;;             }
;;    contracts-store-key-aspect-definitions aspects-resolved-template
;;    ;;;;contracts-store-key-aspect-contracts aspect-mnemonics-contracts
;;    }
;;   )


;; (defn new-contracts-store
;;   ([] (new-contracts-store contracts-store-template))
;;   ([template] (atom (merge contracts-store-template template)))
;;   )



;;(def aspect-definitions (resolve-aspect-mnemonic-definitions aspect-mnemonics-template))
;;(def aspects-resolved-template (resolve-aspects-template aspects-template))



;;(def contracts-store (find-contracts-store))