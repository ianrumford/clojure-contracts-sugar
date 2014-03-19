(ns manifests-tests1
  (:use [clojure.test :only [deftest is function?]])
  (:require [clojure-carp :as carp :refer (surprise-exception trace-exception contract-exception)] 
            [clojure-potrubi.traces.trace :as trace :refer (trace-mark trace-entr trace-exit trace-call trace-body trace-value trace-value-entr trace-value-exit trace-value-call trace-value-body macro-set-trace trace-configure show-trace)]
            [clojure-potrubi.utils.names :as potrubi-utils-names :refer (resolve-name-from-any)]
            [clojure-contracts-sugar.utils.manifests :as ccs-utils-manifests :refer (define-manifests)]
            [clojure-potrubi.traces.diagnostics :as potrubi-traces-diagnostics :refer (macro-set-diagnostics)])
  
  )


(macro-set-trace true *ns* "ENTR")
(trace-configure :first-telltale-format-specification "%-40s")
;;(macro-set-diagnostics false)

(def base-fn1
  (fn [x]
    (println "THIS IS FN1: x" (class x) x)
    x))


(def fn-test3 base-fn1)


(declare manifest-man1)

(defn fn-source-name [x] (str "source-namer-" (resolve-name-from-any x)))

;;(defn fn-source-symbol [x] (symbol (fn-source-name x)))


(defn fn-target-name [x] (str "target-namer-"  (resolve-name-from-any x)))

(def fn-src-namer1 `(fn [~'x] (symbol (str "src-namer1-" (resolve-name-from-any ~'x)))))
(def fn-tgt-namer1 `(fn [~'x] (str "tgt-namer1-" (resolve-name-from-any ~'x))))

(def fn-src-namer2 `(fn [~'x] (str "src-namer2-" (resolve-name-from-any ~'x))))
(def fn-tgt-namer2 `(fn [~'x] (str "tgt-namer2-" (resolve-name-from-any ~'x))))

(def fn-tgt-namer3 (fn [x] (str "tgt-namer3-" (clojure-potrubi.utils.names/resolve-name-from-any x))))
(def fn-tgt-namer4 (fn [x] (str "tgt-namer4-" (clojure-potrubi.utils.names/resolve-name-from-any x))))


(def test-suite-manifest1
  [

   [:man1 'manifest-man1]
   [[:man2 'my-man2] 'my-man2]

   [[:pivot 'manifest-pivot-field] 'manifest-pivot-field]
   
   [[:man3 :my-man3] 'manifest-my-man3]

   [['manifest-man1 :my-man4] 'manifest-my-man4] ;; need manifest-man1 defined for this to work

   [['manifest-man1 'my-man5] 'my-man5] ;; need manifest-man1 defined for this to work
   

   [:man6 'tgt-namer1-man6 {:target-namer fn-tgt-namer1}]

   ;; explicitly specified names - note are literals
   [:man8 'man8 {:target-namer '(fn [v] v)  :source-namer '(fn [v] v)}]

   [[:man7 'src-namer1-man7] 'src-namer1-man7]
   [:man7 'tgt-namer1-man7 {:target-namer fn-tgt-namer1 :source-namer fn-src-namer1}] 

   [:man9 'tgt-namer2-man9 {:target-namer fn-tgt-namer2}]

   [:man10 'tgt-namer3-man10 {:target-namer fn-tgt-namer3}]
   [:man11 'tgt-namer4-man11 {:target-namer fn-tgt-namer4}]
   
   ])

(def test-suite-manifest1-a
  [

   [:man1 'manifest-man1]

   ;;[[:man2 'my-man2] 'my-man2]

   ;;[:man5 'target-namer-man5 {:target-namer fn-target-name}]

   ;;[['manifest-man1 :my-man4] 'manifest-my-man4] ;; need manifest-man1 defined for this to work

   ;;   [['manifest-man1 'my-man5] 'my-man5]

   ;;[[:man7 'src-namer1-man7] 'src-namer1-man7]
   ;;[:man7 'target-namer-man7 {:target-namer fn-target-name :source-namer fn-source-symbol}] wont work - uses run-time fns
   ;;[:man7 'tgt-namer1-man7 {:target-namer fn-tgt-namer1 :source-namer fn-src-namer1}] 
   
   ;;[:man7 'man7 {:target-namer '(fn [v] v)  :source-namer '(fn [v] v)}]

   ;;[:man9 'tgt-namer2-man9 {:target-namer fn-tgt-namer2}]


   
   ])


(def test-suite-active test-suite-manifest1)
;;(def test-suite-active test-suite-manifest1-a)

(deftest test-manifest1
  (doall (for [[man-def test-symbol man-ctrl] test-suite-active]
           (do
             (show-trace "test-manifest1" "ENTRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR")
             (trace-value-entr man-def "test-manifest1????????????????????" "MAN-DEF")
             (doall (println "test-manifest1" "ENTR" "MAN-DEF" (class man-def) man-def))
             (doall (println "test-manifest1" "ENTR" "TEST-SYMBOL" (class test-symbol) test-symbol))
             (doall (println "test-manifest1" "ENTR" "MAN-CTRL" (class man-ctrl) man-ctrl))
             (let [
                   man-args (if man-ctrl (list man-ctrl man-def) (list  man-def))
                   
                   return-var (eval `(define-manifests  ~@man-args))
                   ;;return-var (eval `(define-manitest  ~@man-args))

                   ;;_ (doall (println "test-manifest1" "BODY" "RETURN-VAR" (class return-var) return-var))
                   
                   wanted-value (cond
                                 (keyword? man-def) man-def
                                 (vector? man-def)
                                 (let [first-def (first man-def)]
                                   (cond
                                    (keyword? first-def) first-def
                                    (symbol? first-def) (var-get (resolve first-def)))))


                   ;;_ (doall (println "test-manifest1" "BODY" "WANTED-VALUE" (class wanted-value) wanted-value))

                   return-value (var-get return-var)
                   ;;_ (doall (println "test-manifest1" "BODY" "RETURN-VALUE" (class return-value) return-value))
                   
                   ;; ;; test-value nil
                   ;; ;; test-value (resolve test-symbol)
                   test-value (var-get (resolve test-symbol))
                   
                   ;;_ (doall (println "test-manifest1" "BODY" "TEST-VALUE" (class test-value) test-value))
                   
                   ]
               
               (doall (println "test-manifest1" "EXIT" "MAN-DEF" (class man-def) man-def))
               (doall (println "test-manifest1" "EXIT" "WANTED-VALUE" (class wanted-value) wanted-value))
               (doall (println "test-manifest1" "EXIT" "TEST-SYMBOL" (class test-symbol) test-symbol))
               (doall (println "test-manifest1" "EXIT" "TEST-VALUE" (class test-value) test-value))

               (doall (println "test-manifest1" "EXIT" "RETURN-VAR" (class return-var) return-var))
               (doall (println "test-manifest1" "EXIT" "RETURN-VALUE" (class return-value) return-value))

               (is (= return-value wanted-value)) ;; works
               (is (= test-value wanted-value)) ;; works
               
     
               )))))




