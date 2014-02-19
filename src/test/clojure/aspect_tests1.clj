(ns aspect-tests1
  (:use [clojure.test :only [deftest is function?]])
  (:require [clojure.core.contracts :as ccc]
            [clojure-contracts-sugar :as ccs]
            [clojure-carp :as carp]))

;;(carp/macro-set-trace true *ns* "ENTR")
;;(carp/trace-configure :first-telltale-format-specification "%-40s")

;; Add some mnemonics


(ccs/configure-contracts-store
 aspect-mnemonic-definitions
 {
  :suck-map-keyword-keys-numeric-vals 
  {:suck [[(map? arg0) (every? keyword? (keys abs-arg0)) (every? number? (vals arg0))]]}

  :rel-suck-map-keyword-keys-numeric-vals 
  {:suck [[(map? arg0) (every? keyword? (keys arg0)) (every? number? (vals arg0))]]}
  
  :suck-map-keyword-keys-numeric-vals-and-keyword
  {:suck [:suck-map-keyword-keys-numeric-vals :keyword]}

  :rel-suck-map-keyword-keys-numeric-vals-and-keyword
  {:suck [:rel-suck-map-keyword-keys-numeric-vals :keyword]}
  
  :contract-suck-map-keyword-keys-numeric-vals-and-keyword-spit-number 
  {:suck :suck-map-keyword-keys-numeric-vals-and-keyword :spit :number}

  :suck-map-string-test1 {:suck [:map :string] }
  :suck-map-string-spit-vector-test1 {:suck [:map :string] :spit :vector}

  :abs-map0 {:suck [[(map? abs-arg0)]] :spit [[(map? abs-arg0)]]}
  :abs-vector0 {:suck [[(vector? abs-arg0)]] :spit [[(vector? abs-arg0)]]}
  :abs-string0 {:suck [[(string? abs-arg0)]] :spit [[(string? abs-arg0)]]}
  :abs-number0 {:suck [[(number? abs-arg0)]] :spit [[(number? abs-arg0)]]}
  :abs-keyword0 {:suck [[(keyword? abs-arg0)]] :spit [[(keyword? abs-arg0)]]}

  :abs-map1 {:suck [[(map? abs-arg1)]] :spit [[(map? abs-arg1)]]}
  :abs-vector1 {:suck [[(vector? abs-arg1)]] :spit [[(vector? abs-arg1)]]}
  :abs-string1 {:suck [[(string? abs-arg1)]] :spit [[(string? abs-arg1)]]}
  :abs-number1 {:suck [[(number? abs-arg1)]] :spit [[(number? abs-arg1)]]}
  :abs-keyword1 {:suck [[(keyword? abs-arg1)]] :spit [[(keyword? abs-arg1)]]}
  
  :abs-map2 {:suck [[(map? abs-arg2)]] :spit [[(map? abs-arg2)]]}
  :abs-vector2 {:suck [[(vector? abs-arg2)]] :spit [[(vector? abs-arg2)]]}
  :abs-string2 {:suck [[(string? abs-arg2)]] :spit [[(string? abs-arg2)]]}
  :abs-number2 {:suck [[(number? abs-arg2)]] :spit [[(number? abs-arg2)]]}
  :abs-keyword2 {:suck [[(keyword? abs-arg2)]] :spit [[(keyword? abs-arg2)]]}
  
  :rel-map {:suck [[(map? arg0)]] :spit [[(map? arg0)]]}
  :rel-vector {:suck [[(vector? arg0)]] :spit [[(vector? arg0)]]}
  :rel-string {:suck [[(string? arg0)]] :spit [[(string? arg0)]]}
  :rel-number {:suck [[(number? arg0)]] :spit [[(number? arg0)]]}
  :rel-keyword {:suck [[(keyword? arg0)]] :spit [[(keyword? arg0)]]}

  :map-test1 '[[v] [(map? v) => (map? %)]]
  :vector-test1 '[[v] [(vector? v) => (vector? %)]]
  :suck-map-spit-vector-test1 '[[v] [(map? v) => (vector? %)]]
  :suck-map-spit-vector-test2 '[[v] [(map? v) => vector? ]]

  })


(def base-fn0 (fn [& x]
                (println "THIS IS FN0: x" (class x) x)
                x
                ))

(def base-fn1 (fn [x] (println "THIS IS FN1: x" (class x) x) x))

(def fn-test1 base-fn1)



(defn whatis?
  [& args]
  (doall (println "ENTR" "WHATIS?" "ARGS" (class args) (count args) args))
  ;;(apply (first args) (rest args))
  ;;true
  (first args)
  )

(def test-suite3-working
  [
   ;; simple symbols
   ['map? fn-test1 {:a 1}] ;; ok
   [{:suck 'map?} fn-test1 {:a 1}] ;; ok
   [{:suck 'map? :spit 'map?} fn-test1 {:a 1}] ;; ok
   
   ;; simple built-in mnemonics
   [:map fn-test1 {:a 1}]
   [:string fn-test1 "a string" :not-a-string]
   [:char fn-test1 \a]
   [:keyword fn-test1 :a-keyword]
   [:number fn-test1 3]
   [:list fn-test1 (list (list 1)) 1 (list 1)] ;; ok - double list ;; needed by test - an artefact
   [:coll fn-test1 [1 2 3]]
   [:seq fn-test1 (list (list 1)) 1 (list 1)] ;; ok - double list ;; needed by test - an artefact
   [:pos fn-test1 3 -3]
   [:neg fn-test1 -1 1]
   [:zero fn-test1 0 1]
   [:odd fn-test1 1 2]
   [:even fn-test1 2 1]
   
   ;; aspect-form map 
   [{:suck [:map]} (fn [x] [1 2 3]) {:a 1} 1 [1 2 3]] ;; okz
   [{:suck [:map] :spit :vector} (fn [x] [1 2 3]) {:a 1} 1 [1 2 3]] ;; ok

   ;; aspect-form with symbols
   [{:suck [[:map 'identity] 'number?]} (fn [x y] 3) (list {:a 1} 1) (list :a 1) 3] ;; ok
   [{:suck [[:map 'identity]] :spit 'number?} (fn [x] 3) {:a 1} 1 3] ;; ok

   ;; these based on example in clojure-contracts-sugar
   [{:suck [ [:map '(map? arg0)] ]} (fn [x ] [1 2 3] )  (list {:a 1} ) (list :a ) [1 2 3]] ;; ok but bonkers
   [{:suck [ [:map '(every? keyword? (keys arg0))] ]} (fn [x ] [1 2 3] )  (list {:a 1} ) (list :a ) [1 2 3]] ;; ok
   [{:suck [ [:map '(every? keyword? (keys arg0))] ] :spit :vector} (fn [x ] [1 2 3] )  (list {:a 1} ) (list :a ) [1 2 3]] ;; ok

   ;; others
   [{:suck [[:map '(every? keyword? (keys arg0))] `vector? `string? ] :spit  `map? }  (fn [x y z] {:b 2}) (list {:a  1} [1 2 3] "s1") (list :a 1 "two") {:b 2}] ;; ok 
   
   [{:suck '[[map? (every? keyword? (keys arg0))] (vector? arg1) string? ] :spit  'map? }  (fn [x y z] {:b 2}) (list {:a  1} [1 2 3] "s1") (list :a 1 "two") {:b 2}] ;; ok 
   

   ;; using explicit argument position maps
   [ {:suck {1 :keyword 0 :map } :spit :vector} (fn [m k] [1 2 3]) (list {:a 1} :a) (list :a 1) [1 2 3]] ;; ok
   [ {:suck [{1 :keyword} {0 :map}]  :spit :vector} (fn [m k] [1 2 3]) (list {:a 1} :a) (list :a 1) [1 2 3]] ;; ok
   [ {:suck [{1 :keyword} {0 :map} {2 :string}]  :spit :vector} (fn [m k s] [1 2 3]) (list {:a 1} :a "s1") (list :a 1 "s1") [1 2 3]] ;; ok
   [ {:suck [{0 :keyword} {1 :map} {2 :string}]  :spit :vector} (fn [ k m s] [1 2 3]) (list  :a {:a 1} "s1") (list :a 1 "s1") [1 2 3]] ;; ok
   [ {:suck [{0 :keyword} {1 '[:map (every? keyword? (keys arg0)) (every? number? (vals arg0))]} {2 :string}]  :spit :vector} (fn [k m s] [1 2 3]) (list :a {:a 1} "s1") (list :a 1 "s1") [1 2 3]] ;; ok

   ;; assertions for arguments can be sparse
   [ {:suck [ {1 '[:map (every? keyword? (keys arg0)) (every? number? (vals arg0))]} {2 :string}]  :spit :vector} (fn [a m s] [1 2 3]) (list :a {:a 1} "s1") (list :a 1 "s1") [1 2 3]] ;; ok

   
   ;; custom mnemonics
   [:suck-map-string-test1 (fn [x y] [1 2 3]) (list {:a 1} "s1") (list :a 1) [1 2 3]] ;; ok
   [:suck-map-string-spit-vector-test1 (fn [x y] [1 2 3]) (list {:a 1} "s1") (list :a 1) [1 2 3]] ;; ok

   ;; custom mnemonics with mnemonics
   [:suck-map-keyword-keys-numeric-vals (fn [m] m) (list {:a 1}) (list :a ) {:a 1}] ;; ok
   [:suck-map-keyword-keys-numeric-vals-and-keyword (fn [m k] (k m)) (list {:a 1} :a) (list :a 1) 1] ;; ok
   [:contract-suck-map-keyword-keys-numeric-vals-and-keyword-spit-number (fn [m k] (k m)) (list {:a 1} :a) (list :a 1) 1] ;; ok


   ;; mixing of mnemonics - these may not make "sense"

   [{:suck [:abs-map0] } (fn [x] [1 2 3]) {:a 1} 1 [1 2 3]] ;; ok
   [{:suck [:abs-map0] :spit :abs-vector2} (fn [x] [1 2 3]) {:a 1} 1 [1 2 3]] ;; ok
   [{:suck [:abs-map0 :abs-keyword1] :spit :abs-vector2} (fn [m k] [1 2 3]) (list  {:a 1} :a) (list :a 1) [1 2 3]] ;; ok
   [{:suck [:abs-map0 :rel-keyword] :spit :abs-vector2} (fn [m k] [1 2 3]) (list  {:a 1} :a) (list :a 1) [1 2 3]] ;; ok
   [{:suck [:rel-string :rel-suck-map-keyword-keys-numeric-vals-and-keyword]} (fn [s m k] (k m)) (list "s1" {:a 1} :a) (list "s2" :a 1) 1] ;; ok
   [{:suck [:rel-string :rel-suck-map-keyword-keys-numeric-vals-and-keyword] :spit :number} (fn [s m k] (k m)) (list "s1" {:a 1} :a) (list "s2" :a 1) 1] ;; ok
   [{:suck [:rel-string :abs-map1 :rel-keyword] :spit :number} (fn [s m k] (k m)) (list "s1" {:a 1} :a) (list "s2" :a 1) 1] ;; ok

   
   ;; multiple arities - both tested
   [[ {:suck [:map] :spit :vector} {:suck [:map :number] :spit :vector}] (fn [x] [1 2 3]) {:a 1} 1 [1 2 3]] ;; ok
   [[ {:suck [:map] :spit :vector} {:suck [:map :number] :spit :vector}] (fn [x y] [1 2 3]) (list  {:a 1} 1) (list :a  1) [1 2 3]] ;; ok


   ;; using clojure.core.contracts vector form
   [[['x] ['map? ]]  fn-test1 {:a  1}] ;; ok
   [[['x] ['map? '=> 'map?]]    fn-test1 {:a  1}] ;; ok
   ['[[x y x] [map? vector?  string? => map? ]]  (fn [x y z] {:b 2}) (list {:a  1} [1 2 3] "s1") (list :a 1 "two") {:b 2}] ;; ok
   ['[[x y z] [map? (vector? y)  string? (every? keyword? (keys x)) => map? ]]  (fn [x y z] {:b 2}) (list {:a  1} [1 2 3] "s1") (list :a 1 "two") {:b 2}] ;; ok
   ['[[x y z] [map? (vector? y)  string? (every? keyword? (keys x)) (= (first y) (get x :a)) => map? ]]  (fn [x y z] {:b 2}) (list {:a  1} [1 2 3] "s1") (list :a 1 "two") {:b 2}] ;; ok

   ;; using clojure.core.contracts vector form in a suck definition
   [{:suck [:map '[[k s] [(keyword? k) (string? s) => map? ]]] :spit 'number?} (fn [m k s] (k m) ) (list  {:a 3} :a "s1") (list :a :a 1) 3] ;; ok
   [{:suck ['[[k s] [(keyword? k) (string? s) => map? ]] {2 [:map]}] :spit 'number?} (fn [k s m] (k m) ) (list :a "s1" {:a 3}) (list :a :a 1) 3] ;; ok
   
   
   ;; mixed clojure.core.contracts vector (first arity) and aspect-form (second arity)
   [[[['x] ['map? '=> 'map?]] {:suck [:map :number] :spit :vector}]  (fn [x] x)  {:a 1}] ;; ok - first arity
   [[[['x] ['map? '=> 'map?]] {:suck [:map :number] :spit :vector}]  (fn [x y] [1 2 3] )  (list {:a 1} 1) (list :a 1) [1 2 3]] ;; ok - second arity


   
   
   ])


(def test-suite3-dev
  [

   ;;['map? fn-test1 {:a 1}] ;; ok

   ;;[ {:suck [{0 :keyword} {1 '[:map (every? keyword? (keys arg0)) (every? number? (vals arg0))]} {2 :string}]  :spit :vector} (fn [k m s] [1 2 3]) (list :a {:a 1} "s1") (list :a 1 "s1") [1 2 3]] ;; ok

   

   [:suck-map-string-test1 (fn [x y] [1 2 3]) (list {:a 1} "s1") (list :a 1) [1 2 3]]

   ;; WILL NEVER WORK CURRENTLY

   ;; [[{:suck :map} {:suck :map :spit :vector}]  (fn [x] [1 2 3]) {:a 1} 1 [1 2 3]]  ;; breaks - same arity
   ;; [{:spit [:map ] } fn-test1 {:a 1}] ;; breaks - no suck arity

   
   ])


(def active-suite3 test-suite3-working)
;;(def active-suite3 test-suite3-dev)

(defn to-list
  [any]
  (cond
   (nil? any) nil
   (list? any) any
   :else (list any)))

(deftest test-apply-aspects1
  (doall (for [[test-aspect fn-test# valid-value-nom invalid-value-nom return-value-nom]

               ;;test-suite3-working
               ;;test-suite3-dev
               active-suite3

               ]
           (let [

                 valid-value (to-list valid-value-nom) 
                 
                 ;;invalid-value (or invalid-value-nom "the invalid value string")
                 invalid-value
                 (to-list  (cond
                            (= test-aspect :not-nil) invalid-value-nom
                            :else (or invalid-value-nom "the invalid value string")))
                 
                 return-value
                 (cond
                  (= test-aspect :not-nil) valid-value
                  :else (or return-value-nom valid-value-nom))

                 _ (doall (println "test-apply-aspects1" "TEST-ASPECT" test-aspect "VALID-VALUE" (class valid-value) valid-value "INVALID-VALUE" invalid-value "RETURN-VALUE" return-value))
                 
                 fn-aspect (eval `(ccs/apply-contract-aspects ~fn-test# ~test-aspect))

                 ]

             (doall (println "test-apply-aspects1" "TEST-ASPECT" test-aspect "VALID-VALUE" (class valid-value) valid-value "INVALID-VALUE" invalid-value "RETURN-VALUE" return-value))
             
             (is (= return-value (apply fn-aspect valid-value)))       ;; target has aspect
             (is (thrown? AssertionError (apply fn-aspect invalid-value)))
             
             ))))








