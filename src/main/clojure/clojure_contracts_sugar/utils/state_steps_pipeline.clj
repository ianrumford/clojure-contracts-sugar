(ns clojure-contracts-sugar.utils.state-steps-pipeline
  (:require
   [clojure-contracts-sugar :as ccs :refer (apply-contract-aspects)]

   [clojure-contracts-sugar.utils.utils :as utils :refer (to-collection to-vector)]

   [clojure-contracts-sugar.utils.makros :as utils-makros :refer (form-fn-from-name-and-body form-fn-from-name-and-arities-map define-fn-apply-predicate-to-collection form-fn-try-catch-wrapper)]

   [clojure-contracts-sugar.utils.walk-forms :as utils-walkies
    :refer (walk-keyed-forms
            walk-forms
            is-walk-forms-symbol-maps?
            normalise-walk-forms-replace-maps
            normalise-walk-forms-keyed-replace-maps
            is-walk-forms-replace-map?
            is-walk-forms-replace-maps?
            is-walk-forms-keyed-replace-map?
            is-walk-forms-keyed-replace-maps?)]

   [clojure-carp :as carp :refer (surprise-exception)]))

;; State Step Pipeline for clojure-contracts-sugar

(declare state-pipeline-get-result)

;; *********
;; BEG: misc
;; *********

(defn- resolve-map-values
  "Some values of keys may be functions
   If so call function with args to find value
   Uses list of keys, else all keys"
  [arg-map arg-keys & args]
  {:pre [(map? arg-map) (or (nil? arg-keys) (coll? arg-keys))] :post [(map? %)]}

  (let [resolved-keys  (if arg-keys
                         (flatten arg-keys)
                         (keys arg-map))

        resolved-args (into {}
                            (for [arg-key resolved-keys :when (contains? arg-map arg-key)]
                              (let [arg-value (get arg-map arg-key)

                                    norm-arg-value (if (fn? arg-value)
                                                     (apply arg-value args)
                                                     arg-value)]
                                [arg-key norm-arg-value])))

        updated-arg-map (merge arg-map resolved-args)]

    updated-arg-map))

0
(defn resolve-values
  [fn-args & values]
  {:pre [(coll? fn-args) (coll? values)] :post [(coll? %)]}
  (let [resolved-values (map
                         (fn [value]
                           (cond
                            (fn? value) (apply value fn-args)
                            :else value))
                         values)]
    resolved-values))

(defn resolve-body-forms
  [& {:keys [forms resolve-args] :as opt-args}]
  { :post [(coll? %) (every? coll? %)]}
  (let [_ (assert (coll? forms))
        resolved-forms (map
                        (fn [form]
                          (let [resolved-form (cond
                                               (fn? form) (apply form resolve-args)
                                               :else form)

                                _ (assert (coll? resolved-form))]
                            resolved-form))
                        forms)]
    resolved-forms))

(defn resolve-let-locals
  [& {:keys [locals resolve-args] :as opt-args}]
  {:post [(vector? %)]}

  (let [_ (assert (coll? locals))
        resolved-locals
        (map
         (fn [local]
           (cond
            (fn? local) (apply local resolve-args)
            :else local))
         locals)

        resolved-locals (into [] resolved-locals)]
    resolved-locals))

;; *********
;; FIN: misc
;; *********

;; *************************
;; BEG: validation functions
;; *************************

(defn is-state-pipeline-step-result?
  [result]
  (and (map? result)
       (every? #{:id :result :step} (keys result))))

(define-fn-apply-predicate-to-collection is-state-pipeline-steps-results? is-state-pipeline-step-result?)

(defn is-state-pipeline-step?
  [step]
  (and (map? step)
       (every? #{:fn :args :id} (keys step))
       (fn? (get step :fn))))

(define-fn-apply-predicate-to-collection is-state-pipeline-steps? is-state-pipeline-step?)

(defn is-state-pipeline-state?
  [state]
  (and (map? state)
       (every? #{:step :results :result} (keys state))
       (if (contains? state :step) (is-state-pipeline-step? (get state :step)) true)
       (if (contains? state :results) (is-state-pipeline-steps-results? (get state :results)) true)))

;; *************************
;; BEG: validation functions
;; *************************

;; ********************
;; BEG: summary views
;; ********************


(defn step-summary-view
  "a diagnostic view of step"
  [step]
  {:pre [(is-state-pipeline-step? step)] :post [(map? %)]}
  {:step-id (get step :id)
   :step-args (get step :args)
   :result (get step :result)
   }
  )

(defn state-summary-view
  "a diagnostic view of state"
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(map? %)]}

  ;;(trace-value-call state "state-summary-view" "STATE")
  ;;(assert (is-state-pipeline-state? state))
  
  {:step-id (get-in state [:step :id])
   :step-args (get-in state [:step :args])

   :result (get state :result)
   ;;;:result (into [] (get state :result))

   }
  )


;; (defn trace-value-entr-state-summary
;;   [state & args]
;;   (apply trace-value-entr (state-summary-view state) args))

;; (defn trace-value-exit-state-summary
;;   [state & args]
;;   (apply trace-value-exit (state-summary-view state) args))

;; (defn trace-value-call-state-summary
;;   [state & args]
;;   (apply trace-value-call (state-summary-view state) args))

;; (defn trace-value-body-state-summary
;;   [state & args]
;;   (apply trace-value-body (state-summary-view state) args))

;; ******************
;; FIN: summary views
;; ******************

;; *******************
;; BEG: step functions
;; *******************

(defn state-pipeline-get-step
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-step? %)]}
  (get state :step))

(defn state-pipeline-update-step
  [state step]
  {:pre [(is-state-pipeline-state? state) (is-state-pipeline-step? step)] :post [(is-state-pipeline-state? %)]}
  (let [final-state (assoc state :step step)]
    final-state))

;; *******************
;; FIN: step functions
;; *******************

;; ************************
;; BEG: step args functions
;; ************************

(defn state-pipeline-get-step-args
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(map? %)]}
  (get (state-pipeline-get-step state) :args {}))

(defn state-pipeline-get-step-args
  [state & {:keys [valid-keys resolve-keys default-values] :as opt-args}]
  {:pre [(is-state-pipeline-state? state)] :post [(map? %)]}
  (let [;; any unexpected keys?
        _ (assert (every? #{:valid-keys :resolve-keys :default-values} (keys opt-args)))

        step-id (get (state-pipeline-get-step state) :id :anon)

        step-args (get (state-pipeline-get-step state) :args {})

        _ (if valid-keys
            (do
              (assert (set? valid-keys))
              (assert (every? valid-keys (keys step-args)))))

        ;; need to resolve any keys?
        resolved-args (if resolve-keys (resolve-map-values step-args resolve-keys state))

        step-args (merge step-args resolved-args)

        default-args
        (if default-values
          (let [_ (assert (map? default-values))
                _ (if valid-keys (assert (every? valid-keys (keys default-values))))
                no-keys (for [[def-key _] default-values :while (not (contains? step-args def-key))] def-key)

                ;; only resolve the keys with nil values
                no-keys-defaults (if (not-empty no-keys) (resolve-map-values default-values no-keys state))]
            no-keys-defaults))

        step-args (merge step-args default-args)]

    step-args))

(defn state-pipeline-get-step-args-key
  ([state key-name] (state-pipeline-get-step-args-key state key-name nil))
  ([state key-name value-default]
     (let [step-args (state-pipeline-get-step-args state)
           key-value (cond
                      (fn? value-default)  (get step-args key-name (value-default state))
                      :else (get step-args key-name value-default))]
       key-value)))

(defn state-pipeline-get-step-args-key-or-last-result
  [state key-name]
  (let [form-value (state-pipeline-get-step-args-key state key-name state-pipeline-get-result)]
    form-value))

(defn state-pipeline-get-step-args-form-or-last-result
  [state]
  (state-pipeline-get-step-args-key state :form state-pipeline-get-result))

;; ************************
;; FIN: step args functions
;; ************************

;; ***************************
;; BEG: step results functions
;; ***************************

(defn state-pipeline-get-step-results
  ([state] (state-pipeline-get-step-results state []))
  ([state default]
     {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-steps-results? %)]}
     (get state :results default)))

(defn state-pipeline-collect-step-results
  [state & results]
  {:pre [(is-state-pipeline-state? state) (is-state-pipeline-steps-results? results)] :post [(is-state-pipeline-steps-results? %)]}
  (into [] (concat (state-pipeline-get-step-results state) results)))

(defn state-pipeline-update-step-results
  [state & results]
  {:pre [(is-state-pipeline-state? state) (is-state-pipeline-steps-results? results)] :post [(is-state-pipeline-state? %)]}
  (assoc state :results (apply state-pipeline-collect-step-results state results)))

(defn state-pipeline-get-step-results-by-id
  [state result-id]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-steps-results? %)]}
  (let [step-results (state-pipeline-get-step-results state)
        id-results (filter
                    (fn [step-result]
                      (= result-id (get step-result :id)))
                    step-results
                    )]
    id-results))

;; ***************************
;; FIN: step results functions
;; ***************************

;; *********************
;; BEG: result functions
;; *********************

(defn state-pipeline-get-result
  [state]
  {:pre [(is-state-pipeline-state? state)]}
  (let [result (get state :result)]
    result))

(defn state-pipeline-get-result-by-id
  [state result-id]
  {:pre [(is-state-pipeline-state? state)]}

  (let [step-results-by-id (state-pipeline-get-step-results-by-id state result-id)

        ;; too many results?
        _ (assert (>= (count step-results-by-id) 1))

        step-result (first step-results-by-id)

        result (get step-result :result)]
    result))

(defn state-pipeline-add-result
  [state result]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step (state-pipeline-get-step state)

        step-id (get step :id (str (gensym "step-id-")))

        result-map {:id step-id
                    :result result}

        updated-state (state-pipeline-update-step-results state result-map)

        final-state (assoc updated-state :result result)]
    final-state))

;; *********************
;; FIN: result functions
;; *********************

;; *******************
;; BEG: state pipeline
;; *******************

(defn run-state-pipeline
  [initial-state & state-steps]
  {:pre [(is-state-pipeline-state? initial-state) (every? is-state-pipeline-step? state-steps)] :post [(is-state-pipeline-state? %)]}
  (let [final-state (reduce
                     (fn [state step]

                       (let [step-id (get step :id (str (gensym "step-id-")))

                             step-fn (get step :fn)

                             updated-step (assoc step :id step-id)

                             ;; allow nil step-fn to skip steps
                             new-state (if step-fn
                                         (let [_ (assert (fn? step-fn))
                                               step-state (state-pipeline-update-step state updated-step)

                                               next-state (step-fn step-state)]
                                           next-state)
                                         state)]

                         new-state))

                     initial-state
                     state-steps)]
    final-state))

;; *******************
;; FIN: state pipeline
;; *******************

;; *********************************
;; BEG: utility state step functions
;; *********************************

(defn state-step-define-fn-form
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:name :arities})

        fn-name (get step-args :name)

        nominal-arities (get step-args :arities)

        ;; resolve any value that are functions
        normal-arities (resolve-map-values nominal-arities nil state)
        define-form (form-fn-from-name-and-arities-map fn-name normal-arities)

        updated-state (state-pipeline-add-result state define-form)]

    updated-state))

(defn state-step-call-form
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :resolve-keys (list :form :form-args)
                   :valid-keys #{:form-args :form}
                   :default-values {:form state-pipeline-get-result})

        source-form (get step-args :form)

        source-form-args (get step-args :form-args (list))

        _ (assert (coll? source-form-args))

        call-form (list* source-form source-form-args)

        updated-state (state-pipeline-add-result state call-form)]

    updated-state))

(defn state-step-apply-wrappers
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :resolve-keys (list :form)
                   :valid-keys #{:wrappers :form}
                   :default-values {:form state-pipeline-get-result})

        ;; ;; be intolerant

        wrappers-nominal (get step-args :wrappers)
        source-form (get step-args :form)

        ;; apply wrappers right to left i.e same as comp
        wrapper-normal (reverse (remove nil? (to-vector wrappers-nominal)))

        wrapper-form (reduce
                      (fn [form wrapper] (list wrapper form))
                      source-form
                      wrapper-normal)

        updated-state (state-pipeline-add-result state wrapper-form)]

    updated-state))

(defn state-step-walk-keyed-forms
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:keyed-forms :keyed-replace-maps})

        keyed-forms (get step-args :keyed-forms)
        keyed-replace-maps (get step-args :keyed-replace-maps)

        keyed-walked-forms (walk-keyed-forms keyed-replace-maps keyed-forms)

        updated-state (state-pipeline-add-result state keyed-walked-forms)]

    updated-state))

(defn resolve-replace-maps
  [state replace-maps]
  {:post [(is-walk-forms-replace-maps? %)]}
  (let [normal-replace-maps (normalise-walk-forms-replace-maps replace-maps)

        resolved-replace-maps (map
                               (fn [replace-map] (resolve-map-values replace-map nil state))
                               normal-replace-maps)

        resolved-replace-maps (into [] resolved-replace-maps)]

    resolved-replace-maps))

(defn state-step-walk-forms
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:forms :replace-map}
                   :resolve-keys (list :forms)
                   ;;:default-values {:form state-pipeline-get-result}
                   )

        source-forms (get step-args :forms)
        replace-maps (resolve-replace-maps state (get step-args :replace-map))
        walked-forms (walk-forms replace-maps source-forms)

        updated-state (state-pipeline-add-result state walked-forms)]

    updated-state))

(defn state-step-walk-form
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:form :replace-map}
                   :resolve-keys (list :form)
                   :default-values {:form state-pipeline-get-result})

        source-form (get step-args :form)
        replace-maps (resolve-replace-maps state (get step-args :replace-map))
        walked-forms (walk-forms replace-maps (list source-form))

        _ (assert (>= (count walked-forms) 1))

        walked-form (first walked-forms)

        updated-state (state-pipeline-add-result state walked-form)]

    updated-state))

(defn state-step-apply-contract-aspects
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:form :aspects}
                   :resolve-keys (list :form :aspects)
                   :default-values {:form state-pipeline-get-result}
                   )

        contract-aspects (get step-args :aspects)

        source-form (get step-args :form)

        contract-form `(apply-contract-aspects ~source-form ~contract-aspects)

        updated-state (state-pipeline-add-result state contract-form)]

    updated-state))

(defn state-step-wrap-try-catch
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :resolve-keys (list :target-form)
                   :valid-keys #{:target-form :target-name :wrapper-name :target-arities})

        wrapper-name (get step-args :wrapper-name)
        try-catch-forms (apply form-fn-try-catch-wrapper (apply concat (dissoc  step-args :wrapper-name)))

        _ (assert (>= (count try-catch-forms) 2))

        final-forms (into [] (if-not wrapper-name
                               try-catch-forms
                               (let [wrapper-form (form-fn-from-name-and-body wrapper-name (last try-catch-forms))]
                                 (concat (butlast try-catch-forms) (list wrapper-form) ))))

        updated-state (state-pipeline-add-result state final-forms)]

    updated-state))

(defn state-step-let-form
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args
                   state
                   :valid-keys #{:locals :body}

                   )

        resolved-locals (resolve-let-locals :locals (get step-args :locals) :resolve-args (list state))

        resolved-body (resolve-body-forms :forms (get step-args :body) :resolve-args (list state))

        let-form `(let ~resolved-locals ~@resolved-body)

        updated-state (state-pipeline-add-result state let-form)]

    updated-state))

;; *********************************
;; BEG: utility state step functions
;; *********************************

;; ***************************************
;; BEG: state step administrive functions
;; ***************************************

(defn state-step-ensure-result-is-vector
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [last-result (state-pipeline-get-result state)
        last-result-vector (to-vector last-result)
        updated-state (state-pipeline-add-result state last-result-vector)]
    updated-state))

(defn state-step-set-result
  "set the current result
   e.g. get result of previous step"
  [state]
  {:pre [(is-state-pipeline-state? state)] :post [(is-state-pipeline-state? %)]}
  (let [step-args (state-pipeline-get-step-args state :resolve-keys (list :result))
        new-result (get step-args :result)
        updated-state (state-pipeline-add-result state new-result)]
    updated-state))

;; ***************************************
;; FIN: state step administrive functions
;; ***************************************

