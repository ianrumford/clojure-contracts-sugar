(ns clojure-contracts-sugar.utils.wrapped-functions
  (:require [clojure.core.memoize :as memo]
            [clojure-carp :as carp :refer (surprise-exception trace-value-entr trace-value-exit trace-value-call trace-value-body)]
            [clojure.walk :as walkies]
            [taoensso.timbre.profiling :as profiling])
)

;; Utilities for clojure-contracts-sugar

;; **********************
;; BEG: support functions
;; **********************

(defn wrapped-functions-collect-forms
  [state & forms]
  (concat (get state :forms {}) forms))

(defn wrapped-functions-update-forms
  [state & forms]
  (assoc state :forms (apply wrapped-functions-collect-forms (list* state forms))))

;; *********************
;; BEG: worker functions
;; *********************

(defn define-profiled-function
  [state]
  {:pre [(map? state)] :post [(map? %)]}
  (let [fn-name (get state :fn-name)

        profile-state (get state :profile {})
        profile-telltale (if (contains? profile-state :telltale)
                           (get profile-state :telltale)
                           (keyword  (str fn-name)))

        ;; need to "copy" the source function
        copy-name (gensym "profile")

        copy-form (list 'def copy-name fn-name)

        apply-form (list 'apply copy-name 'args)
        profiling-form (list 'taoensso.timbre.profiling/p profile-telltale  apply-form)
        profile-form (list 'defn fn-name '[& args] profiling-form)

        updated-state (wrapped-functions-update-forms state copy-form profile-form)]

    updated-state))

(defn define-memoized-function
  [state]
  {:pre [(map? state)] :post [(map? %)]}
  (let [fn-name (get state :fn-name)

        memoize-state (get state :memoize)
        _ (assert (map? memoize-state))

        memoize-fn (get memoize-state :memoizer)

        memo-name (if (contains? memoize-state :memo-name)
                  (get memoize-state :memo-name)
                  fn-name)

        memoize-args (get memoize-state :args [])
        _ (assert (coll? memoize-args))
        memo-form (list 'def memo-name  (list* memoize-fn fn-name memoize-args))
        updated-forms (wrapped-functions-collect-forms  state  memo-form)

        updated-state (dissoc
                       (assoc state
                         :forms updated-forms
                         :fn-name memo-name)
                       :memoize)]

    updated-state))

;; *********************
;; FIN: worker functions
;; *********************

;; *************
;; BEG: defaults
;; *************

(def wrapped-functions-defaults
  {:memoize {:handler define-memoized-function
             :memoizer 'clojure.core.memoize/lu
             :args [:lu/threshold 40]}

   :profile {:handler define-profiled-function}})

(def wrapped-functions-steps-default [:profile :memoize])

(def wrapped-functions-default-template {:steps wrapped-functions-steps-default })

(def wrapped-functions-steps-known (into #{} (keys wrapped-functions-defaults)))

(def wrapped-functions-template-keys (into #{} (list* :steps wrapped-functions-steps-known)))

;; *************
;; FIN: defaults
;; *************

;; *************************
;; BEG: main driver function
;; *************************

(defn define-wrapped-functions
  [states]
  {:pre [(coll? states) (every? map? states)]}
  (let [;; state-steps '[define-memoized-function]

        updated-states
        (map
         (fn [state]
           (let [steps (get state :steps)
                 _ (assert (coll? steps))
                 _ (assert (every? wrapped-functions-steps-known steps))
                 steps-order steps

                 updated-state (reduce
                                (fn [state step]
                                  (let [step-default (get wrapped-functions-defaults step)
                                        step-state (get state step {})

                                        updated-step-state (merge step-default step-state)

                                        step-handler (get updated-step-state  :handler)
                                        _ (assert (fn? step-handler))

                                        working-state (assoc state step updated-step-state)

                                        updated-state (step-handler working-state)]
                                    updated-state))
                                state
                                steps-order)]
             updated-state))
         states)]
    updated-states))

;; *************************
;; FIN: main driver function
;; *************************

;; ***************************
;; BEG: wrapped function macros
;; ****************************

(defmacro wrap-functions-with-template
  [wrap-template & wrap-states]
  {:pre [(map? wrap-template) (every? wrapped-functions-template-keys (keys wrap-template))]}
  (let [normalised-wrap-states
        (into []
              (map
               (fn [state]
                 (let [nominal-state (cond
                                  (symbol? state) {:fn-name state}
                                  (map? state) state
                                  :else (surprise-exception state "state is wat?"))

                       normalised-state (merge wrapped-functions-default-template wrap-template nominal-state)]

                   normalised-state))

               wrap-states))

        wrap-forms nil

        final-states (define-wrapped-functions normalised-wrap-states)
        wrap-forms (apply concat (map (fn [final-state] (get final-state :forms)) final-states))]
    `(do
       ~@wrap-forms
       nil)))

(defmacro wrap-functions
  [& wrap-states]
  {:pre [(coll? wrap-states)]}
  (let [wrap-forms `(wrap-functions-with-template ~wrapped-functions-default-template ~@wrap-states)]
    `(do
       ~wrap-forms
       nil)))

;; ***************************
;; FIN: wrapped function macros
;; ****************************
