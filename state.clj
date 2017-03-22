
(ns StateM
  (:require [Monads :as m]))

(defn state [fx])

(deftype StateFn [fx]
  clojure.lang.IFn
  (invoke [_ state-value] (fx state-value))

  m/Monad
  (wrap [_ x] (state x))
  (flat-map [inner-fn state-f]
    (StateFn. (fn [state-value]
                (let [[x new-state] (inner-fn state-value)]
                  ((state-f x) new-state))))))

(defn state [x] (StateFn. (fn [state-value]
                            [x state-value])))

(defn get-state []
  (StateFn. (fn [state-value]
              [state-value state-value])))

(defn update-state [f]
  (StateFn. (fn [state-value]
              [state-value (f state-value)])))

(defn get-val
  ([key]
   (StateFn. (fn [state-value]
               [(get state-value key) state-value])))
  ([key default-value]
   (StateFn. (fn [state-value]
               [(get state-value key default-value) state-value]))))

(defn assoc-val [key value]
  (StateFn. (fn [state-value]
              [state-value (assoc state-value key value)])))

(defn update-val [key f & args]
  (StateFn. (fn [state-value]
              [state-value (apply update state-value key f args)])))

(defn plus-1 [x]
  (m/for [_ (update-val :log (fnil conj [])
                        (print-str "executing 'plus-1' with input of" x))]
    (inc x)))

(defn dbl [x]
  (m/for [_ (update-val :log (fnil conj [])
                        (print-str "executing 'dbl' with input of" x))]
    (+ x x)))
