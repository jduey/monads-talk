
(ns LazyM
  (:require [Monads :as m]))


(deftype Thunk [fx]
  clojure.lang.IFn
  (invoke [_] (fx))

  (wrap [_ x] (thunk x))
  (flat-map [inner-thunk thunk-f]
    (Thunk. (fn []
              (let [inner-x (inner-thunk)
                    new-x-thunk (thunk-f inner-x)]
                (new-x-thunk))))))

(defn thunk [x] (Thunk. (fn [] x)))

(defn plus-1 [x]
  (println "executing 'plus-1' with input of" x)
  (thunk (inc x)))

(defn dbl [x]
  (println "executing 'dbl' with input of" x)
  (thunk (+ x x)))
