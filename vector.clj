
(ns VectorM
  (:require [Monads :as m]))

(extend-type clojure.lang.IPersistentVector
  m/Monad
  (wrap [_ x] (vector x))
  (flat-map [xs f]
    (vec (mapcat f xs))))

(defn plus-1 [x]
  (vector x (inc x)))

(defn dbl [x]
  (vector x (+ x x)))
