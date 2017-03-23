
(ns VectorM)

;; We have a function
(defn plus-1 [x]
  (vector x (inc x)))

;; and another
(defn dbl [x]
  (vector x (+ x x)))

;; Try composing them
(defn boom [x]
  (dbl (plus-1 x)))

;; This doesn't work because the values returned from these functions
;; have a side effect. (Being wrapped in a vector)
;; But conceptually, we think of working with the integers and the
;; side effect should just be in the background

;; So we need some plumbing

;; we could use the 'for' macro
(for [x (plus-1 10)
      y (dbl x)]
  y)

;; but that only works if the side effect is wrapping in a list
;; (note the return value of that expression is a list, not a vector)

;; Let's write a function that will accept a vector of integers, apply one of our
;; functions to each int, then combine all the resulting vectors into a single vector

(defn fn1 [xs f]
  (->> (map f xs)
       (apply concat)
       flatten
       vec))

;; and you're probably ahead of me, but this could be more easily written as

(defn fn2 [xs f]
  (vec (mapcat f xs)))

;; and we have a monad!! No, really. A monad is a datatype (Vector in this case)
;; that has a function like fn1 (or fn2) defined for it. There's another function we'll
;; need to define to be strictly correct and to make some things work later. It's called
;; 'wrap' and all it does is wrap a given value in the side effect of a value that's already
;; wrapped.

;; So let's generalize this just a little, then go on to see examples of other side effects

(ns Monads)

(defprotocol Monad
  (wrap [_ x])
  (flat-map [v f]))

;; So now we have the 'Monad' protocol. Let's go back to VectorM and implement it

(in-ns 'VectorM)
(require '[Monads :as m])

;; extend the Vector type
(extend-type clojure.lang.IPersistentVector
  m/Monad
  (wrap [_ x] (vector x))
  (flat-map [xs f]
    (vec (mapcat f xs))))

(m/flat-map (plus-1 5) dbl)

;; With 'flat-map' defined, we can nest calls and rewrite that expression as
(m/flat-map (plus-1 5)
            (fn [x]
              (dbl x)))

;; Which is kind of useless, but the real power comes with further nestings

(m/flat-map (plus-1 5)
            (fn [x]
              (m/flat-map (dbl x)
                          (fn [y]
                            (plus-1 y)))))
(m/flat-map (plus-1 8)
            (fn [x]
              (m/flat-map (dbl x)
                          (fn [y]
                            (plus-1 y)))))

(for [x (plus-1 5)
      y (dbl x)
      z (plus-1 y)]
  z)

;; But all those calls to 'flat-map' and creating anonymous funcitons is just
;; boiler plate that can be removed with a macro

(in-ns 'Monads)

(defmacro for [bindings expr]
  (let [steps (rest (partition 2 bindings))
        val-sym (gensym "for_")]
    `(let [~val-sym ~(second bindings)]
       (Monads/flat-map ~val-sym
                        (fn [~(first bindings)]
                          ~(reduce (fn [expr [sym mv]]
                                     (cond
                                       (= :when sym) `(if ~mv
                                                        ~expr
                                                        (Monads/zero ~val-sym))
                                       (= :let sym) `(let ~mv
                                                       ~expr)
                                       :else `(Monads/flat-map ~mv (fn [~sym]
                                                                     ~expr))))
                                   `(Monads/wrap ~val-sym ~expr)
                                   (reverse steps)))))))

;; I'm not going to take the time to explain that macro, just show the effects.
;; But do notice that it uses the 'wrap' function, which is why we had to include
;; it in the protocol
;; So back to VectorM

(in-ns 'VectorM)

(m/for [x (plus-1 5)
        y (dbl x)
        z (plus-1 y)]
  z)

(for [x (plus-1 5)
      y (dbl x)
      z (plus-1 y)]
  z)

;; and voila, we get the same effect, but without all the boiler plate


;; Now List should be a piece of cake.

(ns ListM)
(require '[Monads :as m])

;; extend the List type
(extend-type clojure.lang.IPersistentList
  m/Monad
  (wrap [_ x] (list x))
  (flat-map [xs f]
    (mapcat f xs)))

;; define List versions of our functions
(defn plus-1 [x]
  (list x (inc x)))

(defn dbl [x]
  (list x (+ x x)))

(m/flat-map (plus-1 5) dbl)

(m/for [x (plus-1 5)
        y (dbl x)
        z (plus-1 y)]
  z)

;; And HashSet follows the same pattern
(ns SetM)
(require '[Monads :as m])

;; extend the HashSet type
(extend-type clojure.lang.PersistentHashSet
  m/Monad
  (wrap [_ x] (hash-set x))
  (flat-map [xs f]
    (set (mapcat f xs))))

(defn plus-1 [x]
  (hash-set x (inc x)))

(defn dbl [x]
  (hash-set x (+ x x)))

(m/flat-map (plus-1 1) dbl)

(m/for [x (plus-1 1)
        y (dbl x)
        z (plus-1 y)]
  z)

;; To reiterate; A monad is a datatype that implements 2 functions 'wrap' and 'flat-map'
;; Hopefully, you're comfortable with the idea that the data type of a monad is
;; a container of sorts that represents a side effect.

;; Here's a container that just contains a value. And we're going to add a way to
;; convert it to a string so our output will look nice.

(ns MaybeM)
(require '[Monads :as m])

(deftype Maybe [x]
  java.lang.Object
  (toString [_] (str "<Maybe " x ">"))

  m/Monad
  (wrap [_ new-x] (Maybe. new-x))
  (flat-map [_ f]
    (when x
      (f x))))

(deftype Maybe [x]
  java.lang.Object
  (toString [_] (str "<Maybe " x ">"))

  m/Monad
  (wrap [_ new-x] (Maybe. new-x))
  (flat-map [_ f]
    (when (.x _)
            (f (.x _)))))

(defmethod print-method Maybe [v ^java.io.Writer w]
  (.write w (str v)))

;; create a 'Maybe' value

(Maybe. 99)

;; Since out 'flat-map' implementation can return 'nil', we need
;; to extend 'nil' as well

(extend-type nil
  m/Monad
  (wrap [_ x] (Maybe. x))
  (flat-map [_ _] nil))

;; Now, can you tell what this does?

;; Maybe some testing functions will help

(defn plus-1 [x]
  (when (odd? x)
    (Maybe. (inc x))))

(plus-1 3)
(plus-1 4)

(defn dbl [x]
  (when (even? x)
    (Maybe. (+ x x))))

(dbl 7)
(dbl 8)

(m/flat-map (plus-1 3) dbl)
(m/flat-map (plus-1 4) dbl)

(m/for [x (plus-1 1)
        y (dbl x)
        z (plus-1 y)]
  z)

(m/for [x (plus-1 2)
        y (dbl x)
        z (plus-1 y)]
  z)

(m/for [x (plus-1 2)
        y (dbl x)
        z (plus-1 (inc y))]
    z)

(m/for [x (plus-1 3)
        y (dbl x)
        z (plus-1 (inc y))]
      z)

;; Now, let's take it up a notch and look at another kind of container

(ns LazyM)
(require '[Monads :as m])


(defn f []
  22)

;; You can think of the function 'f' as "containing" the value 22. To get it out,
;; you execute the function.

(f)

;; 'f' is called a "thunk". It defers producing the value it contains until a later time.

;; Let's define a Thunk data type

(deftype Thunk [fx]
  clojure.lang.IFn
  (invoke [_] (fx)))

(defmethod print-method Thunk [v ^java.io.Writer w]
  (.write w "<Thunk ...>"))

;; I want to draw special attention to the fact that 'fx' is not just a plain value, it's
;; funciton of no arguments that returns a value when executed. The reason we have to do
;; this will become apparent when we implement 'flat-map' for 'Thunk'

;; And because of this, we're going to write a helper function to create thunks.

(defmacro thunk [& exprs] `(LazyM/Thunk. (fn [] (do ~@exprs))))

(def t (thunk 55))

(t)

;; Now, can you think of how to extend it to implement the Monad interface? 'wrap'
;; should be easy

(extend-type Thunk
  m/Monad
  (wrap [_ x] (thunk x)))

(def w (m/wrap t 99))

(w)

;; Now we turn our attention to 'flat-map'

;; First off, remember 'flat-map' takes a Thunk value that wraps some arbitrary value
;; and a function that takes an arbitrary value and returns a Thunk value. And 'flat-map'
;; should return a new Thunk value.

;; For reference, we'll sketch this out like this

(defn flat-map [inner-thunk thunk-f])
;; => outer-thunk

;;There are several ways to write this 'flat-map', but only one is really correct.
;; We want to be careful that we don't unwrap the 'inner-thunk' until the 'outer-thunk'
;; is executed. So the 'outer-thunk' is going to be a closure over the 'inner-thunk'
;; 'thunk-f' values.

;; It looks like this

(extend-type Thunk
  m/Monad
  (wrap [_ x] (thunk x))
  (flat-map [inner-thunk thunk-f]
    (Thunk. (fn []
              (let [inner-x (inner-thunk)
                    new-x-thunk (thunk-f inner-x)]
                (new-x-thunk))))))

;; And let's define our testing functions. Since a thunk can only hold one value,
;; our functions will look a little different.

;; Just for grins, we'll print out a status message when the execute

(defn plus-1 [x]
  (thunk (println "executing 'plus-1' with input of" x)
         (inc x)))

(def p1 (plus-1 15))

(defn dbl [x]
  (thunk (println "executing 'dbl' with input of" x)
         (+ x x)))

(def d1 (dbl 15))

(def fm-1 (m/flat-map (plus-1 7) dbl))

(fm-1)

;; So, we've just encoded the side effect of lazy evaluation as a monad
;; And we can compose a sequence of lazy calls using 'for'

(def expr (m/for [x (plus-1 5)
                  y (dbl x)
                  z (plus-1 y)]
            z))

;; The Thunk datatype wrapped a function of no arguments. But what could
;; could we do if we wrapped a function that accepted an argument?

;; We could use that value to determine how we executed the thunk.

(ns StateM)
(require '[Monads :as m])

(defmacro state [& exprs] `(StateM/StateFn. (fn [state-value#]
                                              [(do ~@exprs) state-value#])))

(deftype StateFn [fx]
  clojure.lang.IFn
  (invoke [_ state-value] (fx state-value))

  m/Monad
  (wrap [_ x] (state x))
  (flat-map [inner-fn state-f]
    (StateFn. (fn [state-value]
                (let [[x new-state] (inner-fn state-value)]
                  ((state-f x) new-state))))))

(defmethod print-method StateFn [v ^java.io.Writer w]
    (.write w "<StateFn ...>"))

;; That's a good start to our standard monad template. As usual, the 'flat-map'
;; function is where all the magic resides. Notice that when the 'inner-fn'
;; value is invoked, it returns a pair, which get's destructured. You can think
;; of the first value 'x' as the return value from 'inner-fn' and the second as
;; the new state. Let's create a function that will take an arbitrary value
;; and wrap it in a StateFn value.


;; The newly created StateFn value just waits to be invoked with a state value.

;; Now, let's again create our test functions

(defn plus-1 [x]
  (state (println "executing 'plus-1' with input of" x)
         (inc x)))

(def p1 (plus-1 15))

(p1 :some-state-value)

(defn dbl [x]
  (state (println "executing 'dbl' with input of" x)
         (+ x x)))

(def d1 (dbl 15))

(d1 :some-state-value)

(def expr (m/for [x (plus-1 5)
                  y (dbl x)
                  z (plus-1 y)]
            z))

(expr :some-state)
(expr :another-state)

;; But what we don't have yet, is the ability to actually look at, or change,
;; the state value that's being passed around. For that, we need some special
;; functions. One to read the state value.

(defn get-state []
  (StateFn. (fn [state-value]
              [state-value state-value])))

((get-state) :one-state)

;; and one to change it by applying a function to it

(defn update-state [f]
  (StateFn. (fn [state-value]
              [state-value (f state-value)])))

((update-state inc) 71)

;; But let's go one tiny step further. Suppose we say that the state value
;; will always be a hash-map? Then we could have a function that would
;; get a value out of that hash-map given a key.

(defn get-val
  ([key]
   (StateFn. (fn [state-value]
               [(get state-value key) state-value])))
  ([key default-value]
   (StateFn. (fn [state-value]
               [(get state-value key default-value) state-value]))))

((get-val :k) {:k 71})
((get-val :k) {})
((get-val :k 88) {:k 99})
((get-val :k 88) {})

;; So far, so good. No we need to assoc a key/value pair in the state value

(defn assoc-val [key value]
  (StateFn. (fn [state-value]
              [state-value (assoc state-value key value)])))

((assoc-val :msg "boy howdy, that's fun") {})

;; To round out our little language, let's update a key in the state value

(defn update-val [key f & args]
  (StateFn. (fn [state-value]
              [state-value (apply update-in state-value [key] f args)])))

;; And now we can write programs that appear to have mutable state

(def pg1 (m/for [x (get-val :x)
                 result (if (odd? x)
                          (plus-1 x)
                          (dbl x))
                 _ (assoc-val :msg "done with :x")]
           result))

(pg1 {:x 5})
(pg1 {:x 6})

;; Let's redfine our test functions to add logging

(defn plus-1 [x]
  (m/for [_ (update-val :log (fnil conj [])
                        (print-str "executing 'plus-1' with input of" x))]
    (inc x)))

(defn dbl [x]
  (m/for [_ (update-val :log (fnil conj [])
                        (print-str "executing 'dbl' with input of" x))]
    (+ x x)))

(def pg2 (m/for [x (get-val :x)
                 result (if (odd? x)
                          (plus-1 x)
                          (dbl x))
                 _ (update-val :log (fnil conj [])
                               "done with :x")]
           result))

(pg2 {:x 5})
(pg2 {:x 6})


;; the 3 monad laws

(in-ns 'VectorM)

(= (m/flat-map (vector 1 2 3) vector)
   (vector 1 2 3))

(= (m/flat-map (vector 4) dbl)
   (dbl 4))

(= (m/flat-map (m/flat-map (vector 4) plus-1) dbl)
   (m/flat-map (vector 4) (fn [x] (m/flat-map (plus-1 x) dbl))))
