(ns cascade.core
  "# cascade

  Cascade is a library of continuation-passing, tail recursive versions of many
  Clojure core functions.

  The goal is to allow essentially unbounded recursion and mutual recursion of
  seq operations. This means that the seq operations in this library must not
  use the call stack. Instead, they use a combination of continuation-passing to
  ensure that operations can always be in the tail position and trampolining to
  ensure that operations do not use the call stack.

  This provides the ability to write recursive algorithms that work on very nested
  data structures in Clojure(Script) using familiar operations.

  It aims to cover
  - seq operations: reduce, transduce, into, and common transducer-producing fns
  - CPS fn composition: identity, complement, comp

  All seq operations can be passed a continuation as first argument, which will
  be called on completion of the operation, and returns a thunk (0-arity
  function) which can be called to begin the operation. A thunk will return
  either another thunk, which can be called to continue the operation, or the
  result. Thus, they are meant to be used with `clojure.core/trampoline`.

  If a continuation is not passed in to most seq operations, it is assumed you
  want to run the operation eagerly and will trampoline for you, returning the
  result.

  ## Transducers

  Transducer-producing functions like `map`, `filter`, etc. take functions which
  accept a continuation and the element of the sequence they are operating on, and
  should call the continuation with the result instead of returning it.

  When passed a single function, they return a transducer for use with
  `cascade.core/transduce` and `cascade.core/into`.

  When passed a function and a collection, they will eagerly execute the operation
  using `trampoline` and return the result.

  When passed a continuation, a function and a collection, they will return a
  thunk, meaning it can be trampolined.

  ## Examples

  For an example of how this can be used practically, see `cascade.hike`."
  (:refer-clojure
   :exclude [comp complement identity reduce remove some transduce map filter keep into]))


(defn identity
  "Calls `k` with `x`."
  [k x]
  (k x))


(defn cont-with
  "Takes a non-continuation passing function `f` and any arguments to apply to
  the front. Returns a function that accepts a continuation as the first
  argument, and when called applies `f` with the initial args and then any
  additional args passed to the new function, passing the result to the
  continuation."
  ([f]
   (fn [k & more]
     (k (apply f more))))
  ([f x]
   (fn [k & more]
     (k (apply f x more))))
  ([f x y]
   (fn [k & more]
     (k (apply f x y more))))
  ([f x y z]
   (fn [k & more]
     (k (apply f x y z more))))
  ([f x y z & args]
   (fn [k & more]
     (k (apply f x y z (concat args more))))))


(defn complement
  "Takes a continuation-passing function `f` and returns a new continuation-
  passing function which accepts the same args, does the same effects (if any)
  and calls the passed in continuation with the opposite truth value."
  [f]
  (fn [k & args]
    (apply f #(k (not %)) args)))


(defn reduce
  "Continuation-passing style version of `reduce`.
  Calls (step k acc x) for all elements in seqable `coll`. The `step` function
  should call the passed in continuation `k` with the new accumulated value.
  If passed in, calls the continuation `k` with final result. Else trampolines
  and returns the result."
  ([step acc coll]
   (trampoline reduce clojure.core/identity step acc coll))
  ([k step acc coll]
   (reduce k step acc (seq coll) (first coll)))
  ([k step acc coll el]
   (if (seq coll)
     ;; bounce
     #(step
       (fn [acc']
         (let [items (rest coll)]
           (reduce k step acc' items (first items))))
       acc
       el)
     (if (or (list? acc) (seq? acc))
       #(k (reverse acc))
       #(k acc)))))


(defn comp
  "Composes continuation-passing functions. The returned function accepts a
  continuation as its first argument and a variable number of additional args,
  calls the last function with a continuation of the next one and the arguments,
  and so on right-to-left."
  ([f] (fn
         ([k x] (f k x))
         ([k x y] (f k x y))
         ([k x y z] (f k x y z))
         ([k x y z & more] (apply f k x y z more))))
  ([f g] (fn
           ([k x] (g #(f k %) x))
           ([k x y] (g #(f k %) x y))
           ([k x y z] (g #(f k %) x y z))
           ([k x y z & more] (apply g #(f k %) x y z more))))
  ([f g & more]
   (clojure.core/reduce comp (list* f g more))))


(defn map
  "Applies `f`, a continuation-passing function, to each element of `coll` and
  collects the results into a list.

  If a continuation `k` is passed in, calls it with the final result list.
  If `f` and a `coll` are passed in, trampolines and returns the result list.
  If only a function `f` is passed in, returns a continuation-passing transducer
  function for use with `cascade.core/transduce`."
  ([f]
   (fn [rf]
     (fn
       ([k] (rf k))
       ([k xs] (rf k xs))
       ([k xs x]
        (f #(rf k xs %) x)))))
  ([f coll] (trampoline map clojure.core/identity f coll))
  ([k f coll]
   (reduce
    k
    (fn [k acc x]
      (f #(k (cons % acc)) x))
    '() coll)))


(defn filter
  "Applies `pred`, a continuation-passing function, to each element of `coll`
  and collects elements which pass a truth-y value into a list.

  If a continuation `k` is passed in, calls it with the final result list.
  If `f` and a `coll` are passed in, trampolines and returns the result list.
  If only a function `f` is passed in, returns a continuation-passing transducer
  function for use with `cascade.core/transduce`."
  ([pred]
   (fn [rf]
     (fn
       ([k] (rf k))
       ([k xs] (rf k xs))
       ([k xs x]
        (pred
         #(if %
            (rf k xs x)
            (k xs))
         x)))))
  ([pred coll] (trampoline filter clojure.core/identity pred coll))
  ([k pred coll]
   (reduce
    k
    (fn [k acc x] (pred #(if % (k (cons x acc)) (k acc)) x))
    '() coll)))


(defn remove
  "Applies `pred`, a continuation-passing function, to each element of `coll`
  and collects elements which pass a false-y value into a list.

  If a continuation `k` is passed in, calls it with the final result list.
  If `f` and a `coll` are passed in, trampolines and returns the result list.
  If only a function `f` is passed in, returns a continuation-passing transducer
  function for use with `cascade.core/transduce`."
  ([pred] (filter (complement pred)))
  ([pred coll] (filter (complement pred) coll))
  ([k pred coll] (filter k (complement pred) coll)))


(defn keep
  "Applies `pred`, a continuation-passing function, to each element of `coll`
  and collects all truth-y results into a list.

  If a continuation `k` is passed in, calls it with the final result list.
  If `f` and a `coll` are passed in, trampolines and returns the result list.
  If only a function `f` is passed in, returns a continuation-passing transducer
  function for use with `cascade.core/transduce`."
  ([pred]
   (fn [rf]
     (fn
       ([k] (rf k))
       ([k xs] (rf k xs))
       ([k xs x]
        (pred #(if (some? %)
                 (rf k xs %)
                 (k xs))
              x)))))
  ([pred coll] (trampoline keep clojure.core/identity pred coll))
  ([k pred coll]
   (reduce
    k
    (fn [k acc x]
      (pred #(if (some? %)
               (k (cons % acc))
               (k acc))
            x))
    '() coll)))


(defn transduce
  "Continuation-passing style version of `clojure.core/transduce`.
  Takes continuation-passing reducing function `rf` and a CPS xform
  (see `cascade.core/map`, `cascade.core/filter`, et al.) and applies
  (xform rf). Then, reduces the collection using that new reducing fn."
  ([xform rf coll]
   (transduce xform rf (rf) coll))
  ([xform rf init coll]
   (trampoline transduce clojure.core/identity xform rf init coll))
  ([k xform rf init coll]
   (reduce k (xform rf) init coll)))


(defn into
  "Returns a new collection consisting of `to` with all of the items
  resulting from trasnducing `from` with `xform`.
  `xform` should be a continuation-passing transducer. See `cascade.core/map`,
  `cascade.core/filter`, et al."
  ([to xform from]
   (transduce xform (cont-with conj) to from))
  ([k to xform from]
   (transduce k xform (cont-with conj) to from)))


(defn map-into
  "Continuation-passing style of (into acc (map f) coll).
  Calls (f k x) for all `x` in `coll`. `f` must call the passed in continuation
  `c` with the transformed value. `conj`'s the transformed value on to `acc`.
  Calls passed in continuation `k` with final result, or trampolines when not."
  ([f acc coll]
   (trampoline map-into clojure.core/identity f acc coll))
  ([k f acc coll]
   (reduce
    k
    (fn step [k acc x]
      (f #(k (conj acc %)) x))
    acc
    coll)))


(defn some
  ([predk coll]
   (trampoline some clojure.core/identity predk coll))
  ([k predk coll]
   #(if (seq coll)
      (predk
       (fn [?] (if ?
                 (fn [] (k ?))
                 (fn [] (some k predk (rest coll)))))
       (first coll))
      (k nil))))


#_(some
   (fn predk [k x]
     (if (coll? x)
       (some k predk x)
       (k (#{:c} x))))
   [:a :b [[:c]] :d])

#_(some
   (cont-with #{:e})
   #{:a :b :c :d})


(defprotocol IEqualWithContinuation
  "A protocol for comparing two values using continuation-passing. Used by `eq`
  to allow outside extension to custom data types."
  (-eq [x k y] "Accepts a continuation `k` and another value `y`, and either
 calls the continuation with a boolean representing whether they are equal, or
 returns a thunk (0-arity function) that when called will continue the
 operation.

 See `cascade.core/-eq-sequential` and `cascade.core/-eq-unordered` for examples
 how to write recursive CPS algorithms comparing two complex data structures."))


(declare eq)


(extend-protocol IEqualWithContinuation
  nil
  (-eq [_ k y]
    #(k (nil? y)))

  #?(:clj Object :cljs object)
  (-eq [x k y]
    #(k (= x y))))


#?(:cljs
   (extend-type default
     IEqualWithContinuation
     (-eq [x k y]
       #(k (= x y)))))


(defn -eq-sequential
  [k xs ys]
  (if-let [x (first xs)]
    (if-let [y (first ys)]
      (eq
       #(if %
          (-eq-sequential k (rest xs) (rest ys))
          (k false))
       x y)
      ;; we have x but no y
      (k false))
    ;; we have no x, ensure we have no y
    (k (empty? ys))))


(defn -eq-unordered
  [k xs ys]
  (if-let [x (first xs)]
    ;; we can't use contains? here because it could call equiv, which on a
    ;; nested structure would descend. so instead we go slow and scan
    (some
     (fn [z] (if (some? z)
               (-eq-unordered k (rest xs) ys)
               (k false)))
     (fn predk [k y]
       (eq k x y))
     ys)
    (k true)))


(defn eq
  "Just like clojure.core/=, but works for very nested data structures. Supports
  deep equality of all Clojure data types. For custom data types that you want
  to descend into, implement IEqualWithContinuation."
  ([x y] (trampoline eq clojure.core/identity x y))
  ([k x y]
   (cond
     (nil? x) #(k (nil? y))
     (identical? x y) #(k true)
     (map? x) #(if (map? y)
                 (if (= (count x) (count y))
                   (-eq-unordered k x y)
                   (k false))
                 (k false))
     (set? x) #(if (set? y)
                 (if (= (count x) (count y))
                   (-eq-unordered k x y)
                   (k false))
                 (k false))
     (sequential? x) #(if (sequential? y)
                        (if (and (counted? x) (counted? y)
                                 (== (count x) (count y)))
                          (-eq-sequential k x y)
                          (k false))
                        (k false))
     :else (-eq x k y))))


(comment
  (eq "a" "a")
  (eq 1 2)

  (eq [1 [2] 3] [1 [2] 3])

  (eq #{1 2 3} #{1 2 3})
  (eq #{1 2 3} #{1 2 4})
  (eq #{1 2 3} #{1 2})

  (eq #{[1] [2 [3]]}
      #{[1] [2 [3]]})

  (eq [#{1} [2 #{3 4}]]
      [#{1} [2 #{3 4}]])

  (eq [#{1} [2 #{3 4}]]
      [#{1} [2 #{3 4 6}]])

  (eq {:id 1
       :child {:id 2}}
      {:id 1
       :child {:id 2}})

  (eq {:id 1
       :child {:id 2}}
      {:id 1
       :child {:id 2 :foo 3}})

  (eq {:id 1
       :child {:id 2}}
      {:id 1
       :child {:foo 2}})

  (eq {:id 1
       :child {:id 2}}
      {:id 1
       :child {:id [2]}}))


