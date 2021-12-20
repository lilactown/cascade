(ns cascade.core
  (:refer-clojure
   :exclude [comp complement identity reduce remove transduce map filter keep into]))


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
  Calls (step k acc x) for all elements in `coll`. The `step` function should
  call the passed in continuation `k` with the new accumulated value.
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
  "Composes continuation-passing functions right-to-left."
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
  [to xform from]
  (transduce xform (cont-with conj) to from))


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
