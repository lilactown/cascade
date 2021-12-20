(ns cascade.cont
  (:refer-clojure
   :exclude [comp complement identity reduce remove transduce map filter into]))


(defn identity
  "Calls `k` with `x`."
  [k x]
  (k x))


(defn cont-with
  [f]
  (fn [k & more]
    (k (apply f more))))


(defn complement
  [f]
  (fn [k x]
    (f #(k (not %)) x)))


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


#_(reduce
   (fn step [k acc n]
     (k (+ acc n)))
   0
   '(1 2 3))
;; => 6


(defn comp
  ([f] (fn [k x] (f k x)))
  ([f g] (fn [k x]
           (g #(f k %) x)))
  ([f g & more]
   (clojure.core/reduce comp (list* f g more))))


(defn map
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


#_(map (cont-with inc) [1 2 3])
;; => (2 3 4)


(defn filter
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


#_(filter (cont-with even?) [1 2 3 4])
;; => (2 4)


(defn remove
  ([pred] (filter (complement pred)))
  ([pred coll] (filter (complement pred) coll))
  ([k pred coll] (filter k (complement pred) coll)))


#_(remove (cont-with even?) [1 2 3 4])
;; => (1 3)


(defn transduce
  ([xform rf coll]
   (transduce xform rf (rf) coll))
  ([xform rf init coll]
   (trampoline transduce clojure.core/identity xform rf init coll))
  ([k xform rf init coll]
   (reduce k (xform rf) init coll)))


(comment
  (transduce
   (map (cont-with inc))
   (fn
     ([k acc] (k acc))
     ([k acc n] (k (+ acc n))))
   0
   '(1 2 3))

  (transduce
   (filter (cont-with even?))
   (fn
     ([k acc] (k acc))
     ([k acc n] (k (conj acc n))))
   []
   '(1 2 3)))


(defn into
  [to xform from]
  (transduce
   xform
   (fn
     ([k xs] (k xs))
     ([k xs x] (k (conj xs x))))
   to from))


#_(into #{} (map identity) '(1 2 3 4 3 2 1))
;; => #{1 4 3 2}


(defn map-into
  "Continuation-passing style of (into acc (map f) coll).
  Calls (f k x) for all `x` in `coll`. `f` must call the passed in continuation
  `c` with the transformed value. `conj`'s the transformed value on to `acc`.
  Calls `cont` with final result"
  ([f acc coll]
   (trampoline map-into clojure.core/identity f acc coll))
  ([k f acc coll]
   (reduce
    k
    (fn step [k acc x]
      (f #(k (conj acc %)) x))
    acc
    coll)))


#_(map-into
   (as-cont inc)
   '()
   [1 2 3])
;; => (2 3 4)


