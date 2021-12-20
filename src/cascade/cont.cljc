(ns cascade.cont
  (:refer-clojure
   :exclude [comp identity reduce transduce map filter remove into]))


(defn identity
  "Calls `k` with `x`."
  [k x]
  (k x))


(defn apply-with-cont
  ([k f] (f k))
  ([k f x] (f k x))
  ([k f x y] (f k x y))
  ([k f x y z] (f k x y z))
  ([k f x y z & more] (clojure.core/apply f k x y z more)))


(defn cont-with
  [f]
  (fn [k & more]
    (k (apply f more))))


(defn reduce
  "Continuation-passing style version of `reduce`.
  Calls (step k acc x) for all elements in `coll`. The `step` function should
  call the passed in continuation `k` with the new accumulated value.
  Calls `cont` with final result."
  ([step acc coll]
   (trampoline reduce clojure.core/identity step acc coll))
  ([cont step acc coll]
   (reduce cont step acc (seq coll) (first coll)))
  ([cont step acc coll el]
   (if (seq coll)
     ;; bounce
     #(step
       (fn [acc']
         (let [items (rest coll)]
           (reduce cont step acc' items (first items))))
       acc
       el)
     (if (or (list? acc) (seq? acc))
       #(cont (reverse acc))
       #(cont acc)))))


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
  [f]
  (fn [rf]
    (fn
      ([k] (rf k))
      ([k xs] (rf k xs))
      ([k xs x]
       (f #(rf k xs %) x)
       #_(rf k xs (f x))))))


(defn filter
  [pred]
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


(defn transduce
  ([xform rf coll]
   (transduce xform rf (rf) coll))
  ([xform rf init coll]
   (trampoline transduce clojure.core/identity xform rf init coll))
  ([cont xform rf init coll]
   (reduce cont (xform rf) init coll)))


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
  Calls (f c x) for all `x` in `coll`. `f` must call the passed in continuation
  `c` with the transformed value. `conj`'s the transformed value on to `acc`.
  Calls `k` with final result"
  ([f acc coll]
   (trampoline map-into clojure.core/identity f acc coll))
  ([k f acc coll]
   (reduce
    k
    (fn step [c acc x]
      (f #(c (conj acc %)) x))
    acc
    coll)))


#_(map-into
   (as-cont inc)
   '()
   [1 2 3])
;; => (2 3 4)


