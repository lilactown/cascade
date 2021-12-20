(ns cascade.cont
  (:refer-clojure :exclude [identity reduce]))


(defn identity
  "Calls `k` with `x`."
  [k x]
  (k x))


(defn reduce
  "Continuation-passing style version of `reduce`.
  Calls (step c acc x) for all elements in `coll`. The `step` function must call
  the passed in continuation with the new accumulated value.
  Calls `k` with final result."
  ([step acc coll]
   (trampoline reduce clojure.core/identity step acc coll))
  ([k step acc coll]
   (reduce k step acc (seq coll) (first coll)))
  ([k step acc items item]
   (if (seq items)
     ;; bounce
     #(step
       (fn [acc']
         (let [items (rest items)]
           (reduce k step acc' items (first items))))
       acc
       item)
     (if (or (list? acc) (seq? acc))
       #(k (reverse acc))
       #(k acc)))))


#_(reduce-cont
   (fn step [k acc n]
     (k (+ acc n)))
   0
   '(1 2 3))
;; => 6


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


#_(map-into-cont
   (fn step [k x]
     (k (inc x)))
   '()
   [1 2 3])
;; => (2 3 4)


