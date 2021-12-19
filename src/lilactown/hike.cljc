(ns lilactown.hike)


(defn map-entry
  [[mk mv]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn walk-cont
  [k branch inner outer form]
  (let [k (if (map-entry? form)
            (comp k outer map-entry)
            (comp k outer))
        acc (cond
              (map-entry? form) []
              (record? form) form
              :else (empty form))]
    (if (coll? form)
      (branch
       k
       (fn [done item]
         (walk-cont done branch inner outer item))
       acc
       (inner form))
      #(k (inner form)))))


#_(trampoline
   walk
   identity
   (fn [done _branch _acc coll] (done coll))
   identity
   #(reduce + %)
   '(1 2 3))


(defn reduce-cont
  ([k step acc coll]
   (reduce-cont k step acc (seq coll) (first coll)))
  ([k step acc items item]
   (if (seq items)
     ;; bounce
     #(step
       (fn [acc']
         (let [items (rest items)]
           (reduce-cont k step acc' items (first items))))
       acc
       item)
     (if (or (list? acc) (seq? acc))
       #(k (reverse acc))
       #(k acc)))))


#_(trampoline
   reduce-cont
   identity
   (fn step [done acc n]
     (done (+ acc n)))
   0
   '(1 2 3))


(defn map-into-cont
  [k f acc coll]
  (reduce-cont
   k
   (fn step [done acc x]
     (f #(done (conj acc %)) x))
   acc
   coll))


#_(trampoline
   map-into-cont
   identity
   (fn step [k x]
     (k (inc x)))
   '()
   [1 2 3])


(defn prewalk
  [f form]
  (trampoline walk-cont identity map-into-cont f identity form))


(defn postwalk
  [f form]
  (trampoline walk-cont identity map-into-cont identity f form))


(comment
  (postwalk
   (fn [x] (if (number? x) (inc x) x))
   {1 2 3 {4 [5]}})

  (postwalk
   #(doto % prn)
   {1 2 3 {4 {5 {6 7}}}})

  (prewalk
   #(doto % prn)
   {1 2 3 {4 {5 {6 7}}}})

  (prewalk
   #(doto % prn)
   [1 [2 3 [4 5]] 6 [7]])

  (postwalk
   #(doto % prn)
   [1 [2 3 [4 5]] 6 [7]])

  (postwalk
   #(doto % prn)
   '(1 (2 3 (4 5)) 6 (7))))


(comment
  (require '[clojure.walk :as c.w])

  (c.w/postwalk-demo
   [1 [2 3 [4 5]] 6 [7]])

  (defn create [k i]
    (if (zero? i)
      (k)
      (fn []
        (create
         (fn []
           {:id i
            :child (k)})
         (dec i)))))

  (def limit 40000)

  (def really-nested-data
    {:foo (trampoline create (constantly {:id 0}) limit)})

  (do (c.w/postwalk identity really-nested-data)
      nil)

  (update really-nested-data :foo dissoc :child)

  ;; works fine
  (do (postwalk
       #(if (number? %) (inc %) %)
       really-nested-data)
      nil)

  (update
   (postwalk
    #(if (number? %) (inc %) %)
    really-nested-data)
   :foo
   dissoc :child)

  (def really-long-data
    {:foo (for [i (range 0 limit)]
            {:id i
             :child {:id (+ i limit)}})})

  (postwalk
   #(if (number? %) (inc %) %)
   really-long-data))
