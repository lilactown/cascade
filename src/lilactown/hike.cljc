(ns lilactown.hike)


(declare walk)


(defn map-entry
  [[ mk mv ]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn- walk-coll
  ([k inner outer empty c]
   ;; type signature needs to be different
   (walk-coll k inner outer empty empty c))
  ([k inner outer _empty c' items]
   (if-let [item (first items)]
     #(walk
       (fn [item']
         (walk-coll
          k inner outer
          (conj c' item')
          (rest items)))
       inner
       outer
       item)
     #(k c'))))


(defn walk
  [k inner outer form]
  (cond
    (or (list? form) (seq? form))
    (walk-coll (comp k outer reverse) inner outer (empty form) (inner form))

    (map-entry? form)
    (walk-coll (comp k outer map-entry) inner outer [] (inner form))

    (record? form)
    (walk-coll (comp k outer) inner outer form (inner form))

    (coll? form)
    (walk-coll (comp k outer) inner outer (empty form) (inner form))

    :else #(k (outer (inner form)))))


(defn prewalk
  [f form]
  (trampoline walk identity f identity form))


(defn postwalk
  [f form]
  (trampoline walk identity identity f form))


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
   '(1 (2 3 (4 5)) 6 (7)))
  )


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

  (def really-nested-data
    {:foo (trampoline create (constantly {:id 0}) 40000)})

  (do (c.w/postwalk identity really-nested-data)
      nil)

  (update really-nested-data :foo dissoc :child)

  ;; works fine
  (do (postwalk
       #(if (number? %) (inc %) %)
       really-nested-data)
      nil)
  )



