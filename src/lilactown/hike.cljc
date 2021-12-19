(ns lilactown.hike)


(declare walk)


(defn map-entry
  [[mk mv]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn walk
  [done inner outer form]
  (if (coll? form)
    (let [done (cond
                 (or (list? form) (seq? form)) (comp done outer reverse)
                 (map-entry? form) (comp done outer map-entry)
                 :else (comp done outer))
          empty (cond
                  (map-entry? form) []
                  (record? form) form
                  :else (empty form))
          next (fn [next coll' items item]
                 (if (seq items)
                   (walk
                    #(let [items (rest items)]
                       (next next (conj coll' %) items (first items)))
                    inner outer item)
                   #(done coll')))
          form' (inner form)]
      #(next next empty form' (first form')))
    #(done (outer (inner form)))))


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

  (def really-long-data
    {:foo (for [i (range 0 limit)]
            {:id i
             :child {:id (+ i limit)}})})

  (postwalk
   #(if (number? %) (inc %) %)
   really-long-data)
  )
