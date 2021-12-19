(ns lilactown.hike)


(defn map-entry
  [[mk mv]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn walk
  [done next inner outer form]
  (if (coll? form)
    (let [done (cond
                 (or (list? form) (seq? form)) (comp done outer)
                 (map-entry? form) (comp done outer map-entry)
                 :else (comp done outer))
          empty (cond
                  (map-entry? form) []
                  (record? form) form
                  :else (empty form))
          form' (inner form)]
      ;; bounce
      #(next done inner outer empty form' (first form')))
    #(done (outer (inner form)))))


#_(trampoline
 walk
 identity
 (fn [done _inner _outer _coll' coll _item] (done coll))
 identity
 #(reduce + %)
 '(1 2 3))


(defn walk-items
  [done inner outer coll' items item]
  (if (seq items)
    ;; bounce
    (walk
     #(let [items (rest items)]
        (walk-items done inner outer (conj coll' %) items (first items)))
     walk-items
     inner outer item)
    #(done
      (if (or (list? coll') (seq? coll'))
        (reverse coll')
        coll'))))


(defn prewalk
  [f form]
  (trampoline walk identity walk-items f identity form))


(defn postwalk
  [f form]
  (trampoline walk identity walk-items identity f form))


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
