(ns lilactown.hike)


(declare walk)


(defn map-entry
  [[mk mv]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn- walk-coll
  ([next empty coll]
   ;; include empty again b/c type signature needs to be different
   (walk-coll next empty empty coll))
  ([next _empty coll' items]
   #(next next coll' items (first items))))


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
                    #(walk-coll
                      next empty
                      (conj coll' %) (rest items))
                    inner outer item)
                   #(done coll')))]
      (walk-coll
       next
       empty (inner form)))
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



