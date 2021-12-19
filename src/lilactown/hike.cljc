(ns lilactown.hike)


(declare walk)


(defn map-entry
  [mk mv]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn- walk-coll
  ([k inner outer c] (walk-coll k inner outer (empty c) c))
  ([k inner outer c' items]
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


(defn- walk-map-entry
  ([k inner outer e] (walk-map-entry k inner outer e (key e)))
  ([k inner outer e mk]
   #(walk
     (fn [mk']
       (walk-map-entry k inner outer e mk' (val e)))
     inner
     outer
     mk))
  ([k inner outer _ mk mv]
   #(walk
     (fn [mv']
       (fn [] (k (map-entry mk mv'))))
     inner
     outer
     mv)))


;; records can't be transients so we reproduce walk-coll w/o transients
(defn- walk-record
  ([k inner outer r] (walk-record k inner outer r r))
  ([k inner outer r' entries]
   (if-let [entry (first entries)]
     #(walk
       (fn [entry']
         (walk-record
          k inner outer
          (conj r' entry')
          (rest entries)))
       inner
       outer
       entry)
     #(k r'))))


;; lists also can't be transients and we need to reverse it
(defn- walk-list
  ([k inner outer l]
   (walk-list k inner outer (empty l) l))
  ([k inner outer l' items]
   (if-let [item (first items)]
     #(walk
       (fn [item']
         (walk-list
          k inner outer
          (conj l' item')
          (rest items)))
       inner
       outer
       item)
     #(k (reverse l')))))


(defn walk
  [k inner outer form]
  (cond
    (or (list? form) (seq? form))
    (walk-list (comp k outer) inner outer (inner form))

    (map-entry? form)
    (walk-map-entry (comp k outer) inner outer (inner form))

    (record? form)
    (walk-record (comp k outer) inner outer (inner form))

    (coll? form)
    (walk-coll (comp k outer) inner outer (inner form))

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



