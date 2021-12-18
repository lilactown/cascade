(ns lilactown.hike)


(declare visit)


(defn map-entry
  [mk mv]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn- visit-coll
  [k inner outer c]
  (fn walk-coll-loop
    ([] (walk-coll-loop (transient (empty c)) c))
    ([c' items]
     (if-let [item (first items)]
       #(visit
         (fn [item']
           (walk-coll-loop
            (conj! c' (outer item'))
            (rest items)))
         inner
         outer
         (inner item))
       #(k (persistent! c'))))))


(defn- visit-map-entry
  [k inner outer e]
  (fn walk-map-entry-loop
    ([] (walk-map-entry-loop (key e)))
    ([mk]
     #(visit
       (fn [mk']
         (walk-map-entry-loop (outer mk') (val e)))
       inner
       outer
       (inner mk)))
    ([mk mv]
     #(visit
       (fn [mv']
         (fn [] (k (map-entry mk (outer mv')))))
       inner
       outer
       (inner mv)))))


(defn- visit-record
  [k inner outer r]
  (fn walk-record-loop
    ;; records can't be transients so we reproduce visit-coll w/o transients
    ([] (walk-record-loop r r))
    ([r' entries]
     (if-let [entry (first entries)]
       #(visit
         (fn [entry']
           (walk-record-loop
            (conj r' (outer entry'))
            (rest entries)))
         inner
         outer
         (inner entry))
       #(k r')))))


(defn- visit
  [k inner outer form]
  (cond
    (map-entry? form) (visit-map-entry k inner outer form)
    (record? form) (visit-record k inner outer form)
    (coll? form) (visit-coll k inner outer form)
    :else #(k form)))


(defn walk
  [inner outer form]
  (outer (trampoline visit identity inner outer (inner form))))


(defn postwalk
  [f form]
  (walk identity f form))


(defn prewalk
  [f form]
  (walk f identity form))


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

  ;; works fine
  (do (postwalk identity really-nested-data)
      nil))



