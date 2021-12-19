(ns lilactown.hike)


(defn identity-cont
  [k x]
  (k x))


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


(defn map-entry
  [[mk mv]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn walk-cont
  ([inner-cont outer-cont form]
   (walk-cont identity inner-cont outer-cont form))
  ([k inner-cont outer-cont form]
   (let [k (if (map-entry? form)
             #(outer-cont k (map-entry %))
             #(outer-cont k %))
         acc (cond
               (map-entry? form) []
               (record? form) form
               :else (empty form))]
     (if (coll? form)
       (map-into-cont
        k
        (fn step [k item]
          (inner-cont k item))
        acc
        form)
       #(k form)))))


#_(trampoline
   walk-cont
   identity
   identity
   #(reduce + %)
   '(1 2 3))


(defn prewalk
  [f form]
  (trampoline
   walk-cont
   (fn inner [k x]
     (walk-cont k inner identity-cont (f x)))
   identity-cont ; outer
   (f form)))


(defn postwalk
  [f form]
  (letfn [(outer [k x] (k (f x)))]
    (trampoline
     walk-cont
     (fn inner [k x]
       (walk-cont k inner outer x))
     outer
     form)))


(defn keywordize-keys
  [m]
  (letfn [(keywordize-entry
            [[k v]]
            (if (string? k)
              [(keyword k) v]
              [k v]))]
    (postwalk
     (fn [x]
       (if (map? x)
         (into {} (map keywordize-entry) x)
         x))
     m)))


#_(keywordize-keys {"a" 1 :b 2 :c {"d" 3}})
;; => {:a 1, :b 2, :c {:d 3}}


(defn stringify-keys
  [m]
  (letfn [(stringify-entry
            [[k v]]
            (if (keyword? k)
              [(name k) v]
              [k v]))]
    (postwalk
     (fn [x]
       (if (map? x)
         (into {} (map stringify-entry) x)
         x))
     m)))


#_(stringify-keys {"a" 1 :b 2 :c {"d" 3}})
;; => {"a" 1, "b" 2, "c" {"d" 3}}


(defn prewalk-replace
  "Recursively transforms form by replacing keys in smap with their
  values. Like clojure/replace but works on any data structure.  Does
  replacement at the root of the tree first."
  [smap form]
  (prewalk (fn [x] (if (contains? smap x) (smap x) x)) form))


#_(prewalk-replace
   {:a :x :b :y :c :z}
   '(:a :b (:c {:d 1})))
;; => (:x :y (:z {:d 1}))


(defn postwalk-replace
  "Recursively transforms form by replacing keys in smap with their
  values. Like clojure/replace but works on any data structure.  Does
  replacement at the leaves of the tree first."
  [smap form]
  (postwalk (fn [x] (if (contains? smap x) (smap x) x)) form))


#_(postwalk-replace
   {:a :x :b :y :c :z}
   '(:a :b (:c {:d 1})))
;; => (:x :y (:z {:d 1}))


(defn macroexpand-all
  "Recursively performs all possible macroexpansions in form."
  [form]
  (prewalk (fn [x] (if (seq? x) (macroexpand x) x)) form))


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

  (c.w/prewalk-replace
   {:a :x :b :y :c :z}
   '(:a :b (:c {:d 1})))

  (defn create [k i]
    (if (zero? i)
      (k)
      (fn []
        (create
         (fn []
           {:id i
            :child (k)})
         (dec i)))))

  (def limit 10000)

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

  (update-in
   (postwalk
    #(if (number? %) (inc %) %)
    really-nested-data)
   [:foo :child :child :child]
   dissoc :child)

  (def really-long-data
    {:foo (for [i (range 0 limit)]
            {:id i
             :child {:id (+ i limit)}})})

  (postwalk
   #(if (number? %) (inc %) %)
   really-long-data))
