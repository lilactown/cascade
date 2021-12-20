(ns cascade.walk
  (:require
   [cascade.core :as c]))


(defn map-entry
  [[mk mv]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn walk
  "Continuation-passing style version of `clojure.walk/walk`.

  Traverses `form`, an arbitrary data structure. `inner` is a function that
  accepts a continuation and a value. `outer` is a funcion that accepts
  a single value.

  Calls (inner k el) for each element of `form`, building up a data structure of
  the same type, then calls (outer result). When `k` is called, it will continue
  the traversal to the next element in the structure. If not called, this
  short-circuits the traversal and whatever value is returned by `inner` will be
  passed to `outer`.

  Returns a single-arity function for use with `trampoline`.
  See `prewalk` and `postwalk` for more user-friendly variations."
  [inner outer form]
  (if (coll? form)
    (c/map-into
     (if (map-entry? form)
       #(outer (map-entry %))
       #(outer %))
     (fn step [k item]
       (inner k item))
     (cond
       (map-entry? form) []
       (record? form) form
       :else (empty form))
     form)
    #(outer form)))


(defn postwalk
  "Like `clojure.walk/postwalk`, but works for extremely large data structures.
  Performs a depth-first, post-order traversal of form.  Calls f on each
  sub-form, uses f's return value in place of the original. Recognizes all
  Clojure data structures. Consumes seqs as with doall."
  [f form]
  (letfn [(outer [x] (f x))]
    (trampoline
     walk
     (fn inner [k x]
       (walk inner #(k (outer %)) x))
     outer
     form)))


(defn prewalk
  "Like `clojure.walk/prewalk`, but works for extremely large data structures.
  Similar to `postwalk` but does pre-order traversal."
  [f form]
  (trampoline
   walk
   (fn inner [k x]
     (walk inner k (f x)))
   identity ; outer
   (f form)))


(defn keywordize-keys
  "Recursively transforms all map keys from strings to keywords.
  Works for very large data structures."
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
  "Recursively transforms all map keys from keywords to strings.
  Works for very large data structures."
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
  replacement at the root of the tree first.
  Works for very large data structures."
  [smap form]
  (prewalk (fn [x] (if (contains? smap x) (smap x) x)) form))


#_(prewalk-replace
   {:a :x :b :y :c :z}
   '(:a :b (:c {:d 1})))
;; => (:x :y (:z {:d 1}))


(defn postwalk-replace
  "Recursively transforms form by replacing keys in smap with their
  values. Like clojure/replace but works on any data structure.  Does
  replacement at the leaves of the tree first.
  Works for very large data structures."
  [smap form]
  (postwalk (fn [x] (if (contains? smap x) (smap x) x)) form))


#_(postwalk-replace
   {:a :x :b :y :c :z}
   '(:a :b (:c {:d 1})))
;; => (:x :y (:z {:d 1}))


#?(:clj
   (defn macroexpand-all
     "Recursively performs all possible macroexpansions in form. Works for very
  large forms."
     [form]
     (prewalk (fn [x] (if (seq? x) (macroexpand x) x)) form)))


(defn seek
  "Traverses `form`, returning the first element that (pred el) is true or nil.
  May call `pred` multiple times per element."
  [pred form]
  (trampoline
   walk
   (fn inner [k x]
     (if (pred x)
       x
       (walk inner k x)))
   #(when (pred %) %) ;outer
   form))


#_(seek even? '(1 2 3))
;; => 2

(def none `none)
(def none? #{none})

(defn prune
  [pred form]
  (letfn [(outer [x]
            (cond
              (map? x) (into
                        (empty x)
                        (clojure.core/remove
                         #(none? (key %)))
                        x)
              (map-entry? x) (map-entry [(key x) (when-not (none? (val x))
                                                   (val x))])
              (list? x) (apply list (clojure.core/remove none? x))
              (coll? x) (into (empty x) (clojure.core/remove none?) x)
              :else x))]
    (trampoline
     walk
     (fn inner [k x]
       (if (pred x)
         (k none)
         (walk inner (comp k outer) x)))
     outer
     form))
  #_(cont/remove
   (fn [k x]
     )))


(comment
  (postwalk
   (fn [x] (if (number? x) (inc x) x))
   {1 2
    3 {4 [5]}})

  (postwalk
   #(doto % prn)
   {1 2
    3 {4 [5]}})

  (prewalk
   #(doto % prn)
   {1 2
    3 {4 [5]}})

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

  (update-in
   (postwalk
    #(if (number? %) (inc %) %)
    really-nested-data)
   [:foo :child :child :child]
   dissoc :child)

  (seek (every-pred map? #(= (dec limit) (:id %))) really-nested-data)

  (def really-long-data
    {:foo (for [i (range 0 limit)]
            {:id i
             :child {:id (+ i limit)}})})

  (postwalk
   #(if (number? %) (inc %) %)
   really-long-data)

  (seek (every-pred map? #(= (dec limit) (:id %))) really-long-data))
