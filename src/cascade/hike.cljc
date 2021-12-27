(ns cascade.hike
  (:require
   [cascade.core :as c]))


(defn- map-entry
  [[mk mv]]
  #?(:clj (clojure.lang.MapEntry/create mk mv)
     :cljs (cljs.core/MapEntry. mk mv nil)))


(defn walk
  "Continuation-passing style, trampolined version of `clojure.walk/walk`.

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
  (trampoline
   walk
   (fn inner [k x]
     (walk inner #(k (f %)) x))
   f
   form))


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


(defn prune
  [pred form]
  (c/keep
   (fn step [k x]
     (if-not (pred x)
       (cond
         ;; if key doesn't match, remove the entry
         ;; if val doesn't match, replace it with nil
         (map-entry? x) (if-not (pred (key x))
                          (if-not (pred (val x))
                            (k x)
                            (k (assoc x 1 nil)))
                          (k nil))
         (record? k) (c/into k x (c/keep step) x)
         (coll? x) (c/into k (empty x) (c/keep step) x)
         :else (k x))
       (k nil)))
   form))
