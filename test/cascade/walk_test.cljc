(ns casecade.walk-test
  (:require
   [clojure.test :refer [deftest is]]
   [cascade.cont :as cont]
   [cascade.walk :as w]))


(deftest prewalk-replace
  (is (= [:b {:b :b} (list 3 :c :b)]
         (w/prewalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)]))))


(deftest postwalk-replace
  (is (= [:b {:b :b} (list 3 :c :b)]
         (w/postwalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)]))))


(deftest stringify-keys
  (is (= {"a" 1, nil {"b" 2 "c" 3}, "d" 4}
         (w/stringify-keys {:a 1, nil {:b 2 :c 3}, :d 4}))))


(deftest prewalk-order
  (is (= [[1 2 {:a 3} (list 4 [5])]
          1 2 {:a 3} [:a 3] :a 3 (list 4 [5])
          4 [5] 5]
         (let [a (atom [])]
           (w/prewalk (fn [form] (swap! a conj form) form)
                         [1 2 {:a 3} (list 4 [5])])
           @a))))


(deftest postwalk-order
  (is (= [1 2
          :a 3 [:a 3] {:a 3}
          4 5 [5] (list 4 [5])
          [1 2 {:a 3} (list 4 [5])]]
         (let [a (atom [])]
           (w/postwalk (fn [form] (swap! a conj form) form)
                          [1 2 {:a 3} (list 4 [5])])
           @a))))


(defrecord Foo [a b c])

;; (defmulti get-comparator type)

;; (defmethod get-comparator PersistentTreeMap
;;   [o] (.-comp o))

;; (defmethod get-comparator PersistentTreeSet
;;   [o] (get-comparator (.-tree-map o)))


(deftest walk
  (let [colls ['(1 2 3)
               [1 2 3]
               #{1 2 3}
               (sorted-set-by > 1 2 3)
               {:a 1, :b 2, :c 3}
               (sorted-map-by > 1 10, 2 20, 3 30)
               (->Foo 1 2 3)
               (map->Foo {:a 1 :b 2 :c 3 :extra 4})]]
    (doseq [c colls]
      (let [walked (w/walk
                    cont/identity ; inner
                    cont/identity ; outer
                    c)]
        (is (= c walked))
        (is (= (type c) (type walked)))
        (if (map? c)
          (is (= (reduce + (map (comp inc val) c))
                 (w/walk
                  (fn inner [k x]
                    (k (update-in x [1] inc)))
                  (fn outer [k x]
                    (reduce + (vals x)))
                  c)))
          (is (= (reduce + (map inc c))
                 (w/walk
                  (fn inner [k x]
                    (k (inc x)))
                  (fn outer [k x]
                    (k (reduce + x)))
                  c))))
        (when (or (instance? clojure.lang.PersistentTreeMap c)
                  (instance? clojure.lang.PersistentTreeSet c))
          (is (= (.comparator c) (.comparator walked))))))))


(deftest walk-mapentry
  (let [coll [:html {:a ["b" 1]} ""]
        f (fn [e] (if (and (vector? e) (not (map-entry? e))) (apply list e) e))]
    (is (= (list :html {:a (list "b" 1)} "") (w/postwalk f coll)))))
