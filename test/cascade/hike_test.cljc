(ns cascade.hike-test
  (:require
   [clojure.test :refer [deftest is]]
   [cascade.core :as c]
   [cascade.hike :as w]))


(deftest prewalk-replace
  (is (= [:b {:b :b} (list 3 :c :b)]
         (w/prewalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)]))))


(deftest postwalk-replace
  (is (= [:b {:b :b} (list 3 :c :b)]
         (w/postwalk-replace {:a :b} [:a {:a :a} (list 3 :c :a)]))))


(deftest stringify-keys
  (is (= {"a" 1, nil {"b" 2 "c" 3}, "d" 4}
         (w/stringify-keys {:a 1, nil {:b 2 :c 3}, :d 4}))))


(deftest keywordize-keys
  (is (= {:a 1, nil {:b 2, :c 3}, :d 4}
         (w/keywordize-keys {"a" 1, nil {"b" 2, "c" 3}, "d" 4}))))


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

#?(:cljs
   (do (defmulti get-comparator type)

       (defmethod get-comparator PersistentTreeMap
         [o] (.-comp o))

       (defmethod get-comparator PersistentTreeSet
         [o] (get-comparator (.-tree-map o)))))


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
      (let [walked (trampoline
                    w/walk
                    c/identity ; inner
                    identity ; outer
                    c)]
        (is (= c walked))
        (is (= (type c) (type walked)))
        (if (map? c)
          (is (= (reduce + (map (comp inc val) c))
                 (trampoline
                  w/walk
                  (fn inner [k x]
                    (k (update-in x [1] inc)))
                  (fn outer [x]
                    (reduce + (vals x)))
                  c)))
          (is (= (reduce + (map inc c))
                 (trampoline
                  w/walk
                  (fn inner [k x]
                    (k (inc x)))
                  (fn outer [x]
                    (reduce + x))
                  c))))
        #?(:clj
           (when (or (instance? clojure.lang.PersistentTreeMap c)
                     (instance? clojure.lang.PersistentTreeSet c))
             (is (= (.comparator c) (.comparator walked))))
           :cljs
           (when (or (instance? PersistentTreeMap c)
                     (instance? PersistentTreeSet c))
             (is (= (get-comparator c) (get-comparator walked)))))))))


(deftest walk-mapentry
  (let [coll [:html {:a ["b" 1]} ""]
        f (fn [e] (if (and (vector? e) (not (map-entry? e))) (apply list e) e))]
    (is (= (list :html {:a (list "b" 1)} "") (w/postwalk f coll)))))


(deftest walk-really-nested-data
  (letfn [(create [k depth]
            (if (zero? depth)
              (k {:id depth})
              #(create
                (fn [c]
                  (fn [] (k {:id depth :child c})))
                (dec depth))))]
    (let [limit 100000
          data (trampoline create identity limit)]
      ;; can't call = here because it overflows the stack
      (is (c/eq data
                (w/postwalk
                 identity
                 data)))
      (is (not (c/eq data (w/postwalk
                           #(if (number? %) (inc %) %)
                           data))))
      (is (= (inc limit) (:id (w/postwalk
                               #(if (number? %) (inc %) %)
                               data))))
      (is (= (inc limit) (:id (w/prewalk
                               #(if (number? %) (inc %) %)
                               data))))
      (is (= nil (:id (w/prune (every-pred number? even?) data)))))))


(deftest seek
  (is (= 2 (w/seek even? '(1 2 3 4))))
  (is (= 2 (w/seek (every-pred number? even?) '(1 (2 (3 (4)))))))
  (is (= 4 (w/seek (every-pred number? even?) '(4 (3 (2 (1)))))))
  (is (= nil (w/seek (every-pred number? even?) '(1 3 (5)))))
  (let [data {:id 0
              :children [{:id 1}
                         {:id 2}
                         {:id 3 :children [{:id 4}]}]}]
    (is (= {:id 3 :children [{:id 4}]}
           (w/seek (every-pred map? #(= 3 (:id %))) data)))))


(deftest prune
  (is (= '(1 (3 [5 7 {9 nil} #{15 13}]))
         (w/prune
          (every-pred number? even?)
          '(1 (2 3 4 (5 6 7 8 {9 10 12 11} #{13 14 15 16})))))))
