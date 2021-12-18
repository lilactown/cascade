(ns lilactown.hike-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lilactown.hike :as hike]))


(deftest prewalk-order
  (is (= (let [a (atom [])]
           (hike/prewalk (fn [form] (swap! a conj form) form)
                      [1 2 {:a 3} (list 4 [5])])
           @a)
         [[1 2 {:a 3} (list 4 [5])]
          1 2 {:a 3} [:a 3] :a 3 (list 4 [5])
          4 [5] 5])))


(deftest postwalk-order
  (is (= (let [a (atom [])]
           (hike/postwalk (fn [form] (swap! a conj form) form)
                       [1 2 {:a 3} (list 4 [5])])
           @a)
         [1 2
          :a 3 [:a 3] {:a 3}
          4 5 [5] (list 4 [5])
          [1 2 {:a 3} (list 4 [5])]])))


(defrecord Foo [a b c])

;; (defmulti get-comparator type)

;; (defmethod get-comparator PersistentTreeMap
;;   [o] (.-comp o))

;; (defmethod get-comparator PersistentTreeSet
;;   [o] (get-comparator (.-tree-map o)))


(deftest walk
  "Checks that visit returns the correct result and type of collection"
  (let [colls ['(1 2 3)
               [1 2 3]
               #{1 2 3}
               (sorted-set-by > 1 2 3)
               {:a 1, :b 2, :c 3}
               #_(sorted-map-by > 1 10, 2 20, 3 30)
               (->Foo 1 2 3)
               (map->Foo {:a 1 :b 2 :c 3 :extra 4})]]
    (doseq [c colls]
      (let [visited (trampoline hike/walk
                                identity
                                identity identity
                                c)]
        (is (= c visited))
				;;(is (= (type c) (type walked)))
        #_#_(if (map? c)
              (is (= (hike/walk #(update-in % [1] inc) #(reduce + (vals %)) c)
                     (reduce + (map (comp inc val) c))))
              (is (= (hike/walk inc #(reduce + %) c)
                     (reduce + (map inc c)))))
          (when (or (instance? clojure.lang.PersistentTreeMap c)
                    (instance? clojure.lang.PersistentTreeSet c))
            (is (= (.comparator c) (.comparator walked))))))))


(comment
  (trampoline
   hike/walk
   identity
   inc
   identity
   '(1 2 3))

  (trampoline
   hike/walk
   identity
   identity
   #(reduce + %)
   '(1 2 3))

  (require '[clojure.walk :as w])

  (w/walk #(doto % prn) identity '(1 2 3))

  (w/walk identity #(doto % prn) '(1 2 3))
  )
