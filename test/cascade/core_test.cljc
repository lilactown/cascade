(ns cascade.core-test
  (:require
   [cascade.core :as c]
   [clojure.test :refer [deftest is]]))


(deftest t-identity
  (is (= 1 (c/identity inc 0))))


(deftest cont-with
  (let [conj-cont (c/cont-with conj)]
    (is (= 0 (conj-cont #(reduce + %) '())))
    (is (= 3 (conj-cont #(reduce + %) '() 1 2))))
  (let [conj-cont (c/cont-with conj '())]
    (is (= 0 (conj-cont #(reduce + %))))
    (is (= 3 (conj-cont #(reduce + %) 1 2))))
  (let [+-cont (c/cont-with + 0)]
    (is (= 0 (+-cont identity)))
    (is (= 3 (+-cont identity 1 2)))))


(deftest t-complement
  (let [odd?-cont (-> even?
                      (c/cont-with)
                      (c/complement))]
    (is (odd?-cont identity 1))
    (is (not (odd?-cont identity 2)))
    (is (odd?-cont not 2))))


(deftest t-reduce
  (is (= 6 (c/reduce
            (fn step [k acc n]
              (k (+ acc n)))
            0
            '(1 2 3))))
  ;; same as above w/ cont-with for reducing fn
  (is (= 6 (c/reduce
            (c/cont-with +)
            0
            '(1 2 3))))
  (is (= 10 (c/reduce
             (fn rf [k acc x]
               (if (coll? x)
                 (c/reduce #(k (+ acc %)) rf 0 x)
                 (k (+ acc x))))
             0
             '(1 (2 (3 4)))))
      "recursive continuation-passing"))

(deftest t-comp
  (let [f (c/comp
           (c/cont-with inc)
           (c/cont-with #(* 2 %))
           (fn [k x y]
             (k (+ x y))))]
    (is (= 5 (f identity 1 1)))))


(deftest t-map
  (is (= '(2 3 4) (c/map (c/cont-with inc) [1 2 3])))
  (is (= '([0 0 0] [1 1 1] [2 2 2] [3 3 3] [4 4 4])
         (trampoline
          (c/map identity (c/cont-with vector) (range 5) (range 5) (range 5))))))


(deftest t-filter
  (is (= '(2 4) (c/filter (c/cont-with even?) [1 2 3 4]))))


(deftest t-remove
  (is (= '(1 3) (c/remove (c/cont-with even?) [1 2 3 4]))))


(deftest t-keep
  (is (= '(3 5) (c/keep
                 (c/cont-with #(when (even? %) (inc %)))
                 [1 2 3 4]))))


(deftest t-some
  (is (= :c (c/some (c/cont-with #{:c}) #{:a :b :c :d})))
  (is (nil? (c/some (c/cont-with #{:e}) #{:a :b :c :d})))
  (is (= :c (c/some
             (fn predk [k x]
               (if (coll? x)
                 (c/some k predk x)
                 (k (#{:c} x))))
             [:a :b [[:c]] :d]))
      "nested"))


(deftest t-take
  (is (= '(1 2 3 4) (c/take 4 [1 2 3 4 5]))))


(deftest t-take-while
  (is (= '(2 4 6) (c/take-while (c/cont-with even?) [2 4 6 7 8 10 12]))))


(deftest t-drop
  (is (= '(4 5 6) (c/drop 3 [1 2 3 4 5 6]))))


(deftest t-drop-while
  (is (= '(7 8 9 10 11 12)
         (c/drop-while (c/cont-with even?) [2 4 6 7 8 9 10 11 12]))))


(deftest t-mapcat
  (is (= '(2 3 4 5 6 7)
         (trampoline
          c/mapcat identity
          (fn [k xs] (c/map k (c/cont-with inc) xs))
          [[1 2 3] [4 5 6]]))))


(deftest transducers
  (is (= 9 (c/transduce
            (c/map (c/cont-with inc))
            (c/cont-with +)
            #_(fn
                ([k acc] (k acc))
                ([k acc n] (k (+ acc n))))
            0
            '(1 2 3)))
      "map")
  (is (= [2 4] (c/transduce
                (c/filter (c/cont-with even?))
                (c/cont-with conj)
                []
                '(1 2 3 4)))
      "filter")
  (is (= [3 5] (c/transduce
                (c/keep (c/cont-with #(when (even? %) (inc %))))
                (c/cont-with conj)
                [] '(1 2 3 4)))
      "keep")
  (is (= [3 5] (c/transduce
                ;; use normal comp instead of c/cont since transducers
                ;; do not accept continuation
                (comp
                 (c/filter (c/cont-with even?))
                 (c/map (c/cont-with inc)))
                (c/cont-with conj)
                [] '(1 2 3 4)))
      "composition")
  (is (= 6 (c/transduce (c/take 3) (c/cont-with +) 0 [1 2 3 4 5 6])) "take")
  (is (= 12 (c/transduce
             (c/take-while (c/cont-with even?))
             (c/cont-with +) 0 [2 4 6 7 8 10 12]))
      "take-while")
  (is (= [4 5 6] (c/transduce (c/drop 3) (c/cont-with conj) [] [1 2 3 4 5 6]))
      "drop")
  (is (= [7 8 10 12]
         (c/transduce
          (c/drop-while (c/cont-with even?)) (c/cont-with conj)
          [] [2 4 6 7 8 10 12]))
      "drop-while")
  (is (= 21 (c/transduce c/cat (c/cont-with +) 0 [[1 2 3] [4 5 6]])) "cat")
  (is (= [2 3 4 5 6 7]
         (c/transduce
          (c/mapcat (fn [k xs]
                      (c/map k (c/cont-with inc) xs)))
          (c/cont-with conj)
          []
          [[1 2 3] [4 5 6]]))
      "mapcat"))


(deftest t-into
  (is (= #{1 2 3 4} (c/into #{} (c/map c/identity) '(1 2 3 4 3 2 1)))))


(deftest map-into
  (is (= '(2 3 4) (c/map-into (c/cont-with inc) '() [1 2 3]))))
