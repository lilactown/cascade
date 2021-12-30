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
  (is (= '(2 3 4) (c/map (c/cont-with inc) [1 2 3]))))


(deftest t-filter
  (is (= '(2 4) (c/filter (c/cont-with even?) [1 2 3 4]))))


(deftest t-remove
  (is (= '(1 3) (c/remove (c/cont-with even?) [1 2 3 4]))))


(deftest t-keep
  (is (= '(3 5) (c/keep
                 (c/cont-with #(when (even? %) (inc %)))
                 [1 2 3 4]))))


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
      "composition"))


(deftest t-into
  (is (= #{1 2 3 4} (c/into #{} (c/map c/identity) '(1 2 3 4 3 2 1)))))


(deftest map-into
  (is (= '(2 3 4) (c/map-into (c/cont-with inc) '() [1 2 3]))))
