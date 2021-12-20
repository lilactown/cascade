(ns cascade.cont-test
  (:require
   [cascade.cont :as cont]
   [clojure.test :refer [deftest is testing]]))


(deftest t-identity
  (is (= 1 (cont/identity inc 0))))


(deftest cont-with
  (let [conj-cont (cont/cont-with conj)]
    (is (= 0 (conj-cont #(reduce + %) '())))
    (is (= 3 (conj-cont #(reduce + %) '() 1 2))))
  (let [conj-cont (cont/cont-with conj '())]
    (is (= 0 (conj-cont #(reduce + %))))
    (is (= 3 (conj-cont #(reduce + %) 1 2))))
  (let [+-cont (cont/cont-with + 0)]
    (is (= 0 (+-cont identity)))
    (is (= 3 (+-cont identity 1 2)))))


(deftest t-complement
  (let [odd?-cont (-> even?
                      (cont/cont-with)
                      (cont/complement))]
    (is (odd?-cont identity 1))
    (is (not (odd?-cont identity 2)))
    (is (odd?-cont not 2))))


(deftest t-reduce
  (is (= 6 (cont/reduce
            (fn step [k acc n]
              (k (+ acc n)))
            0
            '(1 2 3))))
  ;; same as above w/ cont-with for reducing fn
  (is (= 6 (cont/reduce
            (cont/cont-with +)
            0
            '(1 2 3))))
  (is (= 10 (cont/reduce
             (fn rf [k acc x]
               (if (coll? x)
                 (cont/reduce #(k (+ acc %)) rf 0 x)
                 (k (+ acc x))))
             0
             '(1 (2 (3 4)))))
      "recursive continuation-passing"))

(deftest t-comp
  (let [f (cont/comp
           (cont/cont-with inc)
           (cont/cont-with #(* 2 %))
           (fn [k x y]
             (k (+ x y))))]
    (is (= 5 (f identity 1 1)))))


(deftest t-map
  (is (= '(2 3 4) (cont/map (cont/cont-with inc) [1 2 3]))))


(deftest t-filter
  (is (= '(2 4) (cont/filter (cont/cont-with even?) [1 2 3 4]))))


(deftest t-remove
  (is (= '(1 3) (cont/remove (cont/cont-with even?) [1 2 3 4]))))


(deftest t-keep
  (is (= '(3 5) (cont/keep
                 (cont/cont-with #(when (even? %) (inc %)))
                 [1 2 3 4]))))


(deftest transducers
  (is (= 9 (cont/transduce
            (cont/map (cont/cont-with inc))
            (cont/cont-with +)
            #_(fn
                ([k acc] (k acc))
                ([k acc n] (k (+ acc n))))
            0
            '(1 2 3))))
  (is (= [2 4] (cont/transduce
                (cont/filter (cont/cont-with even?))
                (cont/cont-with conj)
                []
                '(1 2 3 4))))
  (is (= [3 5] (cont/transduce
                (cont/keep (cont/cont-with #(when (even? %) (inc %))))
                (cont/cont-with conj)
                [] '(1 2 3 4)))))


(deftest t-into
  (is (= #{1 2 3 4} (cont/into #{} (cont/map cont/identity) '(1 2 3 4 3 2 1)))))


(deftest map-into
  (is (= '(2 3 4) (cont/map-into (cont/cont-with inc) '() [1 2 3]))))
