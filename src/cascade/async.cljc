(ns cascade.async
  (:require
   [cascade.core :as cc])
  (:refer-clojure :exclude [trampoline]))


(defprotocol IAwaitable
  (-then [a k fail]))


#?(:cljs
   (extend-protocol IAwaitable
     js/Promise
     (-then [p k fail]
       (.then p k fail))))


(def error-sentinel `error)


(defn trampoline
  [f]
  (fn [resolve reject]
    (letfn [(bounce! [f k fail]
              (let [ev (volatile! false)
                    ret (try
                          (f)
                          (catch #?(:cljs js/Error :clj Exception) e
                            (vreset! ev true)
                            e))]
                (cond
                  @ev (reject ret)

                  (fn? ret) (recur ret k fail)

                  (satisfies? IAwaitable ret)
                  (-then ret
                         (fn [ret]
                           (if (fn? ret)
                             (bounce! ret k fail)
                             (resolve ret)))
                         reject)

                  :else (resolve ret))))]
      (bounce! f resolve reject))))


(.then (js/Promise.
        (trampoline
         (cc/map
          vec
          (fn [k x]
            (-> (inc x)
                (js/Promise.resolve)
                (.then k)))
          [1 2 3])))
       prn)


(-> (trampoline
     (cc/transduce
      identity
      (comp
       (cc/map (cc/cont-with
                #(doto (inc %) (prn :foo))))
       (cc/map (fn [k x]
                 (-> (doto (inc x) (prn :bar))
                     (js/Promise.resolve)
                     (.then k))))
       (cc/map (cc/cont-with
                #(doto (inc %) (prn :baz))))
       (cc/take-while (cc/cont-with
                       #(< % 10))))
      (cc/cont-with conj)
      []
      (range 10)))
    (js/Promise.)
    (.then prn))
