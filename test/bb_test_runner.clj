(ns bb-test-runner
  (:require
   [clojure.test :as t]
   [cascade.core-test]
   [cascade.hike-test]))

(defn run-tests
  [& _args]
  (let [{:keys [fail error]}
        (t/run-tests
         'cascade.core-test
         'cascade.hike-test)]
    (when (or (pos? fail)
              (pos? error))
      (System/exit 1))))
