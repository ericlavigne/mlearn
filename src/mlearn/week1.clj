(ns mlearn.week1
  (:require [clojure.math.numeric-tower :as math]))

(defn sum [list-of-numbers]
  (reduce + 0.0 list-of-numbers))

(defn evaluate-polynomial
  "h(x) = theta0 + theta1 * x + theta2 * x^2 * ..."
  [x thetas]
  (let [x-powers (map #(math/expt x %)
                   (range (count thetas)))]
    (sum (map * thetas x-powers))))
