(ns mlearn.regression
  (:require [mlearn.matrix :refer :all]
            [uncomplicate.neanderthal.core :refer [axpy axpy! col mm scal! trans]]
            [uncomplicate.neanderthal.math :refer [pow]]
            [uncomplicate.neanderthal.native :refer :all]
            [uncomplicate.neanderthal.real :refer [entry]]))

(defn powers
  "(powers x n) -> [x^0 x^1 x^2 ... x^(n-1)]"
  [x n]
  (map
    #(pow x %)
    (range n)))

(defn evaluate-polynomial
  "h(x) = theta0 + theta1 * x + theta2 * x^2 * ..."
  [x thetas]
  (let [x-powers (powers x (count thetas))]
    (apply + (map * thetas x-powers))))

(defn create-polynomial-cost-function
  "Returns cost function J(thetas) = average((h(x) - y)^2)/2"
  [xy-pairs count-thetas]
  (let [theta-exponents (range count-thetas)
        xs (map :x xy-pairs)
        ys (map :y xy-pairs)
        sample-size (count xy-pairs)
        y-matrix (dge 1 sample-size ys)
        x-powers (mapcat #(powers % count-thetas) xs)
        x-matrix (dge count-thetas sample-size x-powers)
        one-over-m (/ 1.0 sample-size)
        one-over-2m (/ 0.5 sample-size)]
    {:cost
     (fn [thetas]
       (let [h (mm
                 (dge 1 count-thetas thetas)
                 x-matrix)
             diff (axpy -1 y-matrix h)
             diff-squared (dot diff diff)]
         (* one-over-2m diff-squared)))
     :cost-derivative
     (fn [thetas]
       (let [h (mm
                 (dge 1 count-thetas thetas)
                 x-matrix)
             diff (axpy -1 y-matrix h)
             derivative (mm one-over-m x-matrix (trans diff))]
         (vec (seq (col derivative 0)))))}))

(defn polynomial-gradient-descent
  "Uses bold driver to optimize alpha. Will try adding momentum later.
   https://www.willamette.edu/~gorr/classes/cs449/momrate.html"
  [xy-pairs num-terms tolerance]
  (let [cost (create-polynomial-cost-function xy-pairs num-terms)
        cost-fn (:cost cost)
        cost-derivative-fn (:cost-derivative cost)
        initial-theta (repeat num-terms 0.0)]
    (loop [i 0
           cost (cost-fn initial-theta)
           derivative (cost-derivative-fn initial-theta)
           theta initial-theta
           alpha 1.0]
      (let [new-theta (map (fn [th d]
                             (- th (* alpha d)))
                        theta derivative)
            new-cost (cost-fn new-theta)
            new-derivative (cost-derivative-fn new-theta)
            worse (> new-cost cost)
            new-alpha (* alpha (if worse 0.5 1.1))
            largest-derivative-change (apply max (map #(Math/abs %) new-derivative))]
        (println (str i ") alpha " alpha " cost " cost " theta " (vec theta)
                   " derivative " (vec derivative)))
        (if worse
          (recur (inc i) cost derivative theta new-alpha)
          (if (< largest-derivative-change tolerance)
              new-theta
              (recur (inc i) new-cost new-derivative new-theta new-alpha)))))))
