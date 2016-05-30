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
             derivative (dv count-thetas)]
         (doseq [i (range sample-size)]
           (axpy!
             (entry diff 0 i)
             (col x-matrix i)
             derivative))
         (scal! one-over-m derivative)
         (vec (seq derivative))))}))
