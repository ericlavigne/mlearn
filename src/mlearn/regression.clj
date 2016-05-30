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

(defn polynomial-gradient-descent [xy-pairs num-terms tolerance]
  (let [cost (create-polynomial-cost-function xy-pairs num-terms)
        cost-fn (:cost cost)
        cost-derivative-fn (:cost-derivative cost)
        initial-theta (repeat num-terms 0.0)
        derivative-zero (repeat num-terms 0.0)
        initial-alpha (repeat num-terms 1.0)
        initial-cost (cost-fn initial-theta)
        initial-derivative (cost-derivative-fn initial-theta)]
    (loop [theta initial-theta
           prev-derivative derivative-zero
           alpha initial-alpha
           cost initial-cost
           derivative initial-derivative]
      (let [new-alpha (map (fn [d1 d2 a] (if (> (* d1 d2) 0)
                                           (* a 1.2)
                                           (* a 0.8)))
                        prev-derivative
                        derivative
                        alpha)
            new-theta (map (fn [th a d]
                             (- th (* a d)))
                        theta new-alpha derivative)
            new-cost (cost-fn new-theta)]
        ;(println (str "alpha " (vec alpha) " cost " cost " theta " (vec theta)))
        (if (> new-cost cost)
          (recur
            theta prev-derivative
            (map #(* 0.2 %) alpha)
            cost derivative)
          (let [new-derivative (cost-derivative-fn new-theta)]
            (if (> tolerance (apply max (map #(Math/abs %) new-derivative)))
              new-theta
              (recur new-theta derivative new-alpha new-cost new-derivative))))))))
              
              
