(ns mlearn.week1
  (:use [uncomplicate.neanderthal core math native]))

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

(defn matrix-sum [matrix]
  (apply + (flatten (seq matrix))))

(defn create-cost-function
  "Returns cost function J(thetas) = average((h(x) - y)^2)/2"
  [xy-pairs count-thetas]
  (let [theta-exponents (range count-thetas)
        xs (map :x xy-pairs)
        ys (map :y xy-pairs)
        sample-size (count xy-pairs)
        y-matrix (dge 1 sample-size ys)
        x-powers (mapcat #(powers % count-thetas) xs)
        x-matrix (dge count-thetas sample-size x-powers)
        one-over-2m (/ 0.5 sample-size)]
    (fn [thetas]
      (let [h (mm
                (dge 1 count-thetas thetas)
                x-matrix)
            diff (axpy -1 h y-matrix)
            diff-squared (matrix-sum (mm diff (trans diff)))]
        (* one-over-2m diff-squared)))))
