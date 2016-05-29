(ns mlearn.regression-test
  (:require [clojure.test :refer :all]
            [mlearn.regression :refer :all]))

(deftest model-tests
  (testing "Evaluate polynomial h(x) = 1.0 + 0.5x"
    (is (= 1.0 (evaluate-polynomial 0.0 [1.0 0.5])))
    (is (= 2.0 (evaluate-polynomial 2.0 [1.0 0.5])))
    (is (= 2.5 (evaluate-polynomial 3.0 [1.0 0.5])))))

(deftest cost-function-tests
  (testing "Cost function for perfectly linear training set"
    (let [data [{:x 0 :y 1} {:x 1 :y 1.5} {:x 2 :y 2} {:x 3 :y 2.5}]
          cost (create-cost-function data 2)
          correct-model [1 0.5]
          close-model [0.8 0.6]
          far-model [3 -2]]
      (is (= 0.0 (cost correct-model)))
      (is (< 0
            (cost close-model)
            (cost far-model))))))
