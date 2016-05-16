(ns mlearn.week1-test
  (:require [clojure.test :refer :all]
            [mlearn.week1 :refer :all]))

(deftest model-tests
  (testing "Evaluate polynomial h(x) = 1.0 + 0.5x"
    (is (= 1.0 (evaluate-polynomial 0.0 [1.0 0.5])))
    (is (= 2.0 (evaluate-polynomial 2.0 [1.0 0.5])))
    (is (= 2.5 (evaluate-polynomial 3.0 [1.0 0.5])))))
