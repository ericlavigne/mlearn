(ns uncomplicate.neanderthal.jblas-test
  (:require [clojure.test :refer :all])
  (:use [uncomplicate.neanderthal core java]))

(deftest ax-test
  (testing "scalar-matrix multiplication"
    (is (= (dge-j 3 2 [2 4 6 8 10 12])
           (ax 2 (dge-j 3 2 [1 2 3 4 5 6]))))))

