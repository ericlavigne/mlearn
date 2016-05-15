(ns mlearn.neanderthal-test
  (:require [clojure.test :refer :all])
  (:use [uncomplicate.neanderthal core native]))

(deftest neanderthal-tests
  (testing "Dot product"
    (is (= 140.0
          (dot
            (dv 1 2 3)
            (dv 10 20 30)))))
  (testing "Matrix multiplication"
    (is (=
          (mm
            (dge 3 2 [1 2 3 4 5 6])
            (dge 2 3 [10 20 30 40 50 60]))
          (dge 3 3 [90 120 150 190 260 330 290 400 510])))))
