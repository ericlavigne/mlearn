(ns uncomplicate.neanderthal.jblas-test
  (:require [clojure.test :refer :all])
  (:use [uncomplicate.neanderthal core java native]))

(def ^:dynamic *dge* dge-j)

(use-fixtures :once
  (fn [run-tests]
    (let [implementations [{:name "Native" :dge dge}
                           {:name "Java" :dge dge-j}]]
      (doseq [implementation implementations]
        (binding [*dge* (:dge implementation)]
          (println (str "Running " (:name implementation) " matrix tests."))
          (time
            (run-tests)))))))

(deftest ax-test
  (testing "scalar-matrix multiplication"
    (is (= (*dge* 3 2 [2 4 6 8 10 12])
           (ax 2 (*dge* 3 2 [1 2 3 4 5 6]))))))

(deftest axpy-test
  (testing "matrix-matrix addition"
    (is (= (*dge* 3 2 [5 4 3 0.5 10 2])
           (axpy (*dge* 3 2 [1 2 3 0   5 1])
                 (*dge* 3 2 [4 2 0 0.5 5 1]))))))