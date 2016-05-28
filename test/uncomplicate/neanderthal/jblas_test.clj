(ns uncomplicate.neanderthal.jblas-test
  (:require [clojure.test :refer :all])
  (:use [uncomplicate.neanderthal core java native]))

(def ^:dynamic *dge* dge-j)
(def ^:dynamic *dv* dv-j)

(use-fixtures :once
  (fn [run-tests]
    (let [implementations [{:name "Native" :dge dge :dv dv}
                           {:name "Java" :dge dge-j :dv dv-j}]]
      (doseq [implementation implementations]
        (binding [*dge* (:dge implementation)
                  *dv* (:dv implementation)]
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

(deftest mv-test
  (testing "matrix-vector multiplication"
    (is (= (*dv* 16 4 7)
           (mv (*dge* 3 2 [1 4 2 3 0 1])
             (*dv* 1 5))))))
           
    