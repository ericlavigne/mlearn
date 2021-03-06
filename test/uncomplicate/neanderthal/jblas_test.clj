(ns uncomplicate.neanderthal.jblas-test
  (:require [clojure.test :refer :all])
  (:use [uncomplicate.neanderthal core java native]))

(def ^:dynamic *dge* dge-j)
(def ^:dynamic *dv* dv-j)

(use-fixtures :once
  (fn [run-tests]
    (let [implementations [{:name "Native" :dge dge :dv dv}
                           ;{:name "GPU" :dge clge :dv clv}
                           {:name "Java" :dge dge-j :dv dv-j}]]
                           
      (doseq [implementation implementations]
        (binding [*dge* (:dge implementation)
                  *dv* (:dv implementation)]
          (println (str "Running " (:name implementation) " matrix tests."))
          (time
            (run-tests)))))))

(defn matrix= [a b]
  (and (= (ncols a) (ncols b))
       (every?
         #(< (Math/abs %) 0.001)
         (map -
           (flatten (seq a))
           (flatten (seq b))))))

(defn random-seq [n]
  (vec (map
         (fn [i] (- (rand 2) 1))
         (range n))))

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
           (mv
             (*dge* 3 2 [1 4 2 3 0 1])
             (*dv* 1 5))))))
           
(deftest mm-test
  (testing "matrix-matrix multiplication"
    (is (matrix= (*dge* 2 2 [11 9 10 14])
                 (mm
                   (*dge* 2 3 [1 4 3 0 2 1])
                   (*dge* 3 2 [1 0 5 3 1 2]))))
    (is (matrix= (*dge* 4 3 [486   314   343.5   173
                             410.4 341.6 353.4   285.2
                             691.6 416.4 463.6   190.8])
                 (mm
                   (*dge* 4 2 [1 1 1 1 2104 1416 1534 852])
                   (*dge* 2 3 [-40 0.25 200 0.1 -150 0.4]))))))


(def random-matrix-multiplication-example
  (let [m 32
        n 32
        o 32
        a (random-seq (* m n))
        b (random-seq (* n o))
        a-mat (dge m n a)
        b-mat (dge n o b)
        c-mat (mm a-mat b-mat)
        c (vec (flatten (seq c-mat)))]
    {:a a :b b :c c :m m :n n :o o}))

(deftest random-matrix-multiplication-test
  (testing "multiply two large (256x256) matrices"
    (let [m (:m random-matrix-multiplication-example)
          n (:n random-matrix-multiplication-example)
          o (:o random-matrix-multiplication-example)]
      (is (matrix= (*dge* m o (:c random-matrix-multiplication-example))
            (mm
              (*dge* m n (:a random-matrix-multiplication-example))
              (*dge* n o (:b random-matrix-multiplication-example))))))))
