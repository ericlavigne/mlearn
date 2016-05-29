(ns mlearn.matrix-test
  (:require [clojure.test :refer :all]
            [mlearn.matrix :refer :all]
            [uncomplicate.neanderthal.native :refer :all]))

(deftest dot-test
  (testing "dot product treats horizontal and vertical matrices as vectors"
    (is (= 14.0 (dot (dge 3 1 [1 2 3]) (dge 3 1 [1 2 3]))))
    (is (= 14.0 (dot (dge 3 1 [1 2 3]) (dge 1 3 [1 2 3]))))
    (is (= 14.0 (dot (dge 1 3 [1 2 3]) (dge 3 1 [1 2 3]))))
    (is (= 14.0 (dot (dge 1 3 [1 2 3]) (dge 1 3 [1 2 3]))))))
