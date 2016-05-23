(ns uncomplicate.neanderthal.java
  (:require [uncomplicate.neanderthal.core
             :refer [create create-vector create-ge-matrix]]
            [uncomplicate.neanderthal.impl.jblas
             :refer [jblas-double]]))

(defn dge-j
  ([^long m ^long n source]
   (create-ge-matrix jblas-double m n source))
  ([^long m ^long n]
   (create jblas-double m n)))
   
