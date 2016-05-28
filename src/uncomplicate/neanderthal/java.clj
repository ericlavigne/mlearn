(ns uncomplicate.neanderthal.java
  (:require [uncomplicate.neanderthal.core
             :refer [create create-vector create-ge-matrix]]
            [uncomplicate.neanderthal.impl.jblas
             :refer [jblas-double]]))

(defn dv-j
  "Creates slow (pure Clojure) double vector from source."
  ([source]
   (create-vector jblas-double source))
  ([x & xs]
   (dv-j (cons x xs))))

(defn dge-j
  "Creates slow (pure Clojure) double mxn matrix from source."
  ([^long m ^long n source]
   (create-ge-matrix jblas-double m n source))
  ([^long m ^long n]
   (create jblas-double m n)))
   
