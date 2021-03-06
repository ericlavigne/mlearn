(ns uncomplicate.neanderthal.impl.jblas
  (:require [vertigo.bytes :refer [direct-buffer]]
            [uncomplicate.commons.core :refer [with-release]]
            [uncomplicate.neanderthal
             [protocols :refer :all]
             [core :refer [dim ecount]]
             [block :refer [order buffer stride]]]
            [uncomplicate.neanderthal.impl.buffer-block :refer :all])
  (:import [java.nio ByteBuffer DirectByteBuffer]
           [uncomplicate.neanderthal.protocols
            BLAS BLASPlus Vector Matrix RealVector
            DataAccessor BufferAccessor]))

(defn unimplemented [description]
  (throw (Exception. (str "Unimplemented method " description))))

(deftype DoubleVectorEngine []
  BLAS
  (swap [_ x y] (unimplemented "vector swap"))
  (copy [_ x y] (unimplemented "vector copy"))
  (dot [_ x y] (unimplemented "vector dot"))
  (nrm2 [_ x] (unimplemented "vector nrm2"))
  (asum [_ x] (unimplemented "vector asum"))
  (iamax [_ x] (unimplemented "vector iamax"))
  (rot [_ x y c s] (unimplemented "vector rot"))
  (rotg [_ x] (unimplemented "vector rotg"))
  (rotm [_ x y p] (unimplemented "vector rotm"))
  (rotmg [_ p args] (unimplemented "vector rotmg"))
  (scal [_ alpha x] (unimplemented "vector scal"))
  (axpy [_ alpha x y] (unimplemented "vector axpy"))
  BLASPlus
  (subcopy [_ x y kx lx ky] (unimplemented "vector subcopy"))
  (sum [_ x] (unimplemented "vector sum"))
  (imax [_ x] (unimplemented "vector imax"))
  (imin [_ x] (unimplemented "vector imin")))

(defn block-matrix-m [b]
  (if (= (.order b) COLUMN_MAJOR)
    (.stride b)
    (/ (.count b) (.stride b))))
  
(defn block-matrix-n [b]
  (if (= (.order b) ROW_MAJOR)
    (.stride b)
    (/ (.count b) (.stride b))))

(defn block-matrix-index [b i j]
  (let [col-order (= (.order b) COLUMN_MAJOR)]
    (+ (.offset b)
       (* (.stride b) (if col-order j i))
       (if col-order i j))))

(defn block-matrix-get [b i j]
  (.get double-accessor
        (.buffer b)
        (block-matrix-index b i j)))

(defn block-matrix-set [b i j val]
  (.set double-accessor (.buffer b)
        (block-matrix-index b i j)
        val))

(defn block-vector-n [b]
  (.count b))

(defn block-vector-index [b i]
  (+ (.offset b) i))

(defn block-vector-get [b i]
  (.get double-accessor
    (.buffer b)
    (block-vector-index b i)))

(defn block-vector-set [b i val]
  (.set double-accessor (.buffer b)
    (block-vector-index b i)
    val))

(deftype DoubleGeneralMatrixEngine []
  BLAS
  (swap [_ x y] (unimplemented "matrix swap"))
  (copy [_ x y]
    (doseq [i (range (block-matrix-m x))
            j (range (block-matrix-n x))]
      (block-matrix-set y i j
        (block-matrix-get x i j))))
  (dot [_ x y] (unimplemented "matrix dot"))
  (nrm2 [_ x] (unimplemented "matrix nrm2"))
  (asum [_ x] (unimplemented "matrix asum"))
  (iamax [_ x] (unimplemented "matrix iamax"))
  (rot [_ x y c s] (unimplemented "matrix rot"))
  (rotg [_ x] (unimplemented "matrix rotg"))
  (rotm [_ x y p] (unimplemented "matrix rotm"))
  (rotmg [_ p args] (unimplemented "matrix rotmg"))
  (scal [_ alpha x] (unimplemented "matrix scal"))
  (axpy [_ alpha x y]
    (doseq [i (range (block-matrix-m x))
            j (range (block-matrix-n x))]
      (block-matrix-set y i j
        (+ (block-matrix-get y i j)
           (* alpha (block-matrix-get x i j))))))
  (mv [_ alpha a x beta y]
    (let [m (block-matrix-m a)
          n (block-matrix-n a)
          m-range (range m)
          n-range (range n)]
      (doseq [i m-range]
        (block-vector-set y i
          (+ (* beta (block-vector-get y i))
             (* alpha (reduce (fn [acc j]
                                (+ acc
                                   (* (block-matrix-get a i j)
                                      (block-vector-get x j))))
                              0.0 n-range)))))))
  (rank [_ alpha x y a] (unimplemented "matrix rank"))
  (mm [_ alpha a b beta c]
    (let [m (block-matrix-m a)
          n (block-matrix-n a)
          o (block-matrix-n b)
          m-range (range m)
          n-range (range n)
          o-range (range o)]
      (doseq [mi m-range
              oi o-range]
        (block-matrix-set c mi oi
          (+ (* beta (block-matrix-get c mi oi))
             (* alpha (reduce (fn [acc ni]
                                (+ acc
                                   (* (block-matrix-get a mi ni)
                                      (block-matrix-get b ni oi))))
                        0.0 n-range)))))))
  BLASPlus
    (subcopy [_ x y kx lx ky] (unimplemented "matrix subcopy"))
    (sum [_ x] (unimplemented "matrix sum"))
    (imax [_ x] (unimplemented "matrix imax"))
    (imin [_ x] (unimplemented "matrix imin")))
  
  
(deftype JblasFactory [^DataAccessor acc ^BLAS vector-eng ^BLAS matrix-eng]
  DataAccessorProvider
  (data-accessor [_]
    acc)
  Factory
  (create-vector [this n buf _]
    (if (and (<= 0 (long n) (.count acc buf))
             (instance? ByteBuffer buf) (.isDirect ^ByteBuffer buf))
      (->RealBlockVector this acc vector-eng (.entryType acc) true buf n 1)
      (throw (IllegalArgumentException.
              (format "I can not create an %d element vector from %d-element %s."
                      n (.count acc buf) (class buf))))))
  (create-matrix [this m n buf order]
    (if (and (<= 0 (* (long m) (long n)) (.count acc buf))
             (instance? ByteBuffer buf) (.isDirect ^ByteBuffer buf))
      (let [order (or order DEFAULT_ORDER)
            ld (max (long (if (= COLUMN_MAJOR order) m n)) 1)]
        (->RealGeneralMatrix this acc matrix-eng (.entryType acc) true
                             buf m n ld order))
      (throw (IllegalArgumentException.
              (format "I do not know how to create a %dx%d matrix from %s."
                      m n (type buf))))))
  (vector-engine [_]
    vector-eng)
  (matrix-engine [_]
    matrix-eng))
  
(def jblas-double
  (->JblasFactory double-accessor
                  (->DoubleVectorEngine)
                  (->DoubleGeneralMatrixEngine)))
                    
