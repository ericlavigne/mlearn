(ns mlearn.matrix
  (:require [uncomplicate.neanderthal.core :as n]
            [uncomplicate.neanderthal.real :as nr]))

(defn dot [m1 m2]
  (let [res-mat (n/mm
                  (if (= 1 (n/mrows m1))
                    m1 (n/trans m1))
                  (if (= 1 (n/mrows m2))
                    (n/trans m2) m2))]
    (nr/entry res-mat 0 0)))
