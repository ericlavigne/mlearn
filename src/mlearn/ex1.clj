(ns mlearn.ex1
  (:require [mlearn.io :as io]))

(def dataset1
  (map (fn [row]
         {:x (Double/parseDouble (get row 0))
          :y (Double/parseDouble (get row 1))})
    (io/load-csv-resource "ex1data1.txt")))
