(ns mlearn.io
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn load-csv-resource [filename]
  (with-open [f (io/reader (io/file (io/resource filename)))]
    (doall (csv/read-csv f))))