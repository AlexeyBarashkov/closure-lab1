(ns barashkov.first-lab.core
  (:require [clojure.java.io :as io]
    [clojure.string :as str]))


(def ^:private distance-calculation-types #{"euclidean" "hamming"})


(defn- check-arguments
  [distance-calculation-type file-path]
  (reduce
    (fn [memo expression]
      (and memo expression))
    true
    [(not (str/blank? distance-calculation-type))
     (not (str/blank? file-path))
     (contains? distance-calculation-types distance-calculation-type)
     (.exists (io/file file-path))]))

(defn -main
  [distance-calculation-type file-path]
  (if (check-arguments distance-calculation-type file-path)
    (do
      (println "Distance type:" distance-calculation-type)
      (println "Source file:" file-path))
    (println "Wrong arguments"))
  )