(ns barashkov.first-lab.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def ^:private distance-calculation-types #{"euclidean" "hamming"})


(defn sqr [x]
  (* x x))

(defn- euclidean-distance
  [p1 p2]
  (let [i (atom -1)]
    (->>
      (reduce (fn [memo val]
                (do
                  (swap! i inc)
                  (+ memo (sqr (- val (get p2 @i))))))
              0 p1)
      (Math/sqrt))))

(defn- hamming-distance
  [p1 p2]
  (let [i (atom -1)]
    (reduce (fn [memo val]
              (do
                (swap! i inc)
                (if (not= val (get p2 @i))
                  (inc memo)
                  memo)))
            0 p1)))

(defn- get-distance-function
  [distance-calculation-type]
  (case distance-calculation-type
    "euclidean" euclidean-distance
    "hamming" hamming-distance))

(defn- parse-string
  [str]
  (->> (str/split str #",")
       (butlast)
       (reduce (fn [memo val]
                 (conj memo (Double/parseDouble val)))
               [])))

; read file to list of hash-maps
(defn- get-points
  [filename]
  (let [i (atom -1)]
    (->> (io/reader filename)
         (line-seq)
         (reduce (fn [memo str]
                   (if (not (str/blank? str))
                     (do
                       (swap! i inc)
                       (conj memo
                             (hash-map :index @i :value (parse-string str))))
                     memo))
                 []))))

; check if arguments are available
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
      (println "Source file:" file-path)
      (let [points (get-points file-path)
            get-distance (get-distance-function distance-calculation-type)]
            ;kernels (clusterize get-distance points)]
        (println points)))
    (println "Wrong arguments"))
  )