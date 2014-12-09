(ns first-lab.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- sqr [x]
  (* x x))


(def ^:private r-a 3.0)
(def ^:private alpha (/ 4 (sqr r-a)))
(def ^:private r-b (* 1.5 r-a))
(def ^:private betta (/ 4 (sqr r-b)))
(def ^:private distance-calculation-types #{"euclidean" "hamming"})
(def ^:private e_max 0.5)
(def ^:private e_min 0.15)

(defn- exponent
  [param distance]
  (Math/exp (- (* param distance))))


(defn- get-potential
  [get-distance points point]
  (->> (reduce
         (fn [memo p]
           (+ memo
              (->>
                (get-distance (:value point) (:value p))
                (exponent alpha))))
         0 points)
       (assoc point :potential)))


(defn- get-potentials
  [get-distance points]
  (map (partial get-potential get-distance points) points))


(defn- calculate-point-potential
  [get-distance center point]
  (->> (get-distance (:value point) (:value center))
       (exponent betta)
       (* (:potential center))
       (- (:potential point))
       (assoc point :potential)))


(defn- calculate-points-potentials
  [get-distance center points]
  (->> (map (partial calculate-point-potential get-distance center) points)
       (sort-by :potential >)))


(defn- find-max-potential-point
  [points]
  (->> (sort-by :potential points)
       (last)))


(defn- find-shortest-distance
  [get-distance center centers]
  (reduce
    (fn [memo c]
      (min memo
           (get-distance (:value center)
                               (:value c))))
    Double/POSITIVE_INFINITY centers))


(defn- clusterize
  ([get-distance points]
    (let [potentials (->> (get-potentials get-distance points)
                          (sort-by :potential >))
          first-center (first potentials)]
      (clusterize get-distance
                  potentials
                  first-center
                  [first-center])))
  ([get-distance points first-center centers]
    (let [revised-points (calculate-points-potentials get-distance ((comp first reverse) centers) points)
          next-center (first revised-points)
          first-potential (:potential first-center)
          next-potential (:potential next-center)]
      (if (> next-potential (* first-potential e_max))
        (recur get-distance
               revised-points
               first-center
               (conj centers next-center))
        (if-not (< next-potential (* first-potential e_min))
          (let [shortest-distance (find-shortest-distance get-distance next-center centers)]
            (if (<= 1 (+ (/ shortest-distance r-a) (/ next-potential first-potential)))
              (recur get-distance
                     revised-points
                     first-center
                     (conj centers next-center))
              (let [revised-points (rest revised-points)]
                (recur get-distance
                       (conj revised-points (assoc next-center :potential 0))
                       first-center
                       (conj centers (find-max-potential-point revised-points))))))
          centers)))))


(defn- get-euclidean-distance
  [p1 p2]
  (let [i (atom -1)]
    (->>
      (reduce (fn [memo val]
                (do
                  (swap! i inc)
                  (+ memo (sqr (- val (get p2 @i))))))
              0 p1)
      (Math/sqrt))))


(defn- get-hamming-distance
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
    "euclidean" get-euclidean-distance
    "hamming" get-hamming-distance))


(defn- parse-string
  [str]
  (->> (str/split str #",")
       (butlast)
       (reduce (fn [memo val]
                 (conj memo (Double/parseDouble val)))
               [])))


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
            get-distance (get-distance-function distance-calculation-type)
            centers (clusterize get-distance points)]
        (println "Centers finded: " (count centers))
        (->> centers
             (map #(printf "line: %3d, potential: %.4f, data: %s \n" (:index %) (:potential %) (:value %)))
             (dorun))))
    (println "Wrong arguments")))