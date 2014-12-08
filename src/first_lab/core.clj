(ns barashkov.first-lab.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- sqr [x]
  (* x x))

; define algorithm constants
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


; rewrite
(defn- get-potential
  [get-distance points point]
  (->> (reduce
         (fn [memo p]
           (+ memo (->> (get-distance (:value point)
                                      (:value p))
                        (exponent alpha))))
         0 points)
       (assoc point :potential)))

;rewrite
(defn- get-potentials
  [distance-func points]
  (map (partial get-potential distance-func points) points))


;rewrite
(defn- revise-point-potential
  [distance-func center point]
  (->> (distance-func (:value point) (:value center))
       (exponent betta)
       (* (:potential center))
       (- (:potential point))
       (assoc point :potential)))

;rewrite
(defn- revise-points-potentials
  [distance-func center points]
  (->> (map (partial revise-point-potential distance-func center) points)
       (sort-by :potential >)))

(defn- find-max-potential-point
  [points]
  (->> (sort-by :potential > points)
       (first)))

(defn- find-shortest-distance
  [distance-func center centers]
  (reduce
    (fn [memo another-center]
      (min memo (distance-func (:value center)
                               (:value another-center))))
    Double/POSITIVE_INFINITY centers))

; entry point of Cluster Estimation algorithm
(defn- clusterize
  ([distance-func points]                                   ; init recursion call
    (let [potentials (->> (get-potentials distance-func points)
                          (sort-by :potential >))
          first-center (first potentials)]
      (clusterize distance-func
                  potentials
                  first-center
                  [first-center])))
  ([distance-func points first-center centers]
    (let [revised-points (revise-points-potentials distance-func ((comp first reverse) centers) points)
          next-center (first revised-points)
          first-potential (:potential first-center)
          next-potential (:potential next-center)]
      (if (> next-potential (* first-potential e_max))
        (recur distance-func
               revised-points
               first-center
               (conj centers next-center))
        (if (< next-potential (* first-potential e_min))
          centers
          (let [shortest-distance (find-shortest-distance distance-func next-center centers)]
            (if (<= 1 (+ (/ shortest-distance r-a) (/ next-potential first-potential)))
              (recur distance-func
                     revised-points
                     first-center
                     (conj centers next-center))
              (let [revised-points (rest revised-points)]
                (recur distance-func
                       (conj revised-points (assoc next-center :potential 0))
                       first-center
                       (conj centers (find-max-potential-point revised-points)))))))))))


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
            get-distance (get-distance-function distance-calculation-type)
            centers (clusterize get-distance points)]
        (println "Centers finded: " (count centers))
        (->> centers
             (map #(printf "line: %3d, potential: %.4f, data: %s \n" (:index %) (:potential %) (:value %)))
             (dorun))))
  (println "Wrong arguments")))