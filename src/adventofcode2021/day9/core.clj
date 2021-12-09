(ns adventofcode2021.day9.core
  (:require [clojure.string :as str]))

(defn load-input [file]
  (as-> (str "resources/" file) f
    (slurp f)
    (str/split-lines f)
    (map (fn [row] (map #(Integer/parseInt (str %)) row)) f)))

(defn height
  "Return the height from the map given a specific row i and column j"
  [hmap i j]
  ;; check boundaries
  (when (and
         (< i (count hmap)) (<= 0 i)
         (< j (count (first hmap))) (<= 0 j))
    (nth (nth hmap i) j)))

(defn adjacent-points [i j]
  [[(- i 1) j] [(+ i 1) j] [i (- j 1)] [i (+ j 1)]])

(defn low-point?
  "For a particular point i,j in the height map, return true if this is a low point,
  meaning all adjacent (up down left right) points are higher"
  [hmap i j]
  (let [adj-points (adjacent-points i j)
        center (height hmap i j)
        adj-heights (remove nil? (map (fn [[i j]] (height hmap i j)) adj-points))]
    (every? #(< center %)
            adj-heights)))

(defn risk-sum
  "Sum the risk levels of all low points for the given height map"
  [hmap]
  (let [heights (for [x (range 0 (count hmap))
                      y (range 0 (count (first hmap)))
                      :let [h (height hmap x y)]
                      :when (low-point? hmap x y)]
                  h)]
    (apply + (map #(+ 1 %) heights))))

(defn part1 []
  (risk-sum (load-input "day9.txt")))

;; part 2 is basically a poor mans version of the connected components algorithm

(def point-labels (atom {}))
(defn map-basin
  "For a given starting point i and j and a basin label, recursively traverse the graph
  until you reach no more valid points (> 9) and label those points"
  [hmap i j label]
  (when-let [h (height hmap i j)]
    (when (and (< h 9) (not (contains? @point-labels [i j])))
      (let [points (remove (fn [[x y]] (nil? (height hmap x y))) (adjacent-points i j))
            ;; figure this out
            updated-labels (swap! point-labels assoc [i j] label)]
        ;; label and continue exploring
        (apply merge updated-labels (map (fn [[x y]] (map-basin hmap x y label)) points))))))

(defn map-basins
  "Return a map of all points labeled with their basins"
  [hmap]
  (let [points (for [x (range 0 (count hmap))
                     y (range 0 (count (first hmap)))]
                 [x y])]
    (loop [points points
           label 0]
      (if (not (seq points))
        ;; when we are out of points return the labels
        @point-labels
        ;; otherwise label and recur
        (let [[i j] (first points)]
          (if (and (not= 9 (height hmap i j)) (not (get @point-labels [i j])))
            ;; if the point isn't already labeled, run map basin and recur with updated values
            (do
              (map-basin hmap i j label)  
              (recur (rest points) (inc label) ))
          ;; otherwise just move on with the same values
            (recur (rest points) label)))))))

(defn largest-basin-sizes
  [hmap]
  (->> hmap
       map-basins
       (group-by second)
       vals
       (map count)
       sort
       reverse
       (take 3)
       (apply *)))

(defn part2 []
  (largest-basin-sizes (load-input "day9.txt")))

