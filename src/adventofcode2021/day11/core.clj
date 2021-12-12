(ns adventofcode2021.day11.core
  (:require [clojure.set :as sets]
            [clojure.string :as str]))

(defn load-input
  [file]
  (as-> (str "resources/" file) f
    (slurp f)
    (str/split-lines f)
    (map (fn [row] (map #(Integer/parseInt (str %)) row)) f)
    (map vec f)
    (vec f)))

(defn grid-val
  "return the value at i j in the grid"
  [grid i j]
  (nth (nth grid i) j))

(defn flashes
  "Return the indices of elements greater than nine"
  [grid]
  (into #{} (for [y (range 0 (count grid))
                  x (range 0 (count (first grid)))
                  :when (< 9 (grid-val grid x y))]
              [x y])))

(defn inc-grid
  "Increment each grid value by 1"
  [grid]
  (let [size-y (count grid)
        size-x (count (first grid))]
    (vec (map vec (partition size-y size-x (map inc (flatten grid)))))))

(defn clear-flashes
  "Transform all >= 10 to 0"
  [grid]
  (let [size-y (count grid)
        size-x (count (first grid))]
    (vec (map vec (partition size-y size-x (map #(if (<= 10 %) 0 %) (flatten grid)))))))

(defn inc-grid-val
  "increment the grid location at i j by 1"
  [grid i j]
  (update grid i #(update % j inc)))

(defn neighbors
  "Return all neighbor pairs for i and j including diagonals"
  [grid i j]
  (for [x (range (- j 1) (+ j 2))
        y (range (- i 1) (+ i 2))
        :when (and (<= 0 x) (< x (count (first grid)))
                   (<= 0 y) (< y (count grid))
                   (or (not= j x) (not= i y)))]
    [y x]))

(defn update-flash-neighbor
  [grid i j]
  (let [neighbor-points (neighbors grid i j)]
    (reduce (fn [curr-grid [i j]] (inc-grid-val curr-grid i j))
            grid
            neighbor-points)))

(defn update-flash-neighbors
  "Return a new grid where the energy of flash neighbors has been increased"
  [grid flashes]
  (reduce (fn [curr-grid [i j]] (update-flash-neighbor curr-grid i j)) grid flashes))

(defn step
  "Apply a single step to the octopus grid, returning a map of the new grid
  and the number of flashes triggered."
  [grid]
  (let [new-grid (inc-grid grid)]
    (loop [curr-grid new-grid
           curr-flashes #{}]
      (let [new-flashes (sets/difference (flashes curr-grid) curr-flashes)
            all-flashes (sets/union curr-flashes new-flashes)]
      ;; if there are new flashes after incrementing, keep going
        (if (seq new-flashes)
          (recur (update-flash-neighbors curr-grid new-flashes) all-flashes)
          {:grid (clear-flashes curr-grid) :flashes all-flashes})))))

(defn count-flashes
  "Count the number of flashes for the grid after a given number of
  steps."
  [grid num-steps]
  (reduce (fn [{:keys [curr-grid num-flashes]} _]
            (let [step-result (step curr-grid)]
              {:curr-grid (:grid step-result)
               :num-flashes (+ num-flashes (count (:flashes step-result)))}))
          {:curr-grid grid
           :num-flashes 0}
          (range 0 num-steps)))

(defn part1 []
  (:num-flashes (count-flashes (load-input "day11.txt") 100)))

(defn all-zeros?
  [grid]
  (every? #(= 0 %) (flatten grid)))

(defn synchronized-flashes
  "Return the first step where every octopus flashes"
  [grid]
  (loop [curr-grid grid
         i 0]
    (if (all-zeros? curr-grid)
      i
      (recur (:grid (step curr-grid)) (inc i)))))

(defn part2 []
  (synchronized-flashes (load-input "day11.txt")))


