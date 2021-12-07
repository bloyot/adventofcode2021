(ns adventofcode2021.day7.core
  (:require [clojure.string :as str]))

(defn load-input [file]
  (as-> (str "resources/" file) f
    (slurp f)
    (str/split-lines f)
    (first f)
    (str/split f #",")
    (map #(Integer/parseInt %) f)))

(defn fuel-cost
  "Determine the fuel cost to align all crabs with the target position"
  [crabs target]
  (apply + (map #(Math/abs (- target %)) crabs)))

(defn variable-fuel-cost
  "Determine the fuel cost to align all crabs with the target position
  where each step costs 1 more than the previous"
  [crabs target]
  (->> crabs
       (map #(Math/abs (- target %)))
       (map #(apply + (range 1 (+ % 1))))
       (apply +)))

(defn optimal-fuel-cost
  "Determine the optimal fuel cost to align a list of crabs"
  [crabs]
  (let [median (nth (sort crabs) (quot (count crabs) 2))]
    (fuel-cost crabs median)))

(defn part1 []
  (optimal-fuel-cost (load-input "day7.txt")))

(defn optimal-variable-fuel-cost
  "Determine the optimal fuel cost to align a list of crabs given
  variable costs"
  [crabs]
  ;; just brute force it, it only takes about a minute to run
  (let [target-range (range (apply min crabs) (apply max crabs))
        costs (into {} (map (fn [pivot] [pivot (variable-fuel-cost crabs pivot)]) target-range))]
    (second (first (sort-by second costs)))))

(defn part2 []
  (optimal-variable-fuel-cost (load-input "day7.txt")))

