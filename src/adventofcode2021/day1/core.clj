(ns adventofcode.day1.core
  (:require [clojure.string :as str]))

(defn load-input []
  (map #(Integer/parseInt %) (str/split (slurp "resources/day1.txt") #"\n")))

;; part 1
(defn count-increases
  "Count the number of times the depth measurement increases from the previous measurement"
  [depths]
  (let [pairs (partition 2 1 depths)]
    (count (filter (fn [[a b]] (< a b)) pairs))))

(defn part1 []
  (count-increases (load-input)))

;; part 2

(defn count-window-increases
  "Count the number of times the 3 element sliding window increased over the previous window"
  [depths]
  (let [windows (partition 3 1 depths)
        window-sums (map (partial apply +) windows)]
    (count-increases window-sums)))

(defn part2 []
  (count-window-increases (load-input)))
