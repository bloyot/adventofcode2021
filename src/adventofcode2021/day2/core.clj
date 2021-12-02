(ns adventofcode2021.day2.core
  (:require [clojure.string :as str]))

(defn load-input []
  (let [lines (str/split (slurp "resources/day2.txt") #"\n")
        direction-pairs (map #(str/split % #" ") lines)]
    (map (fn [[dir amount]] [dir (Integer/parseInt amount)]) direction-pairs)))

(defn final-position
  "From a set of direction pairs (i.e [forward 5] compute the product of the final horizontal
  position and depth."
  [directions]
  ;; order doesn't seem to matter so just group by direction and sum/multiply
  (let [grouped (group-by (comp keyword first) directions)
        sum-group (fn [k] (apply + (map second (get grouped k))))]
    (* (sum-group :forward) (- (sum-group :down) (sum-group :up)))))

(defn part1 []
  (final-position (load-input)))

(defn final-position-aim
  "From a set of direction pairs computes the product of the final horizontal position and
  depth, accounting for aim"
  [directions]
  (loop [curr-directions directions
         aim 0
         horizontal 0
         depth 0]
    (if (empty? curr-directions)
      (* horizontal depth)
      (let [[direction amount] (first curr-directions)]
        (case direction
          "forward" (recur (rest curr-directions) aim (+ horizontal amount) (+ depth (* aim amount)))
          "down" (recur (rest curr-directions) (+ aim amount) horizontal depth)
          "up" (recur (rest curr-directions) (- aim amount) horizontal depth))))))

(defn part2 []
  (final-position-aim (load-input)))
