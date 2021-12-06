(ns adventofcode2021.day5.core
  (:require [clojure.string :as str]))

(defn load-input [file]
  (->> (slurp (str "resources/" file))
       str/split-lines
       (map #(str/split % #" -> "))
       (map (fn [[p1 p2]] (concat (str/split p1 #",") (str/split p2 #","))))
       (map (fn [coll] (map #(Integer/parseInt %) coll)))
       (map (fn [[x1 y1 x2 y2]] {:x1 x1 :y1 y1 :x2 x2 :y2 y2}))))

(defn non-diagonal? [{:keys [x1 y1 x2 y2]}]
  (or (= x1 x2) (= y1 y2)))

(defn fill-lines
  [x-range y-range y-fill]
  (if (not (seq x-range))
    (repeat (count y-range) y-fill)
    x-range))

(defn range-mod [p1 p2]
  (cond
    (< p1 p2) 1
    (> p1 p2) -1
    (= p1 p2) 0))

(defn points-to-line
  "Given the start and end points, produce a sequence of all points on
  the line."
  [{:keys [x1 y1 x2 y2]}]
  (let [x-mod (range-mod x1 x2)
        y-mod (range-mod y1 y2)
        x (range x1 (+ x2 x-mod) (if (= 0 x-mod) 1 x-mod))
        y (range y1 (+ y2 y-mod) (if (= 0 y-mod) 1 y-mod))
        x-filled (if (< (count x) (count y)) (fill-lines x y x1) x)
        y-filled (if (< (count y) (count x)) (fill-lines y x y1) y)]
    (->> [x-filled y-filled]
         (apply interleave)
         (partition 2 2)
         (map (fn [[x1 y1]] {:x1 x1 :y1 y1})))))

(defn count-overlapping-line-points
  "Return the count of the number of points where more than 1 line exists"
  [points include-diagonal?]
  (->> points
      (filter (if include-diagonal? identity non-diagonal?))
      (map points-to-line)
      (apply concat)
      frequencies
      (filter #(< 1 (second %)))
      count))

(defn part1 []
  (count-overlapping-line-points (load-input "day5.txt") false))

(defn part2 []
  (count-overlapping-line-points (load-input "day5.txt") true))
