(ns adventofcode2021.day6.core
  (:require [clojure.string :as str]))

(defn load-input [file]
  (as-> (str "resources/" file) f
    (slurp f)
    (str/trim f)
    (str/split f #",")
    (map #(Integer/parseInt %) f)))

(defn update-fish-freq
  "Given the frequencies of fish by timer, iterate the day and add new fish as approriate"
  [freq]
  (let [fish-by-day (replace {nil 0} (map #(get freq %) (range 9)))
        zeros (first fish-by-day)]
    (as-> fish-by-day f
      (vec f)
      (assoc f 7 (+ zeros (nth fish-by-day 7)))
      (assoc f 9 zeros)
      (rest f)
      (map-indexed (fn [idx itm] [idx itm]) f)
      (into {} f))))

(defn simulate-fish
  [fish num-days]
  (loop [freq (frequencies fish)
         day 0]
    (if (<= num-days day)
      (apply + (map second freq))
      (recur (update-fish-freq freq) (inc day)))))

(defn part1 []
  (simulate-fish (load-input "day6.txt") 80))

(defn part2 []
  (simulate-fish (load-input "day6.txt") 256))
