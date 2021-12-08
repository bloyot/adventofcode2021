(ns adventofcode2021.day8.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn load-input [file]
  (as-> (str "resources/" file) f
    (slurp f)
    (str/split-lines f)
    (map #(str/split % #"\|") f)
    (map (fn [[input output]] {:input (str/split (str/trim input) #" ")
                               :output (str/split (str/trim output) #" ")}) f)))

(defn map-unique-digit
  "Given an input/output signal consisting of the characters a-g return the digit if it's unique, else
  nil"
  [signal]
  (let [num-segments (count signal)]
    (case num-segments
      2 1
      3 7
      4 4
      7 8
      nil)))

(defn find-unique-output-digits
  [signals]
  (->> signals
       (map :output)
       flatten
       (map map-unique-digit)
       (filter (comp not nil?))
       count))

(defn part1 []
  (find-unique-output-digits (load-input "day8.txt")))

(defn overlap?
  "Determine if the digits in a overlap with those in b"
  [a b]
  (set/subset? (set a) (set b)))

(defn signal-map
  "Convert the input signals to a map of sorted string to number "
  [signal-input]
  ;; from looking at the input, we know there is only 1 of each of 1 4 7 8, so we
  ;; can find those and use that to make some deductions
  (let [sorted (sort-by count signal-input)
        five-digits (filter #(= 5 (count %)) signal-input)
        six-digits (filter #(= 6 (count %)) signal-input)
        one (first sorted)
        three (first (filter #(overlap? one %) five-digits))
        four (nth sorted 2)
        seven (second sorted)
        eight (last sorted)
        nine (first (filter #(overlap? four %) six-digits))
        zero (first (filter #(overlap? one %) (remove #(= nine %) six-digits)))
        six (first (remove #(overlap? one %) (remove #(= nine %) six-digits)))
        five (first (remove
                     #(contains? (set six) (first (set/difference (set one) (set %))))
                     (remove #(= three %) five-digits)))
        two (first (filter
                    #(contains? (set six) (first (set/difference (set one) (set %))))
                    (remove #(= three %) five-digits)))]

    (into {} (map-indexed (fn [idx itm] [(apply str (sort itm)) idx])
                          [zero one two three four five six seven eight nine]))))

(defn decode-signal
  "Find the integer value of the output based on the signals of the input"
  [{:keys [input output]}]
  (let [mapping (signal-map input)]
    (->> output
         (map sort)
         (map #(apply str %))
         (map #(get mapping %))
         (apply str)
         Integer/parseInt)))

(defn signals-output
  "Decode all signals and sum their output values"
  [signals]
  (apply + (map decode-signal signals)))

(defn part2 []
  (signals-output (load-input "day8.txt")))
