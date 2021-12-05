(ns adventofcode2021.day4.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn coll-string-to-int
  [coll]
  (map (fn [l] (Integer/parseInt l)) coll))

(defn load-input
  "Returns a map with :numbers and :boards"
  [file]
  (let [lines (->> file
                   (str "resources/")
                   slurp
                   str/split-lines
                   (filter seq))
        numbers (as-> (first lines) line
                  (str/split line #",")
                  (map (fn [n] (Integer/parseInt n)) line))
        boards (->> (rest lines)
                    (map #(str/split % #" "))
                    (map #(filter seq %))
                    (map coll-string-to-int)
                    (partition 5 5))]
    {:numbers numbers
     :boards boards}))

(defn row
  "Get the ith row from the board"
  [board i]
  (for [x (range 5)]
    (nth (nth board x) i)))

(defn col
  "Get the ith col from the board"
  [board i]
  (nth board i))

(defn check-numbers
  "For a given collection of numbers, check if they have been drawn yet"
  [board-numbers drawn-numbers]
  (set/subset? (set board-numbers) (set drawn-numbers)))

(defn board-winning?
  "Determine if this board wins based on the numbers that have been
  drawn so far. Returns the board if it is winning, else nil"
  [board drawn-numbers]
  (when
   (some identity
         (for [x (range 5)]
           (or (check-numbers (row board x) drawn-numbers)
               (check-numbers (col board x) drawn-numbers))))
    board))

(defn compute-board-score
  "For the given board (5x5 array) and drawn numbers compute the score
  as the sum of all unmarked numbers, multiplied by the last number
  drawn."
  [board drawn-numbers]
  (as-> board b
    (flatten b)
    (set b)
    (set/difference b (set drawn-numbers))
    (apply + b)
    (* b (last drawn-numbers))))

(defn bingo
  "Determine which board is the winner based on the given numbers, and
  then compute that board score"
  [boards numbers]
  (loop [boards boards
         drawn [(first numbers)]
         remaining (rest numbers)]
    (let [winning-board (map #(board-winning? % drawn) boards)]
      (if (some identity winning-board)
        (compute-board-score (remove nil? winning-board) drawn)
        (recur boards (concat drawn [(first remaining)]) (rest remaining))))))

(defn part1 []
  (let [input (load-input "day4.txt")]
    (bingo (:boards input) (:numbers input))))

(defn last-bingo
  "Same criteria but the last board to win"
  [boards numbers]
  (loop [winning-boards []
         remaining-boards boards
         drawn [(first numbers)]
         remaining (rest numbers)]
    (let [new-winners (filter #(board-winning? % drawn) remaining-boards)
          new-remaining (filter #(not (board-winning? % drawn)) remaining-boards)]
      (if (empty? new-remaining)
        (compute-board-score (first new-winners) drawn)
        (recur
         (concat winning-boards new-winners)
         new-remaining
         (concat drawn [(first remaining)])
         (rest remaining))))))

(defn part2 []
  (let [input (load-input "day4.txt")]
    (last-bingo (:boards input) (:numbers input))))

