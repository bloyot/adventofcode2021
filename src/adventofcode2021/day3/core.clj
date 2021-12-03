(ns adventofcode2021.day3.core
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]))

(defn load-input []
  (str/split (slurp "resources/day3.txt") #"\n"))

(defn binary-string-to-int
  "Convert a binary number string to its integer value"
  [binary-string]
  (let [reversed-ints (reverse (map #(Integer/parseInt (str %)) binary-string))]
    (->> reversed-ints
         (map-indexed (fn [idx itm] (* itm (Math/pow 2 idx))))
         (reduce +)
         int)))

(defn bit-frequencies
  "Return a sequence of the frequencies of each bit position"
  [diagnostics]
  ;; we basically want to just turn this into a matrix, transpose it, and then count determine the most
  ;; common element in each row
  (->> diagnostics
       (map vec)
       vec
       matrix/transpose
       (map frequencies)))

(defn epsilon-rate
  "Compute the epsilon rate, which is the inverse of the gamma rate"
  [gamma-rate]
  (apply str (map #(if (= \1 %) "0" "1") gamma-rate)))

(defn gamma-rate
  "Compute the gamma rate from the list of diagnostic binary numbers (as strings). The
  gamma rate is a binary number string where each bit is the most common bit in the corresponding
  position for the diagnostics"
  [diagnostics]
  ;; get the frequencies, sort them, and take the highest, convert back to string
  (->> (bit-frequencies diagnostics)
       (map (comp first reverse (partial sort-by second)))
       (map first)
       (apply str)))

(defn power-consumption
  "From the given list of diagnostic binary numbers (as strings), compute the power consumption
  as the product of the gamma rate and epsilon rate"
  [diagnostics]
  (let [gamma (gamma-rate diagnostics)
        epsilon (epsilon-rate gamma)]
    (* (binary-string-to-int gamma) (binary-string-to-int epsilon))))

(defn part1 []
  (power-consumption (load-input)))

(defn find-active-bit [bit-freq most-freq? bit]
  (let [counts (nth bit-freq bit)
        zero (get counts \0)
        one (get counts \1)]
    (cond
      (< zero one) (if most-freq? \1 \0)
      (< one zero) (if most-freq? \0 \1)
      (= zero one) (if most-freq? \1 \0))))

(defn rating
  [diagnostics most-freq?]
  (let [diag-matrix (vec (map vec diagnostics))]
    (loop [candidates diag-matrix
           bit-freq (bit-frequencies diagnostics)
           bit 0]
      (if (= 1 (count candidates))
        (apply str (first candidates))
        (let [active-bit (find-active-bit bit-freq most-freq? bit)
              new-candidates (filter #(= active-bit (nth % bit)) candidates)
              new-candidates-bit-freq (map frequencies (matrix/transpose new-candidates))]
          (recur new-candidates new-candidates-bit-freq (inc bit)))))))

(defn oxygen-rating
  "Filter diagnostics based on the most common bit in each column, until only one is remaining"
  [diagnostics]
  (rating diagnostics true))

(defn co2-rating
  "Filter diagnostics based on the least common bit in each column, until only one is remaining"
  [diagnostics]
  (rating diagnostics false))

(defn life-support-rating
  "Compute the life support rating as the product of the oxygen and co2 ratings"
  [diagnostics]
  (let [oxygen (oxygen-rating diagnostics)
        co2 (co2-rating diagnostics)]
    (* (binary-string-to-int oxygen) (binary-string-to-int co2))))

(defn part2 []
  (life-support-rating (load-input)))


