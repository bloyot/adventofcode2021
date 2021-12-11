(ns adventofcode2021.day10.core
  (:require [clojure.string :as str]))

(def open #{\{ \[ \( \<})
(def close #{\} \] \) \>})
(def close-pairs {\} \{
                  \] \[
                  \) \(
                  \> \<})
(def open-pairs {\{ \}
                  \[ \]
                  \( \)
                  \< \>})
(def syntax-point-values
  {\) 3
   \] 57
   \} 1197
   \> 25137})
(def auto-complete-point-values
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn load-input [file]
  (as-> (str "resources/" file) f
    (slurp f)
    (str/split-lines f)))

(defn line-status
  "Return a map of the status, including the first corrupt character
  if the line is corrupt"
  [line]
  ;; use a stack to push and pop openining characters as we go through the seq
  ;; when finding a close character, the most recent element of the stack should
  ;; be the corresponding open
  (loop [stack '()
         chars line]
    (if (empty? chars)
      (if (empty? stack) {:status :valid} {:status :incomplete})
      (let [curr-char (first chars)
            remaining-chars (rest chars)]
        (cond
          (open curr-char)
          (recur (conj stack curr-char) remaining-chars)

          (and (close curr-char) (= (get close-pairs curr-char) (first stack)))
          (recur (rest stack) remaining-chars)

          ;; otherwise this is a corrupt line, return immediately
          :else
          {:status :corrupt :first-illegal-char curr-char})))))

(defn syntax-points
  "Return the number of sytax points based on the first illegal
  character in any line."
  [lines]
  (->> lines
       (map line-status)
       (filter #(= (:status %) :corrupt))
       (map :first-illegal-char)
       (map #(get syntax-point-values %))
       (apply +)))

(defn part1 []
  (syntax-points (load-input "day10.txt")))

(defn complete-line
  "Given an incomplete line return the characters required to complete
  the chunks."
  [line]
  (loop [stack '()
         chars line]
    (if (empty? chars)
      (map #(get open-pairs %) stack)
      (let [curr-char (first chars)
            remaining-chars (rest chars)]
        (if (open curr-char)
          (recur (conj stack curr-char) remaining-chars)
          (recur (rest stack) remaining-chars))))))

(defn auto-complete-line-value
  "Return the value of a given autocompletion"
  [line]
  (reduce (fn [score itm]
            (+ (* score 5) (get auto-complete-point-values itm)))
          0 line))

(defn auto-complete-points
  "Return the amount of points for the autocompletions of imcomplete
  lines."
  [lines]
  (let [point-vals (->> lines
                        (filter #(= (:status (line-status %) %) :incomplete))
                        (map complete-line)
                        (map auto-complete-line-value)
                        sort)]
    (nth point-vals (quot (count point-vals) 2))))

(defn part2 []
  (auto-complete-points (load-input "day10.txt")))


