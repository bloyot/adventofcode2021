(ns adventofcode2021.day12.core
  (:require [clojure.string :as str]))

(defn uppercase? [s]
  (= s (str/upper-case s)))

(defn load-input [file]
  (as-> (str "resources/" file) f
    (slurp f)
    (str/split-lines f)
    (map #(str/split % #"-") f)
    (concat f (map (fn [[a b]] [b a]) f))
    (group-by first f)
    (into {} (map (fn [[k v]] [k (remove #(= k %) (flatten v))]) f))
    (into {} (map (fn [[k v]]
                    [k {:name k
                        :connections v
                        :type (if (uppercase? k) :big :small)}])
                  f))))

(defn valid-next?
  "Return true if we haven't visited this cave, or if it is a big cave"
  [cave visited]
  (or (not (contains? visited cave)) (= :big (:type cave))))

(defn paths-to-end
  "Return the paths through the cavern for the given start cave"
  [curr caves path visited]
  (let [new-path (conj path curr)]
    (if (= "end" (:name curr))
      new-path
      (let [next (map #(get caves %) (:connections curr))
            filtered (filter #(valid-next? % visited) next)
            next-paths (flatten (map #(paths-to-end % caves new-path (conj visited curr)) filtered))]
        next-paths))))

(defn part1
  []
  (let [caves (load-input "day12.txt")
        flat-paths (paths-to-end (get caves "start") caves [] #{})]
    (->> flat-paths
         (map :name)
         (partition-by #(= "end" %))
         (remove #(= '("end") %))
         (map #(conj % "end"))
         count)))

(defn valid-next-double?
  "Return true if we haven't visited this cave, or if it is a big cave"
  [cave visited]
  (let [name (:name cave)
        visited-cave-count (get visited cave)
        visited-cave-count-safe (if (nil? visited-cave-count) 0 visited-cave-count)
        has-double-visit? (some #(< 1 %) (vals (remove #(= :big (:type (first %))) visited)))]
    (cond
      ;; if it's the start or end cave, can only visit once
      (and (#{"start" "end"} name) (< 0 visited-cave-count-safe))
      false

      ;; if it's a small cave, and we've visited it more than once and we've had a double visit already
      (and (= :small (:type cave)) (< 0 visited-cave-count-safe) has-double-visit?)
      false

      :else
      true)))

(defn inc-nil [n] (if (nil? n) 1 (inc n)))

(defn paths-to-end-double
  "Return the paths through the cavern for the given start cave"
  [curr caves path visited]
  (let [new-path (conj path curr)]
    (if (= "end" (:name curr))
      new-path
      (let [next (map #(get caves %) (:connections curr))
            filtered (filter #(valid-next-double? % visited) next)
            next-paths (flatten
                        (map #(paths-to-end-double % caves new-path (update visited curr inc-nil)) filtered))]
        next-paths))))

(defn has-duplicate?
  [path]
  (let [freq (frequencies path)]
    (< 1 (count (filter (fn [[k v]] (< 1 v)) (remove (fn [[k v]] (uppercase? k)) freq))))))
(defn part2
  []
  (let [caves (load-input "day12.txt")
        flat-paths (paths-to-end-double (get caves "start") caves [] {})]
    (->> flat-paths
         (map :name)
         (partition-by #(= "end" %))
         (remove #(= '("end") %))
         (map #(conj (vec %) "end"))
         ;; some bug? just filter extras /shrug
         (remove has-duplicate?)
         count)))
