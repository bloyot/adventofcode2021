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
  (or (not (contains? @visited cave)) (= :big (:type cave))))

(defn paths
  "Return the paths through the cavern for the given start cave"
  [start caves number-paths visited]
  (swap! visited conj start)
  (if (= "end" (:name start))
    (+ number-paths 1)
    (let [next (map #(get caves %) (:connections start))
          filtered (filter #(valid-next? % visited) next)]
      (apply + (map #(paths % caves (+ number-paths 1) visited) filtered)))))

(defn part1
  []
  (let [caves (load-input "day12example.txt")]
    (paths (get caves "start") caves 0 (atom #{}))))
