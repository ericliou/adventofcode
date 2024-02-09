(ns aoc2023.day11
  (:require
   [clojure.string :as string]))

(def input (slurp "resource/input/2023/day11.txt"))

(defn parse-coords [input]
  (let [lines (string/split-lines input)]
    (for [i (range (count lines))
          j (range (count (first lines)))
          :when (= \# (get (get lines i) j))]
      [i j])))

(defn combinations [coll]
  (loop [coll coll
         acc []]
    (if (seq coll)
      (recur (rest coll)
             (into acc (map #(vector (first coll) %) (rest coll))))
      acc)))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn expand [coords multiplier]
  (let [lines (string/split-lines input)
        empties (fn [lines] (keep-indexed #(when (every? (partial = \.) %2) %1) lines))
        transpose (partial apply map vector)
        empty-lines (empties lines)
        empty-cols (empties (transpose lines))]
    (map (fn [[x y]]
           [(+ x (* (dec multiplier) (count (filter #(< % x) empty-lines))))
            (+ y (* (dec multiplier) (count (filter #(< % y) empty-cols))))]) coords)))

(comment
  (let [coords (parse-coords input)
        expanded (expand coords 1000000)
        combs (combinations expanded)]
    (reduce + (map (partial apply distance) combs)))) ; 406725732046

