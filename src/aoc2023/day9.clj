(ns aoc2023.day9
  (:require [clojure.string :as string]))

(def input (slurp "resource/input/2023/day9.txt"))
(defn parse [s] (->> (string/split-lines s)
                     (mapv #(re-seq #"-?\d+" %))
                     (mapv (partial mapv parse-long))))

(defn extrapolate [v]
  (if (every? zero? v)
    0
    (+ (last v) (extrapolate (map - (rest v) v)))))

(comment
  (let [lines (parse input)]
    (reduce + (map extrapolate lines))))

;; Part 2
(comment
  (let [lines (parse input)]
    (transduce (comp (map reverse)
                     (map extrapolate)) + lines)))
