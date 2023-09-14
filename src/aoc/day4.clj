(ns aoc.day4
  (:require [clojure.string :as str]))

(defn parse-int [s] (Integer/parseInt s))

(def input-pairs
  (->> (slurp "resource/input/day4.txt")
       str/split-lines
       (map (fn [s]
              (->> (str/split s #",")
                   (map (fn [sections]
                          (map parse-int (str/split sections #"-")))))))))

(defn fully-contains? [[a b] [c d]]
  (and (>= a c) (<= b d)))

(defn overlap? [[a b] [c d]]
  (and (<= c b) (<= a d)))

(comment "Part 1"
         (->> input-pairs
              (filter (fn [[v1 v2]]
                        (or (fully-contains? v1 v2)
                            (fully-contains? v2 v1))))
              count))

(comment "Part 2"
         (->> input-pairs
              (filter #(apply overlap? %))
              count))
