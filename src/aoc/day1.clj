(ns aoc.day1
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2022/day/1

(comment
  ;; Part 1
  (-> (slurp "resource/input/day1.txt")
      (s/split #"\n\n")
      (->> (map s/split-lines)
           (map #(map (fn [s] (Integer/parseInt s)) %))
           (map #(apply + %))
           (apply max)))
  )

(comment
  ;; Part 2
  (-> (slurp "resource/input/day1.txt")
      (s/split #"\n\n")
      (->> (map s/split-lines)
           (map #(map (fn [s] (Integer/parseInt s)) %))
           (map #(apply + %))
           (sort)
           (take-last 3)
           (apply +)))
  )
