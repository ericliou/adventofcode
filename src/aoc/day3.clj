(ns aoc.day3
  (:require [clojure.string :as s]
            [clojure.set]))

(def priority
  (zipmap (concat (map char (range (int \a) (inc (int \z))))
                  (map char (range (int \A) (inc (int \Z)))))
          (range 1 53)))

(defn intersection [coll]
  (->> (map set coll)
       (apply clojure.set/intersection)
       first))

(defn split-intersect [s]
  (intersection (split-at (/ (count s) 2) s)))

(comment
  ;; Part 1
  (->> (slurp "resource/input/day3.txt")
       (s/split-lines)
       (transduce (map (comp priority split-intersect)) +)))

(comment
  ;; Part 2
  (->> (slurp "resource/input/day3.txt")
       (s/split-lines)
       (partition 3)
       (transduce (map (comp priority intersection)) +)))
