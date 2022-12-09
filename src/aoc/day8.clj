(ns aoc.day8
  (:require [clojure.string :as s]))

(def board
  (->> (slurp "resource/input/day8.txt")
       s/split-lines
       (map #(map (comp parse-long str) %))))

(defn transpose* [matrix]
  (apply map vector matrix))

(def transpose (memoize transpose*))

(defn left-view [board i j]
  (reverse (take j (nth board i))))

(defn right-view [board i j]
  (drop (inc j) (nth board i)))

(defn bottom-view [board i j]
  (right-view (transpose board) j i))

(defn top-view [board i j]
  (left-view (transpose board) j i))

(defn visible? [line-sight self]
  (every? (partial > self) line-sight))

(defn externally-visible? [board i j]
  (transduce (map #(visible? % (nth (nth board i) j)))
             (completing #(or %1 %2))
             false
             [(left-view board i j)
              (right-view board i j)
              (bottom-view board i j)
              (top-view board i j)]))

(defn scenic-score-line [line-sight self]
  ;; stop if you reach an edge or
  ;; at the first tree that is the same height or taller than the tree under consideration
  (cond
    (empty? line-sight) 0
    (< (first line-sight) self) (inc (scenic-score-line (rest line-sight) self))
    ;; too tall, stop
    :else 1))

(defn scenic-score [board i j]
  (transduce (map #(scenic-score-line % (nth (nth board i) j)))
             *
             [(left-view board i j)
              (right-view board i j)
              (bottom-view board i j)
              (top-view board i j)]))

(comment
  ;; Part 1
  (count (filter identity
           (for [i (range (count board))
                 j (range (count (first board)))]
             (externally-visible? board i j)))))

(comment
  ;; Part 2
  (apply max
         (for [i (range (count board))
               j (range (count (first board)))]
           (scenic-score board i j))))

