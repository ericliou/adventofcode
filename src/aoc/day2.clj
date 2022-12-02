(ns aoc.day2
  (:require [clojure.string :as s]))

;; https://adventofcode.com/2022/day/2

(def xyz->rock-paper-scissors
  {\X :rock
   \Y :paper
   \Z :scissors})

(def abc->rock-paper-scissors
  {\A :rock
   \B :paper
   \C :scissors})

(def rules
  ;; [opponent myself result]
  #{[:rock :rock :draw]
    [:scissors :scissors :draw]
    [:paper :paper :draw]
    [:rock :scissors :lose]
    [:scissors :paper :lose]
    [:paper :rock :lose]
    [:rock :paper :win]
    [:scissors :rock :win]
    [:paper :scissors :win]})

(def shape-score
  {:rock 1
   :paper 2
   :scissors 3})

(defn outcome [opponent myself]
  (last (first
         (filter #(and (= (first %) opponent)
                       (= (second %) myself)) rules))))

(defn outcome-score [opponent myself]
  (case (outcome opponent myself)
    :draw 3
    :win 6
    :lose 0))

(defn score [opponent myself]
  (+ (shape-score myself)
     (outcome-score opponent myself)))

(comment
  ;; Part 1
  (->> (slurp "resource/input/day2.txt")
       (s/split-lines)
       (transduce (comp
                   (map #(score (abc->rock-paper-scissors (first %))
                                (xyz->rock-paper-scissors (last %)))))
                  +)))

(def xyz->outcome
  {\X :lose
   \Y :draw
   \Z :win})

(defn shape-to-choose [opponent outcome]
  (second
   (first
    (filter #(and (= (first %) opponent)
                  (= (last %) outcome)) rules))))
(comment
  ;; Part 2
  (->> (slurp "resource/input/day2.txt")
       (s/split-lines)
       (transduce (comp
                   (map #(vector (abc->rock-paper-scissors (first %))
                                 (xyz->outcome (last %))))
                   (map #(score (first %) (apply shape-to-choose %))))
                  +)))
