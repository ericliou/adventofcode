(ns aoc.day13
  (:require [clojure.string :as str]))

(defn rules [left right]
  (cond
    (and (integer? left) (integer? right))
    (condp apply [left right]
      = :continue
      < true
      > false)

    (integer? left)
    (recur [left] right)

    (integer? right)
    (recur left [right])

    (and (empty? left) (empty? right)) :continue
    (empty? left) true
    (empty? right) false

    :else
    (let [r (rules (first left) (first right))]
      (if (= r :continue)
        (recur (rest left) (rest right))
        r))))

(defn process-input [input]
  (->> input
       str/split-lines
       (remove empty?)
       (map read-string)))

(def input (slurp "resource/input/day13.txt"))

;; Part 1
(let [signalsv (->> input
                    process-input
                    (partition 2))]
  (->> (map-indexed (fn [i signal]
                      (if (apply rules signal) (inc i) 0))
                    signalsv)
       (reduce +)))

;; Part 2
(let [div1 [[2]]
      div2 [[6]]
      signalsv (conj (process-input input) div1 div2)
      sorted (sort rules signalsv)]
  (* (inc (.indexOf sorted div1))
     (inc (.indexOf sorted div2))))
