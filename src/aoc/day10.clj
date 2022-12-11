(ns aoc.day10
  (:require [clojure.string :as s]))

(defn parse-long-if [x] (some-> x parse-long))

(def input
  (->> (slurp "resource/input/day10.txt")
       s/split-lines
       (map #(s/split % #" "))
       (map #(update % 1 parse-long-if))))

(defn op-cycles [[op v]]
  (if (= op "noop") [0] [0 v]))

(defn v-during-cycles [input]
  (reductions + 1 (mapcat op-cycles input)))

(comment
  ; Part 1
  (let [signal-strength (->> (v-during-cycles input)
                             (map * (drop 1 (range))))]
    (->> (range 20 (inc 220) 40)
         (map #(nth signal-strength (dec %)))
         (reduce +))))

(comment
  ;; Part 2
  (let [v-during (v-during-cycles input)
        pixel-in-position (fn [p]
                            (if (<= (abs (- (nth v-during p) (mod p 40))) 1)
                              "#" "."))]
    (->> (range 240)
         (map pixel-in-position)
         (partition 40)
         (map s/join)
         (map println))))
