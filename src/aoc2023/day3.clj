(ns aoc2023.day3
  (:require [clojure.string :as string]))

(def input (slurp "resource/input/2023/day3.txt"))
(def mtx (mapv vec (string/split-lines input)))

(defn re-pos [pattern s]
  (let [matcher (re-matcher pattern s)]
    (loop [res []]
      (if (re-find matcher)
        (recur (conj res {:start (. matcher start)
                          :end (. matcher end)
                          :group (. matcher group)}))
        res))))

(defn numbers [i s]
  (->> (re-pos #"\d+" s)
       (mapv (fn [{:keys [start end group] :as m}]
               (merge m
                      {:pos (mapv vector (repeat i) (range start end))
                       :num (parse-long group)
                       :line i})))))

(def adjacent-units
  (for [i (range -1 2)
        j (range -1 2)]
    [i j]))

(defn out-of-bound? [[x y]]
  (or (neg? x) (neg? y)
      (>= x (count (first mtx)))
      (>= y (count mtx))))

(defn adjacents [positions]
  (set (for [p1 adjacent-units
             p2 positions
             :let [p (mapv + p1 p2)]
             :when (not (out-of-bound? p))] p)))

(def -symbol? (complement #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.}))

(defn adj-to-symbol? [num]
  (->> (adjacents (:pos num))
       (map #(get-in mtx %))
       (some -symbol?)))

;; Part 1
(comment
  (->> (string/split-lines input)
       (map-indexed numbers)
       flatten
       (filter adj-to-symbol?)
       (map :num)
       (reduce +)))

;; Part 2
(def lines (string/split-lines input))

(defn overlap?
  [x1 y1 x2 y2]
  (or (<= x1 x2 (dec y1))
      (<= x1 (dec y2) (dec y1))))

(defn adj-numbers [nums]
  (fn [{:keys [start end line] :as _gear}]
    (let [[ymin ymax] [(max 0 (dec line)) (min (count mtx) (inc line))]
          adj-line-nums (mapcat (vec nums) (range ymin (inc ymax)))]
      (filter (fn [{s :start e :end}]
                (overlap? (dec start) (inc end) s e))
              adj-line-nums))))

(defn gears [i s]
  (->> (re-pos #"\*" s)
       (map #(assoc % :line i))))

(comment
  (let [nums (map-indexed numbers lines)
        gears (flatten (map-indexed gears lines))]
    (transduce (comp (map (adj-numbers nums))
                     (filter #(= 2 (count %)))
                     (map (partial transduce (map :num) *)))
               +
               gears)))
