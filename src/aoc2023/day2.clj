(ns aoc2023.day2
  (:require [clojure.string :as string]))

(def input (slurp "resource/input/2023/day2.txt"))

(defn revelations [s]
  (let [revelations-str (second (string/split s #":"))]
    (->> (re-seq #"\d+|red|green|blue" revelations-str)
         (partition 2)
         (mapv #(-> % vec (update 0 parse-long)))
         (group-by second))))

(def test-max {"red" 12
               "green" 13
               "blue" 14})

(defn map-ge? [m1 m2]
  (every? #(>= (val %) (get m2 (key %))) m1))

(defn max-per-color [revelations]
  (reduce-kv (fn [m k v]
               (assoc m k (first (apply max-key first v)))) {}
             revelations))

;; Part 1
(comment
  (transduce (comp (map revelations)
                   (map max-per-color)
                   (map-indexed (fn [i v] (when (map-ge? test-max v) (inc i))))
                   (filter identity))
             +
             (string/split-lines input)))

;; Part 2
(comment
  (transduce (map (comp (partial reduce *)
                        vals
                        max-per-color
                        revelations))
             +
             (string/split-lines input)))
