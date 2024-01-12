(ns aoc2023.day5
  (:require
   [clojure.string :as string]
   [clj-async-profiler.core :as prof]))

(def input (slurp "resource/input/2023/day5.txt"))

(defn convert [mapping x]
  (or (some (fn [[dst-start src-start len]]
              (when (and (<= src-start x) (< x (+ src-start len)))
                 (+ dst-start (- x src-start))))
            mapping)
      x))

(defn- parse [input]
  (->> (string/split input #"\n\n")
       (map #(map parse-long (re-seq #"\d+" %)))))

(defn- chain-convert [maps s]
  (reduce (fn [acc mp] (convert mp acc)) s maps))

;; Part 1
(comment
  (let [[seeds & rst] (parse input)
        maps (map (partial partition 3) rst)]
    (->> (map (partial chain-convert maps) seeds)
         (reduce min))))
  ;; => 313045984

;; Part 2
;; Elapsed time: 2357166 msecs (40 min)
(comment
  (time
   (let [[seeds & rst] (parse input)
         seeds (mapcat (fn [[start len]] (range start (+ start len))) (partition 2 seeds))
         maps (map (partial partition 3) rst)]
     (reduce min (map (partial chain-convert maps) seeds)))))

;; Part 2, reversing solution
;; Elapsed time: 107345 msecs (1.5 min)
(comment
  (time (let [[seeds & rst] (parse input)
         seed-ranges (partition 2 seeds)
         reversed-maps (->> rst reverse (map (fn [m] (mapv (fn [[a b c]] [b a c]) (partition 3 m)))))
         real-seed? (fn [n] (some (fn [[start len]]
                                    (and (>= n start)
                                         (< n (+ start len)))) seed-ranges))]
     (some (fn [x] (when (real-seed? (chain-convert reversed-maps x)) x)) (range 30000000)))))
;; => 82
