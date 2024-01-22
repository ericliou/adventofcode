(ns aoc2023.day6
  (:require [clojure.string :as string]
            [clojure.math :refer [sqrt floor ceil]]))

(def input
  "Time:        47     84     74     67
Distance:   207   1394   1209   1014")

(defn -int? [n] (zero? (mod n 1)))

(defn quadratic-formula [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (when (pos? discriminant)
      [(/ (+ (- b) (sqrt discriminant)) (* 2 a))
       (/ (- (- b) (sqrt discriminant)) (* 2 a))])))

(defn count-beat-record [allowed-time record-distance]
  (let [results (quadratic-formula -1 allowed-time (- record-distance))
        [i0 i1] (let [[low high] (sort results)]
                  [(long (if (-int? low) (inc low) (ceil low)))
                   (long (if (-int? high) (dec high) (floor high)))])]
    (inc (- i1 i0))))

;; Part 1
(comment
  (let [extract-ints (fn [s] (map parse-long (re-seq #"\d+" s)))
        time-dist-pairs (->> (string/split-lines input)
                             (mapv extract-ints)
                             (apply mapv vector))]
    (reduce * (map (partial apply count-beat-record) time-dist-pairs))))

;; Part 2
(comment
  (let [extract-int (fn [s] (parse-long (apply str (re-seq #"\d+" s))))
        time-dist-pair (->> (string/split-lines input)
                            (mapv extract-int))]
    (apply count-beat-record time-dist-pair)))
