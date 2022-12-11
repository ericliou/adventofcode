(ns aoc.day9
  (:require [clojure.string :as s]))

(def input
  (->> (slurp "resource/input/day9.txt")
       s/split-lines
       (map #(update (s/split % #" ") 1 parse-long))))

(def directions
  {"R" [1 0]
   "L" [-1 0]
   "U" [0 1]
   "D" [0 -1]})

(defn norm [n]
  (if (zero? n)
    0
    (/ n (abs n))))

(defn adjacent? [[x1 y1] [x2 y2]]
  (and (<= (abs (- x1 x2)) 1)
       (<= (abs (- y1 y2)) 1)))

(defn new-position [[tx ty :as t] [hx hy :as h]]
  (if (adjacent? t h)
   [tx ty]
   [(+ tx (norm (- hx tx))) (+ ty (norm (- hy ty)))]))

(defn move-head [[hx hy] [cx cy]]
  [(+ hx cx) (+ hy cy)])

(defn trail-count2 [rope-len]
  (let [commands (mapcat #(repeat (second %) (directions (first %))) input)
        head-trail (reductions move-head [0 0] commands)]
    (-> (iterate #(reductions new-position %) head-trail)
        (nth (dec rope-len))
        distinct
        count)))

(comment
  ;; Part 1
  (trail-count2 2)

  ;; Part 2
  (trail-count2 10))


(comment
  (defn trail-count [rope-len]
    (loop [[[cx cy] & c-rest] (mapcat #(repeat (second %) (directions (first %))) input)
           [[hx hy] & ts] (repeat rope-len [0 0])
           trail []]
      (let [ts' (reduce #(conj %1 (new-position %2 (last %1))) [[(+ hx cx) (+ hy cy)]] ts)
            trail' (conj trail (last ts'))]
        (if c-rest
          (recur c-rest ts' trail')
          (count (distinct trail')))))))
