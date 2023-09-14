(ns aoc.day6)

(def input (slurp "resource/input/day6.txt"))

(defn detect [len]
  (->> input
       (partition len 1)
       (keep-indexed (fn [i coll]
                       (when (apply distinct? coll)
                         i)))
       first
       (+ len)))

(comment
  (detect 4)
  (detect 14))
