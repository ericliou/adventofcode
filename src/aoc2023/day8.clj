(ns aoc2023.day8
  (:require
   [clojure.string :as string]
   [clojure.math.numeric-tower :as math]))

(def input (slurp "resource/input/2023/day8.txt"))

(defn- next-node [network current instr]
  (let [[l r] (get network current)]
    (if (= instr \L) l r)))

(defn parse [input]
  (let [[instructions _ & nodes] (string/split-lines input)]
    {:instructions instructions
     :network (->> (map (partial re-seq #"\w+") nodes)
                   (reduce (fn [acc [k r l]]
                             (assoc acc k [r l])) {}))}))

(defn- journey-len [instructions network end? start]
  (loop [current start
         i 0
         instructions (cycle instructions)]
    (if (end? current)
      i
      (let [next (next-node network current (first instructions))]
        (recur next
               (inc i)
               (rest instructions))))))

;; Part 1
(comment
  (let [{:keys [network instructions]} (parse input)]
    (journey-len instructions network #(= % "ZZZ") "AAA")))
;;
;; Part 2
(comment
  (let [{:keys [network instructions]} (parse input)
        start-nodes (filter #(= (last %) \A) (keys network))
        end? (fn end? [node] (= (last node) \Z))
        journey-len #(journey-len instructions network end? %)]
    (reduce math/lcm 1 (mapv journey-len start-nodes)))) ; 14616363770447

