(ns aoc.day5
  (:require [clojure.string :as str]))

;;     [G] [R]                 [P]
;;     [H] [W]     [T] [P]     [H]
;;     [F] [T] [P] [B] [D]     [N]
;; [L] [T] [M] [Q] [L] [C]     [Z]
;; [C] [C] [N] [V] [S] [H]     [V] [G]
;; [G] [L] [F] [D] [M] [V] [T] [J] [H]
;; [M] [D] [J] [F] [F] [N] [C] [S] [F]
;; [Q] [R] [V] [J] [N] [R] [H] [G] [Z]
;;  1   2   3   4   5   6   7   8   9

(def initial-crates
  ['()
   '(\L \C \G \M \Q)
   '(\G \H \F \T \C \L \D \R)
   '(\R \W \T \M \N \F \J \V)
   '(\P \Q \V \D \F \J)
   '(\T \B \L \S \M \F \N)
   '(\P \D \C \H \V \N \R)
   '(\T \C \H)
   '(\P \H \N \Z \V \J \S \G)
   '(\G \H \F \Z)])

(defn parse-int [s] (Integer/parseInt s))

(def moves
  (->> (slurp "resource/input/day5.txt")
       str/split-lines
       (drop-while #(not (str/starts-with? % "move")))
       (map #(map parse-int (rest (re-matches #"move (\d+) from (\d+) to (\d+)" %))))))

(defn move [crates n from to]
  (let [[a b] (split-at n (get crates from))]
   (-> crates
       (update from (fn [_] b))
       (update to #(into % a)))))

(defn move-retaining-order [crates n from to]
  (let [[a b] (split-at n (get crates from))]
    (-> crates
        (update from (fn [_] b))
        (update to #(concat a %)))))

(comment "Part 1"
         (->> (reduce #(apply move %1 %2)
                      initial-crates
                      moves)
              (map first)
              (apply clojure.core/str)))
;; => "VCTFTJQCG"

(comment "Part 2"
         (->> (reduce #(apply move-retaining-order %1 %2)
                      initial-crates
                      moves)
              (map first)
              (apply clojure.core/str)))
;; => "GCFGLDNJZ"
