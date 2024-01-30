(ns aoc2023.day7
  (:require
   [clojure.string :as string]))

(def input (slurp "resource/input/2023/day7.txt"))

(def card-order [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])
(defn card-order->strength [order]
  (into {} (map vector order (range))))

(def card-strength (card-order->strength card-order))

(defn kind-strength [hand]
  (let [f (frequencies hand)
        fvals (vec (vals f))
        max-f (apply max (vals f))]
    (cond
      (= max-f 5) 0
      (= max-f 4) 1
      (= (set fvals) #{3 2}) 2
      (= max-f 3) 3
      (= (count (filter #{2} fvals)) 2) 4
      (= max-f 2) 5
      :else 6)))

(defn strength [hand] (into [(kind-strength hand)] (map card-strength hand)))

;; Part 1
(comment
  (let [m (->> (re-seq #"[\w]+" input)
               (partition 2)
               (map (fn [[hand bid]] {:hand hand :bid (parse-long bid) :strength (strength hand)})))]
    (->> (sort-by :strength #(compare %2 %1) m)
         (map :bid)
         (map * (map inc (range)))
         (reduce +))))
; 250254244

;; Part 2

(def card-order-2 [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J])
(def card-strength-2 (card-order->strength card-order-2))

(defn joker-pretend [hand]
  (let [cmp (fn [[c1 f1] [c2 f2]]
              (compare [f2 (card-strength-2 c1)]
                       [f1 (card-strength-2 c2)]))
        pretend-card (->> (frequencies hand)
                          (sort cmp)
                          (map key)
                          (remove #{\J})
                          first)]
    (cond-> hand
      pretend-card (string/replace \J pretend-card))))

(defn strength-2 [hand]
  (apply vector
         (kind-strength (joker-pretend hand))
         (map card-strength-2 hand)))

(comment
  (let [m (->> (re-seq #"[\w]+" input)
               (partition 2)
               (map (fn [[hand bid]] {:bid (parse-long bid)
                                      :strength (strength-2 hand)})))]
    (->> (sort-by :strength #(compare %2 %1) m)
         (map :bid)
         (map * (map inc (range)))
         (reduce +))))
; 250087440

