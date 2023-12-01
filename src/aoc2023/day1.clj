(ns aoc2023.day1
  (:require [clojure.string :as string]))

;; Day 1: Trebuchet?!

(def input (slurp "resource/input/2023/day1.txt"))

;; Part 1
(comment
  (->> input
       string/split-lines
       (mapv #(let [numbers (re-seq #"\d" %)]
                (+ (* 10 (parse-long (first numbers))) (parse-long (last numbers)))))
       (reduce +)))

;; Part 2 - solution with regex
(def spelled-out
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(def str->digit
  (into spelled-out (map (juxt str identity)) (range 1 10)))

;; (?=(...))
;; Lookahead. They do not consume characters in the string, but only assert whether a match is possible or not.
(def pattern
  (re-pattern (str "(?=(" (string/join "|" (keys str->digit)) "))")))

(comment
  (->> input
       string/split-lines
       (mapv #(let [numbers (->> (re-seq pattern %)
                                 (map second)
                                 (map str->digit))]
                (+ (* 10 (first numbers)) (last numbers))))
       (reduce +)))


;; Part 2 - solution with index-of
(defn extract-digits [s substrs]
  (->> (map (juxt identity (partial string/index-of s)) substrs)
       (filter second)
       (sort-by second) ; sort by index
       (map first))) ; clean up index

(defn decode [s]
  (let [digits-str (extract-digits s (keys str->digit))]
    (parse-long (str (str->digit (first digits-str))
                     (str->digit (last digits-str))))))

(comment
  (->> input
       string/split-lines
       (map decode)
       (reduce +)))
