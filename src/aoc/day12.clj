(ns aoc.day12
  (:require [clojure.string :as s])
  (:import [clojure.lang PersistentQueue]))

(def ex
"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn process-input [s]
  (->> (s/split-lines s)
       (mapv (partial mapv int))))

(def terrain (-> #_ex
                 (slurp "resource/input/day12.txt")
                 process-input))

(defn matrix->map
  "[[0 1] [2 3]] to {[0 0] 0 ...}"
  [matrix]
  (reduce #(apply assoc %1 %2)
          {}
          (for [i (range (count matrix))
                j (range (count (first matrix)))]
            [[i j] (get-in matrix [i j])])))

(def terrain-map (matrix->map terrain))

(defn get-terrain [p]
  (when-let [t (terrain-map p)]
    (cond
      (= t (int \S)) (int \a)
      (= t (int \E)) (int \z)
      :else t)))

(defn find-m [value]
  (filter (fn [[_ v]] (= v value)) terrain-map))

(defn find-char [c] (ffirst (find-m (int c))))
(def end (find-char \E))
(def start (find-char \S))

(defn destination? [p] (= p end))

(defn walkable? [p1 p2]
  (when-let [p2-terrain (get-terrain p2)]
    (<= (- p2-terrain (get-terrain p1)) 1)))

(defn next-steps [[x y :as p] visited?]
  (->> [[(inc x) y] [x (dec y)] [(dec x) y] [x (inc y)]]
       (remove visited?)
       (filter (partial walkable? p))))

(defn reconstruct [m]
  (loop [path []
         next end]
    (if next
      (recur (conj path next)
             (get m next))
      path)))

(defn print-result-matrix [xs]
  (let [path? (into #{} xs)]
    (doseq [x (range (count terrain))
           y (range (count (first terrain)))]
     (when (= y 0) (println))
     (print
      (cond
        (= [x y] start) "S"
        (= [x y] end) "E"
        (path? [x y]) "X"
        :else ".")))))

(defn bfs
  [p next-steps done?]
  (loop [queue (conj PersistentQueue/EMPTY p)
         seen {p nil}]
    (let [coord (peek queue)]
      (if (done? coord)
        (reconstruct seen)
        (let [visited (into #{} (keys seen))
              nexts (next-steps coord visited)]
          (recur (apply conj (pop queue) nexts)
                 (reduce (fn [acc next]
                           (assoc acc next coord))
                         seen
                         nexts)))))))

(comment "First part"
  (dec (count (bfs start next-steps destination?)))
  ;; => 484

)
