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

(def get-terrain
  (let [start (int \S)
        end (int \E)]
    (fn [p]
      (when-let [t (terrain-map p)]
        (cond
          (= t start) (int \a)
          (= t end) (int \z)
          :else t)))))

(defn find-m [value]
  (filter (fn [[_ v]] (= v value)) terrain-map))

(defn find-char [c] (ffirst (find-m (int c))))
(def end (find-char \E))
(def start (find-char \S))
(def possible-starts (conj (map first (find-m (int \a))) start))

(defn destination? [p] (= p end))

(defn walkable? [p1 p2]
  (let [t1 (get-terrain p1)
        t2 (get-terrain p2)]
    (when (and t1 t2)
      (<= (- t2 t1) 1))))

(defn reverse-walkable? [p1 p2]
  (walkable? p2 p1))

(defn next-steps [walkable? [x y :as p] visited?]
  (->> [[(inc x) y] [x (dec y)] [(dec x) y] [x (inc y)]]
       (remove visited?)
       (filter (partial walkable? p))))

(defn reconstruct [m last-coord]
  (loop [path []
         next last-coord]
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
      (cond

        (empty? queue)
        nil

        (done? coord)
        (reconstruct seen coord)

        :else
        (let [visited (into #{} (keys seen))
              nexts (next-steps coord visited)]
          (recur (apply conj (pop queue) nexts)
                 (reduce (fn [acc next]
                           (assoc acc next coord))
                         seen
                         nexts)))))))

(comment "Part 1"
         (dec (count (bfs start (partial next-steps walkable?) destination?)))
  ;; => 484
         )

(comment "Part 2"

         (apply min
                (transduce (comp
                            (map #(bfs % (partial next-steps reverse-walkable?) destination?))
                            (filter identity)
                            (map count)
                            (map dec))
                           conj
                           possible-starts))
  ;; => 17

  ;; more efficient solution: do a reverse lookup
         (let [path (bfs end (partial next-steps walkable?) (fn [p] (contains? (into #{} possible-starts) p)))]
           (print-result-matrix path)
           (dec (count path)))
  ;; 17
         )
