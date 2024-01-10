(ns aoc.day14
  (:require [clojure.string :as str]))

(def ex
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse-int [s] (Integer/parseInt s))

(defn range-or-repeat [a b]
  (cond
    (> a b) (range b (inc a))
    (< a b) (range a (inc b))
    :else (repeat a)))

(defn gen-rock-coords [[x1 y1] [x2 y2]]
  (map vector (range-or-repeat x1 x2) (range-or-repeat y1 y2)))

(defn tap [x] (println x) x)

(defn min-second
  ([] nil)
  ([c] c)
  ([c1 c2]
   (if (and c1 c2)
     (min-key second c1 c2)
     (or c1 c2))))


;; Performance improvement attempts on the fn `highest`:
;;
;; - threading vs transducer
;;   transduce improved from 1200ms to 790ms
;;
;; - experiment with coords data structure, index by x and sort by y
;;   painful, the rest is too coupled with it
;;
;; - reduce number of calls of the fn highest
;;   reduced from 750ms to 370ms
;;
;; - remove first and second, and changed to destructuring
;;   reduced from 370ms to 240ms
;;
;; - union vs conj
;;   270 vs 250 (interesting)
;;
;; - pmap vs map (instead of filter)
;;   2913ms vs 655ms (!)

(defn highest
  "Highest under y inclusive."
  [coords x y]
  #_(->> coords
       (pmap (fn mapping [[i j]] ( (and (= x i) (>= j y)))))
       (filter identity)
       (reduce min-second) tap)

  (transduce (filter (fn filtering [[i j]] (and (= x i) (>= j y))))
             min-second
             coords)
  #_(some->> coords
           (filter (fn filtering [i] (and (= x (first i)) (>= (second i) y))))
           not-empty
           (apply min-key second)))

(def rocks-coords
(->> ;; ex
    (slurp "resource/input/day14.txt")
       str/split-lines
       (take 1) ;; small test set
       (mapcat (fn [s]
                 (->> (str/split s #" -> ")
                      (map #(mapv parse-int (str/split % #",")))
                      (partition 2 1)
                      (mapcat (partial apply gen-rock-coords))))))
  )

(def rocks
  (->> rocks-coords
       (into #{})))

(def rocks-matrix
  (let [all-elements rocks
        [min-x _] (apply min-key first all-elements)
        [max-x _] (apply max-key first all-elements)
        [_ min-y] (apply min-key second all-elements)
        [_ max-y] (apply max-key second all-elements)]

    (for [j (range (inc max-y))
          i (range (inc max-x))]
      (when (rocks [i j])
        :rock))))

(defn highest-surface
  "nil if no surface"
  [sands [x y]]
  (highest (into sands rocks) x y))

(def source-x 500)
(def source-y 0)
(def source-coord [source-x source-y])

(defn add-sand [sands [x y]]
  (conj sands [x (dec y)]))

(defn sand? [sands x y]
  (sands [x y]))

(defn rock? [x y]
  (rocks [x y]))

(defn blocked? [sands [x y]]
  (or (sand? sands x y) (rock? x y)))

(defn print-cave [sands rocks]
  (let [all-elements (into sands rocks)
        [min-x _] (apply min-key first all-elements)
        [max-x _] (apply max-key first all-elements)
        [_ min-y] (apply min-key second all-elements)
        [_ max-y] (apply max-key second all-elements)]
    (println "x" [min-x max-x] "y" [min-y max-y])
    (doseq [j (range min-y (inc max-y))
            i (range min-x (inc max-x))]
      (when (= i min-x) (println))
      (cond
        (sand? sands i j) (print "O")
        (rock? i j) (print "X")
        :else (print "."))))
  (println))

(defn drop-sand-fn [stop? highest-surface blocked?]

  (fn [[x y :as coords] sands iter]
    (let [[hx hy :as highest] (highest-surface sands coords)]

      (if (stop? highest)
        (do #_(print-cave sands rocks)
            (count sands))

        (let [highest-l [(dec x) hy]
              highest-r [(inc x) hy]
              iter (inc iter)]
          (cond
            (and (blocked? sands highest-l)
                 (blocked? sands highest-r))
           ;; call next drop from source
            (recur source-coord (add-sand sands highest) iter)

           ;; left blocked, right free
            (blocked? sands highest-l)
            (recur highest-r sands iter)

           ;; left free, right any
            :else
            (recur highest-l sands iter)))))))

(defn infinite-fall? [highest-surface]
  (nil? highest-surface))

(defn source-blocked? [highest-surface]
  (= highest-surface source-coord))

(def max-y (+ 2 (second (apply max-key second rocks))))

(defn highest-surface-finite [sands [x _y :as coords]]
  (or (highest-surface sands coords)
      [x max-y]))

(defn blocked-finite? [sands [_x y :as coords]]
  (or (blocked? sands coords)
      (= y max-y)))


(comment "part 1"
  ((drop-sand-fn infinite-fall? highest-surface blocked?) source-coord #{} 0)

  ;; 719
  )

(comment "part 2"
         ;; takes a really long time, needs improvement, only using single thread
         (time ((drop-sand-fn source-blocked? highest-surface-finite blocked-finite?) source-coord #{} 0))
         #_(time (dotimes [n 100] ((drop-sand-fn source-blocked? highest-surface-finite blocked-finite?) source-coord #{} 0)));; => 23390
)

(defn burn-cpu [op secs]
  (let [start (System/nanoTime)]
    (while (< (/ (- (System/nanoTime) start) 1e9) secs)
      (op))))

(defn test-one []
  (burn-cpu #(reduce + (mapv inc (range 1000))) 10))

(comment
  (test-one)
  )
