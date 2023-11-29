(ns aoc.day7
  (:require [clojure.string :as str]))

(defn file-size [line-file]
  (parse-long (second (re-matches #"^(\d+) .*" line-file))))

(defn cmd [line]
  (condp re-matches line
    #"^(\d+) .*" :file
    #"^\$ cd .*" :cd
    :noop))

(defn cd-path [line-cd]
  (second (re-matches #"^\$ cd (.*)" line-cd)))

(defn make-dir-map [lines]
  (loop [lines lines
         tree {}
         path []]
    (if (seq lines)
      (case (cmd (first lines))
        :cd (recur (rest lines) tree (if (= ".." (cd-path (first lines)))
                                       (vec (butlast path))
                                       (conj path (cd-path (first lines)))))
        :file (recur (rest lines) (update tree path #(+ (or % 0) (file-size (first lines)))) path)
        (recur (rest lines) tree path))
      tree)))

(defn subpaths [path]
  (when (seq path)
    (into [path] (subpaths (butlast path)))))

(defn include-parents [path->size]
  (mapcat (fn [[path size]]
            (map vector (subpaths path) (repeat size)))
          path->size))

(defn compute-parent-dirs [dir-map]
  (reduce (fn [acc [k v]]
            (update acc k #(+ (or % 0) v)))
          {}
          (include-parents dir-map)))

(def dir->size
  (->> (slurp "resource/input/day7.txt")
       str/split-lines
       make-dir-map
       compute-parent-dirs))

(comment ; Part 1
  (->> dir->size
       (map second)
       (filter #(< % 100000))
       (reduce +)))
;; => 1427048

(comment ; Part 2
  (let [used-space (get dir->size ["/"])
        still-needed-space (- 30000000 (- 70000000 used-space))]
    (->> dir->size
         (map second)
         (filter #(> % still-needed-space))
         (reduce min))))
;; => 2940614
