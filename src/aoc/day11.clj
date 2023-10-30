(ns aoc.day11)

(def ex
  [{:items [79 98]
    :base 23
    :op #(*' 19 %)
    :test #(if (zero? (mod % 23)) 2 3)}
   {:items [54, 65, 75, 74]
    :base 19
    :op #(+' 6 %)
    :test #(if (zero? (mod % 19)) 2 0)}
   {:items [79, 60, 97]
    :base 13
    :op #(*' % %)
    :test #(if (zero? (mod % 13)) 1 3)}
   {:items [74]
    :base 17
    :op #(+' 3 %)
    :test #(if (zero? (mod % 17)) 0 1)}])

(def monkeys
  [{:items [57 58]
    :op #(* 19 %)
    :base 7
    :test #(if (zero? (mod % 7)) 2 3)}
   {:items [66, 52, 59, 79, 94, 73]
    :op #(+ 1 %)
    :base 19
    :test #(if (zero? (mod % 19)) 4 6)}
   {:items [80]
    :op #(+ 6 %)
    :base 5
    :test #(if (zero? (mod % 5)) 7 5)}
   {:items [82, 81, 68, 66, 71, 83, 75, 97]
    :op #(+ 5 %)
    :base 11
    :test #(if (zero? (mod % 11)) 5 2)}
   {:items [55, 52, 67, 70, 69, 94, 90]
    :op #(* % %)
    :base 17
    :test #(if (zero? (mod % 17)) 0 3)}
   {:items [69, 85, 89, 91]
    :op #(+ 7 %)
    :base 13
    :test #(if (zero? (mod % 13)) 1 7)}
   {:items [75, 53, 73, 52, 75]
    :op #(* 7 %)
    :base 2
    :test #(if (zero? (mod % 2)) 0 4)}
   {:items [94, 60, 79]
    :op #(+ 2 %)
    :base 3
    :test #(if (zero? (mod % 3)) 1 6)}])

(defn exec-one [worry-fn monkeys n]
  (let [{:keys [items op test active] :as monkey} (get monkeys n)]
    (loop [items items
           monkeys (assoc monkeys n (assoc monkey :items []
                                           :active ((fnil + 0) active (count items))))]
      (if-let [item (first items)]
        (let [new-item (worry-fn (op item))
              n' (test new-item)]
          (recur (rest items)
                 (update-in monkeys [n' :items] #(conj % new-item))))
        monkeys))))

(defn round [worry-fn monkeys]
  (reduce (partial exec-one worry-fn) monkeys (range (count monkeys))))

(comment ; Part 1
  (->> (last (take 21 (iterate (partial round #(quot % 3)) monkeys)))
       (map :active)
       sort
       (take-last 2)
       (reduce *)))

(defn common-divisor [monkeys]
  (->> monkeys (map :base) (reduce *)))

(comment ; Part 2
  (let [monkeys monkeys
        divisor (common-divisor monkeys)]
    (->> (last (take 10001 (iterate (partial round #(mod % divisor)) monkeys)))
        (map :active)
        sort
        (take-last 2)
        (reduce *))))
;; => 14399640002

  ; (prof/profile)
  ; (prof/serve-ui 8080)
