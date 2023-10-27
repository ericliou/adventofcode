(ns aoc.day11
  (:require
   [clojure.math :as math]
   [clj-async-profiler.core :as prof]))

(def input "
Monkey 0:
  Starting items: 57, 58
  Operation: new = old * 19
  Test: divisible by 7
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 66, 52, 59, 79, 94, 73
  Operation: new = old + 1
  Test: divisible by 19
    If true: throw to monkey 4
    If false: throw to monkey 6

Monkey 2:
  Starting items: 80
  Operation: new = old + 6
  Test: divisible by 5
    If true: throw to monkey 7
    If false: throw to monkey 5

Monkey 3:
  Starting items: 82, 81, 68, 66, 71, 83, 75, 97
  Operation: new = old + 5
  Test: divisible by 11
    If true: throw to monkey 5
    If false: throw to monkey 2

Monkey 4:
  Starting items: 55, 52, 67, 70, 69, 94, 90
  Operation: new = old * old
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 3

Monkey 5:
  Starting items: 69, 85, 89, 91
  Operation: new = old + 7
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 7

Monkey 6:
  Starting items: 75, 53, 73, 52, 75
  Operation: new = old * 7
  Test: divisible by 2
    If true: throw to monkey 0
    If false: throw to monkey 4

Monkey 7:
  Starting items: 94, 60, 79
  Operation: new = old + 2
  Test: divisible by 3
    If true: throw to monkey 1
    If false: throw to monkey 6
")

(def ex
  [{:items [79 98]
    :op #(*' 19 %)
    :test #(if (zero? (mod % 23)) 2 3)}
   {:items [54, 65, 75, 74]
    :op #(+' 6 %)
    :test #(if (zero? (mod % 19)) 2 0)}
   {:items [79, 60, 97]
    :op #(math/pow % 2)
    :test #(if (zero? (mod % 13)) 1 3)}
   {:items [74]
    :op #(+' 3 %)
    :test #(if (zero? (mod % 17)) 0 1)}])

(comment
  (mod (* (mod 15 7) 19) 7)
  (mod (* 15 19) 7)

  (let [a 4
        b 9
        c 19

        f (comp long math/pow)
        x (f a b)
        y (mod x c)]

    (println (str x " " y))
    (= (mod x c)
       (mod y c)))
  )


(def monkeys
  [{:items [57 58]
    :op #(* 19 %)
    :test #(if (zero? (mod % 7)) 2 3)}
   {:items [66, 52, 59, 79, 94, 73]
    :op #(+ 1 %)
    :test #(if (zero? (mod % 19)) 4 6)}
   {:items [80]
    :op #(+ 6 %)
    :test #(if (zero? (mod % 5)) 7 5)}
   {:items [82, 81, 68, 66, 71, 83, 75, 97]
    :op #(+ 5 %)
    :test #(if (zero? (mod % 11)) 5 2)}
   {:items [55, 52, 67, 70, 69, 94, 90]
    :op #(* % %)
    :test #(if (zero? (mod % 17)) 0 3)}
   {:items [69, 85, 89, 91]
    :op #(+ 7 %)
    :test #(if (zero? (mod % 13)) 1 7)}
   {:items [75, 53, 73, 52, 75]
    :op #(* 7 %)
    :test #(if (zero? (mod % 2)) 0 4)}
   {:items [94, 60, 79]
    :op #(+ 2 %)
    :test #(if (zero? (mod % 3)) 1 6)}])


(defn exec-one [worry-fn monkeys n]
  (let [{:keys [items op test active] :as monkey} (get monkeys n)]
    (loop [items items
           monkeys (assoc monkeys n (assoc monkey :items []
                                           :active (+ (or active 0) (count items))))]
      (if-let [item (first items)]
        (let [new-item (worry-fn (op item))
              n' (test new-item)]
          (recur (rest items)
                 (update-in monkeys [n' :items] #(conj % new-item))))
        monkeys))))

(def i (atom 0))

(defn round [worry-fn monkeys]
  (println (swap! i inc))
  (reduce (partial exec-one worry-fn) monkeys (range (count monkeys))))


(comment

  (->> (last (take 21 (iterate (partial round #(quot % 3)) monkeys)))
       (map :active)
       sort
       (take-last 2)
       (reduce *)
       )

  (->> (last (take 21 (iterate (partial round #(quot % 3)) ex)))
       (map :active)
       sort
       (take-last 2)
       (reduce *)
       )

  (quot 10 3)

  )

(comment ; Part 2

  ; (prof/profile)

  (->> (last (take 850 (iterate (partial round identity) ex)))
       (map :active)
       sort
       (take-last 2)
       (reduce *)
       )

  (prof/serve-ui 8080)

         )
