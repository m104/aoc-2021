(ns aoc-clj.day2
  (:require [aoc-clj.core :refer [load-lines str->int]]
            [clojure.string :refer [split]]))

(def lines (load-lines "2021/day2.txt"))
(defn str->item [val] (let [[direction distance-str] (split val #" ")]
                        [direction (str->int distance-str)]))
(def items (map str->item lines))

(def part1-test-items (map str->item
                           ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"]))
(def part2-test-items part1-test-items)

(defn update-location
  [{:keys [position depth]} [direction distance]]
  (let [[coordinate increment] (case direction
                                 "up" [:depth (- distance)]
                                 "down" [:depth distance]
                                 "forward" [:position distance])
        depth (if (= coordinate :depth) (+ increment depth) depth)
        position (if (= coordinate :position) (+ increment position) position)]
    {:position position :depth depth}))

(def initial-location {:depth 0 :position 0})

(defn part1 [movements]
  (loop [location initial-location
         [movement & rest] movements]
    (if movement
      (recur (update-location location movement)
             rest)
      location)))

(println "Part 1 test answer:")
(println (part1 part1-test-items))

(println "Part 1 answer:")
(println (part1 items))

(defn update-aim
  [location units]
  (update location :aim + units))

(defn update-aimed-location
  [{:keys [aim] :as location} amount]
  (-> location
      (update :depth + (* amount aim))
      (update :position + amount)))

(defn update-by-movement
  [location [direction units]]
  (case direction
    "up" (update-aim location (- units))
    "down" (update-aim location units)
    "forward" (update-aimed-location location units)))

(def initial-aimed-location {:depth 0 :position 0 :aim 0})

(defn part2 [movements]
  (reduce update-by-movement initial-aimed-location movements))

(println "Part 2 test answer:")
(println (part2 part2-test-items))

(println "Part 2 answer:")
(println (part2 items))
