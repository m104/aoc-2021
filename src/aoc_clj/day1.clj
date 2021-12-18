(ns aoc-clj.day1
  (:require [aoc-clj.core :refer [load-lines str->int]]))

(def lines (load-lines "2021/day1.txt"))
(def depths (map str->int lines))

(def part1-test-depths [199 200 208 210 200 207 240 269 260 263])
(def part2-test-depths [607 618 618 617 647 716 769 792])

(defn part1
  [depths]
  (->> depths
       (partition 2 1)
       (filter #(apply < %))
       count))

(println "Part 1 test answer:")
(println (part1 part1-test-depths))

(println "Part 1 answer:")
(println (part1 depths))

(defn part2
  [depths]
  (->> depths
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (filter #(apply < %))
       count))

(println "Part 2 test answer:")
(println (part2 part2-test-depths))

(println "Part 2 answer:")
(println (part2 depths))
