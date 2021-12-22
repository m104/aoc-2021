(ns aoc-clj.day7
  (:require [aoc-clj.core :refer [load-lines str->int]]
            [clojure.string :refer [split split-lines trim]]))

(def input-lines (load-lines "2021/day7.txt"))

(def test-lines
  (split-lines "16,1,2,0,4,2,7,1,2,14"))

(defn parse-input
  [lines]
  (mapv str->int
        (-> lines
            first
            (split #","))))

(defn median
  [coll]
  (let [sorted (sort coll)
        midpoint (/ (count sorted) 2)]
    (nth sorted midpoint)))

(defn part1
  [lines]
  (-> lines
      parse-input
      median))

(println "Part 1 test answer:")
(println (part1 test-lines))

(println "Part 1 answer:")
(println (part1 input-lines))

; TODO
(defn part2
  [lines]
  (-> lines
      parse-input))

(println "Part 2 test answer:")
(println (part2 test-lines))

(println "Part 2 answer:")
(println (part2 input-lines))
