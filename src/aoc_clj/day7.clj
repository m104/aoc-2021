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

(defn fuel-cost
  [coll target]
  (reduce
   (fn [sum pos]
     (let [fuel (apply - (reverse (sort [target pos])))]
       (println "pos:" pos "fuel:" fuel)
       (+ sum
          fuel)))
   0
   coll))

(defn min-fuel-cost
  [coll]
  (let [target (median coll)]
    (println "target:" target)
    (fuel-cost coll target)))

(defn part1
  [lines]
  (-> lines
      parse-input
      min-fuel-cost))

(println "Part 1 test answer:")
(println (part1 test-lines))

(println "Part 1 answer:")
(println (part1 input-lines))

(defn gauss-sum
  [n]
  (/ (* n (inc n))
     2))

(defn fuel-cost2
  [positions target]
  (reduce
   (fn [sum position]
     (let [delta (apply - (reverse (sort [position target])))
           fuel (gauss-sum delta)]
       (+ sum fuel)))
   0
   positions))

(defn min-fuel-cost2
  [positions]
  (reduce
   (fn [prev position]
     (let [cost (fuel-cost2 positions position)]
       (println "position:" position "cost:" cost)
       (min prev cost)))
   (Integer/MAX_VALUE)
   (range (apply max positions))))

(defn min-fuel-cost3
  [positions]
  (loop [left 0
         right (dec (apply max positions))
         left-value (fuel-cost2 positions left)
         right-value (fuel-cost2 positions right)]
    (println "left:" left "right:" right "lvalue:" left-value "rvalue:" right-value)
    (if (<= (- right left)
            1)
      (min left-value right-value)
      (let [pivot (int (Math/floor (+ left (/ (- right left) 2))))
            pivot-value (fuel-cost2 positions pivot)]

        (if (<= left-value right-value)
          (recur left pivot left-value pivot-value)
          (recur pivot right pivot-value right-value))))))

(defn part2
  [lines]
  (-> lines
      parse-input
      min-fuel-cost3))

(println "Part 2 test answer:")
(println (part2 test-lines))

(println "Part 2 answer:")
(println (part2 input-lines))
