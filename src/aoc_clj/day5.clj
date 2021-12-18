(ns aoc-clj.day5
  (:require [aoc-clj.core :refer [load-lines str->int]]
            [clojure.string :refer [split split-lines trim]]))

(def input-lines (load-lines "2021/day5.txt"))

(def test-lines
  (split-lines
   "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))

(defn parse-input-line
  [line]
  (let [[first last] (split line #" -> ")
        ; split coords
        [x1 y1] (map str->int (split first #","))
        [x2 y2] (map str->int (split last #","))
        ; sort coords
        [x1 x2] (sort [x1 x2])
        [y1 y2] (sort [y1 y2])]
    (when (or (= x1 x2) (= y1 y2))
      (for [y (range y1 (inc y2))
            x (range x1 (inc x2))]
        [x y]))))

(defn parse-input
  [lines]
  (->> lines
       (map parse-input-line)
       (filter seq)
       (apply concat)))

(defn make-map
  [coords]
  (loop [m {}
         [xy & rest] coords
         i 0]
    (if (seq xy)
      (recur (update m xy #(inc (or % 0))) rest (inc i))
      {:i i
       :map m})))

(defn make-fast-map
  [coords]
  (loop [m (transient {})
         [xy & rest] coords
         i 0]
    (if (seq xy)
      (let [current (get m xy 0)
            new-m (assoc! m xy (inc current))]
        (recur new-m rest (inc i)))
      {:i i
       :map (persistent! m)})))

(defn find-matching-coords
  [m]
  (count (filter #(>= % 2) (vals m))))

(defn part1
  [lines]
  (-> lines
      parse-input
      make-map
      :map
      find-matching-coords))

(println "Part 1 test answer:")
(println (part1 test-lines))

(println "Part 1 answer:")
(println (part1 input-lines))

(take 5 (iterate #(+ -1 %) 5))

(defn walk-seq
  [start end]
  (let [[min max] (sort [start end])
        delta (- max min)
        dir (if (<= start end) inc dec)]
    (take (inc delta) (iterate dir start))))

(defn parse-input-line-with-diags
  [line]
  (let [[first last] (split line #" -> ")
        [x1 y1] (map str->int (split first #","))
        [x2 y2] (map str->int (split last #","))
        dx (- x2 x1)
        dy (- y2 y1)]
    (cond
      ; horizontal line
      (and (= dy 0) (not= dx 0))
      (for [x (walk-seq x1 x2)]
        [x y1])
      ; vertical line
      (and (= dx 0) (not= dy 0))
      (for [y (walk-seq y1 y2)]
        [x1 y])
      ; diagonal line
      (and (= (Math/abs dx) (Math/abs dy)) (not= dx 0))
      (let [xseq (walk-seq x1 x2)
            yseq (walk-seq y1 y2)]
        (for [n (range (count xseq))]
          [(nth xseq n) (nth yseq n)])))))

(defn parse-input-with-diags
  [lines]
  (->> lines
       (map parse-input-line-with-diags)
       (filter seq)
       (apply concat)))

(parse-input-with-diags test-lines)

(defn part2
  [lines]
  (-> lines
      parse-input-with-diags
      make-fast-map
      :map
      find-matching-coords
      time))

(println "Part 2 test answer:")
(println (part2 test-lines))

(println "Part 2 answer:")
(println (part2 input-lines))
