(ns aoc-clj.day9
  (:require [aoc-clj.core :refer [load-lines str->int combinations
                                  invert-map invert-map-of-sets]]
            [clojure.string :refer [split split-lines trim]]
            [clojure.set :refer [union]]))

(def input-lines (load-lines "2021/day9.txt"))

(def test-lines
  (split-lines
   "2199943210
3987894921
9856789892
8767896789
9899965678"))

(defn parse-row
  [line]
  (mapv str->int (split line #"")))

(defn parse-input
  [lines]
  (let [rows (mapv parse-row lines)]
    {:width (count (first rows))
     :height (count rows)
     :rows rows}))

(defn valid-position?
  [{:keys [width height]} [col row]]
  (and (< -1 col width)
       (< -1 row height)))

(defn get-value
  [{:keys [width height rows] :as heatmap}
   [col row]]
  (if (valid-position? heatmap [col row])
    (nth (nth rows row) col)
    9))

(defn neighbors
  [[col row]]
  (for [[dx dy] [[1 0]
                 [-1 0]
                 [0 1]
                 [0 -1]]]
    [(+ col dx) (+ row dy)]))

(defn low-point?
  [heatmap coords]
  (let [value (get-value heatmap coords)]
    (every?
     (fn [neighbor]
       (< value
          (get-value heatmap neighbor)))
     (neighbors coords))))

(defn find-low-points
  [{:keys [width height] :as heatmap}]
  (let [all-coords (for [col (range 0 width)
                         row (range 0 height)]
                     [col row])]
    (filter (partial low-point? heatmap) all-coords)))

(defn part1
  [lines]
  (let [heatmap (parse-input lines)
        low-points (for [coord (find-low-points heatmap)]
                     {:coord coord
                      :risk (inc (get-value heatmap coord))})
        total-risk (reduce + 0 (map :risk low-points))]
    {:count (count low-points)
     :total-risk total-risk
     :low-points low-points}))

(println "Part 1 test answer:")
(println (part1 test-lines))
; 15

(println "Part 1 answer:")
(println (part1 input-lines))
; 498

(defn part2
  [lines]
  (->> lines
       parse-input))

(println "Part 2 test answer:")
;(println (part2 test-lines))

(println "Part 2 answer:")
;(println (part2 input-lines))
