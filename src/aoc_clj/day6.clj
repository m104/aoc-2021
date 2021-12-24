(ns aoc-clj.day6
  (:require [aoc-clj.core :refer [load-lines str->int]]
            [clojure.string :refer [split split-lines trim]]))

(def input-lines (load-lines "2021/day6.txt"))

(def test-lines
  (split-lines "3,4,3,1,2"))

(def repro-time 7)

(def initial-state {:day 0})

(defn parse-input
  [lines]
  (let [line (first lines)]
    (mapv str->int (split line #","))))

(defn group-fish-by-age
  [fish]
  (let [grouped (frequencies fish)
        ages (range (inc repro-time))]
    (mapv (fn [age]
            (get grouped age 0))
          ages)))

(defn build-state
  [fish]
  (assoc initial-state :fish-ages (group-fish-by-age fish)))

;; [0 1 2 3 4 5 6 7 8]
;; 0: store
;; 1 - 8: shift left
;; lastly, add the 0 values to 6 and 8

(defn right-padv
  [v size init]
  (if (>= (count v) size)
    v
    (let [remaining (- size (count v))
          fill (mapv (constantly init) (range remaining))]
      (into [] (concat v fill)))))

(defn simulate-day
  [{:keys [day fish-ages] :as state}]
  (println "Simulating day:" state)
  (let [new-fish (nth fish-ages 0)
        new-ages (-> fish-ages
                     (subvec 1)
                     (right-padv (+ 2 repro-time) 0)
                     (update (dec repro-time) + new-fish)
                     (update (inc repro-time) + new-fish))]
    {:day (inc day)
     :fish-ages (into [] new-ages)}))

(defn simulate-days
  [state f days]
  (println "Simulating" days "days, starting:" state)
  (last (take (inc days) (iterate f state))))

(defn score-state-array
  [{:keys [day fish-ages]}]
  {:day day
   :answer (reduce + fish-ages)})

(defn part1
  [lines days]
  (-> lines
      parse-input
      build-state
      (simulate-days simulate-day days)
      score-state-array))

(println "Part 1 test answer:")
(println (part1 test-lines 18))

(println "Part 1 answer:")
(println (part1 input-lines 80))

(defn part2
  [lines days]
  (part1 lines days))

(println "Part 2 test answer:")
(println (part2 test-lines 18))

(println "Part 2 answer:")
(println (part2 input-lines 256))
