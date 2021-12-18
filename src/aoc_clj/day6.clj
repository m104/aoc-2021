(ns aoc-clj.day6
  (:require [aoc-clj.core :refer [load-lines str->int]]
            [clojure.string :refer [split split-lines trim]]))

(def input-lines (load-lines "2021/day6.txt"))

(def test-lines
  (split-lines "3,4,3,1,2"))

(def repro-time 7)

(def initial-state {:day 0 :fish []})

(defn parse-input
  [lines]
  (let [line (first lines)
        fish (mapv str->int (split line #","))]
    (assoc initial-state :fish fish)))

(defn simulate-day
  [{:keys [day fish]}]
  (loop [[current & remaining] fish
         existing []
         new []]
    (if current
      (let [has-child (= 0 current)
            updated (if has-child (dec repro-time) (dec current))
            new (if has-child
                  (conj new (inc repro-time))
                  new)]
        (recur remaining (conj existing updated) new))
      {:day (inc day)
       :fish (into [] (concat existing new))})))

(defn simulate-days
  [state days]
  (loop [state state day 1]
    (println "State on day" day ":" state)
    (if (> day days)
      state
      (recur (simulate-day state) (inc day)))))

(defn score-state
  [{:keys [fish days]}]
  {:answer (count fish)
   :days days})

(defn part1
  [lines days]
  (-> lines
      parse-input
      (simulate-days days)
      score-state))

(println "Part 1 test answer:")
(println (part1 test-lines 18))

(println "Part 1 answer:")
;(println (part1 input-lines 80))

(defn group-fish
  [{:keys [day fish]}]
  {:day day
   :fish-groups (frequencies fish)})

(defn simulate-day-faster
  [{:keys [day fish-groups]}]
  ;(println "Simulating day" (inc day) fish-groups)
  {:day (inc day)
   :fish-groups
   (letfn [(add [increment] #(apply + (filter number? [increment %])))]
     (reduce-kv
      (fn [new-groups age count]
        (cond
          (= 0 age) (-> new-groups
                        (update (inc repro-time) (add count))
                        (update (dec repro-time) (add count)))
          :else (update new-groups (dec age) (add count))))
      {} fish-groups))})

(defn simulate-days-faster
  [state days]
  ;(println "Simulating..." days state)
  (last (take (inc days) (iterate simulate-day-faster state))))

(defn grouped-fish-answer
  [{:keys [fish-groups]}]
  ;(println "Summing up" fish-groups)
  {:answer (->> fish-groups
                vals
                (reduce +))})

(defn part2
  [lines days]
  (-> lines
      parse-input
      group-fish
      (simulate-days-faster days)
      grouped-fish-answer))

(println "Part 2 test answer:")
(println (part2 test-lines 18))

(println "Part 2 answer:")
(println (part2 input-lines 256))
