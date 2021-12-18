(ns aoc-clj.day3
  (:require [aoc-clj.core :refer [load-lines str->int]]))

(def lines (load-lines "2021/day3.txt"))
(defn str->item [val] (str->int (str val)))
(def items (map #(map str->item %) lines))

(def part1-test-items
  (map #(map str->item %)
       ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"]))
(def part2-test-items part1-test-items)

(defn bit->increment
  [bit]
  (if (= 1 bit) 1 -1))

(defn bits->increments
  [bit-set]
  (map bit->increment bit-set))

(defn combine-increments
  [increments]
  (reduce (fn [increments1 increments2]
            (loop [combined (list)
                   [inc1 & rest1] increments1
                   [inc2 & rest2] increments2]
              (if inc1
                (recur (conj combined (+ inc1 inc2)) rest1 rest2)
                (reverse combined))))
          increments))

(defn total->bit
  [total]
  (if (>= total 0) 1 0))

(defn totals->bits
  [totals]
  (map total->bit totals))

(defn reverse-bits
  [bit-set]
  (map #(if (= 1 %) 0 1) bit-set))

(defn bits->int
  [bits]
  (loop [value 0 [bit & rest] bits]
    (if bit
      (recur (+ (* 2 value) bit) rest)
      value)))

(defn part1 [bit-sets]
  (let [increments (map bits->increments bit-sets)
        totals (combine-increments increments)
        gamma-bits (totals->bits totals)
        gamma (bits->int gamma-bits)
        epsilon-bits (reverse-bits gamma-bits)
        epsilon (bits->int epsilon-bits)]
    {:answer (* gamma epsilon)
     :gamma-bits gamma-bits
     :epsilon-bits epsilon-bits
     :gamma gamma
     :epsilon epsilon}))

(println "Part 1 test answer:")
(println (part1 part1-test-items))

(println "Part 1 answer:")
(println (part1 items))

(defn filter-bit-sets
  [criteria-fn tie position bit-sets]
  (let [bits (map #(nth % position) bit-sets)
        freq (frequencies bits)
        is-tie (= (get freq 1) (get freq 0))
        ranked-bits (map first (sort-by #(- (second %)) freq))
        bit-value (if is-tie
                    tie
                    (criteria-fn ranked-bits))]
    (filter #(= bit-value (nth % position)) bit-sets)))

(defn find-indicator-value
  [bit-sets criteria-fn tie]
  (loop [bit-sets bit-sets
         position 0]
    (if (= 1 (count bit-sets))
      (first bit-sets)
      (recur (filter-bit-sets criteria-fn tie position bit-sets)
             (inc position)))))

(defn part2
  [bit-sets]
  (let [oxygen-bits (find-indicator-value bit-sets first 1)
        oxygen (bits->int oxygen-bits)
        co2-bits (find-indicator-value bit-sets second 0)
        co2 (bits->int co2-bits)]
    {:answer (* oxygen co2)
     :oxygen-bits oxygen-bits
     :oxygen oxygen
     :co2-bits co2-bits
     :co2 co2}))

(println "Part 2 test answer:")
(println (part2 part2-test-items))

(println "Part 2 answer:")
(println (part2 items))
