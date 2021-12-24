(ns aoc-clj.day8
  (:require [aoc-clj.core :refer [load-lines str->int]]
            [clojure.string :refer [split split-lines trim]]
            [clojure.set :refer [union]]))

(def input-lines (load-lines "2021/day8.txt"))

(def test-lines
  (split-lines
   "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

(defn parse-signals
  [s]
  (mapv #(into #{} (split % #""))
        (split s #" ")))

(defn parse-input
  [lines]
  (mapv
   (fn [line]
     (let [[patterns outputs] (map parse-signals
                                   (split line #" \| "))]
       {:patterns patterns
        :outputs outputs}))
   lines))

(def digit-connections
  {0 #{:a :b :c :e :f :g}
   1 #{:c :f}
   2 #{:a :c :d :e :g}
   3 #{:a :c :d :f :g}
   4 #{:b :c :d :f}
   5 #{:a :b :d :f :g}
   6 #{:a :b :d :e :f :g}
   7 #{:a :c :f}
   8 #{:a :b :c :d :e :f :g}
   9 #{:a :b :c :d :f :g}})

(def digits-by-connection-count
  (reduce-kv
   (fn [coll digit connections]
     (println "digit:" digit "connections:" connections)
     (update coll
             (count connections)
             (fn [current]
               (union (or current #{})
                      #{digit}))))
   {}
   digit-connections))

(defn count-unique-output-digits
  [outputs]
  (count (filter #(= 1 (count
                        (get digits-by-connection-count
                             (count %))))
                 outputs)))

(defn part1
  [lines]
  (->> lines
       parse-input
       (map :outputs)
       (map count-unique-output-digits)
       (reduce +)))

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
