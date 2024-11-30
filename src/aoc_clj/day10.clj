(ns aoc-clj.day10
  (:require [aoc-clj.core :refer [load-lines str->int combinations
                                  invert-map invert-map-of-sets]]
            [clojure.string :refer [split split-lines trim]]
            [clojure.set :refer [union]]))

(def input-lines (load-lines "2021/day10.txt"))

(def test-lines
  (split-lines
   "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

(def chunk-pairs
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def error-scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn valid-chunk?
  ([s] (valid-chunk? s '()))
  ([[c & rest] [need & need-rest :as needed]]
   (let [match (get chunk-pairs c)]
     (cond
       ; when empty string, valid if no remaining needed chars
       (nil? c) (nil? need)
       ; when a closer is detected, add it to the stack
       match (valid-chunk? rest (conj needed match))
       ; when there is no needed closer, this is an invalid chunk
       (nil? need) false
       ; lastly, check that c is the needed closer and that the remaining string is valid
       :else (and (= c need)
                  (valid-chunk? rest need-rest))))))

(defn invalid-chunk-score
  [s]
  (loop [[c & rest] s
         [need & need-rest :as needed] '()]
    (let [match (get chunk-pairs c)]
      (cond
        ; empty string, no errors but possibly incomplete
        (nil? c) 0
        ; when a closer is detected, add it to the stack
        match (recur rest (conj needed match))
        ; lastly, check that c is the needed closer and that the remaining string is valid
        :else (and (= c need)
                   (recur rest need-rest))))))

; valid chunks
(map invalid-chunk-score
     ["([])"
      "{()()()}"
      "<([{}])>"
      "[<>({}){}[([])<>]]"
      "(((((((((())))))))))"])

; invalid chunks
(map invalid-chunk-score
     ["(]"
      "{()()()>"
      "(((()))}"
      "<([]){()}[{}])"])


(valid-chunk? "()")

(defn parse-input
  [lines]
  (map #(split % #"") lines))

(defn part1
  [lines]
  (->> lines
       parse-input))

(println "Part 1 test answer:")
(println (part1 test-lines))

(println "Part 1 answer:")
;(println (part1 input-lines))

(defn part2
  [lines]
  (->> lines
       parse-input))

(println "Part 2 test answer:")
;(println (part2 test-lines))

(println "Part 2 answer:")
;(println (part2 input-lines))
