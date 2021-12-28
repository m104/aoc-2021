(ns aoc-clj.day8
  (:require [aoc-clj.core :refer [load-lines str->int combinations
                                  invert-map invert-map-of-sets]]
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
  (mapv #(into #{}
               (map keyword (split % #"")))
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

(def all-connections [:a :b :c :d :e :f :g])
(def all-digits (range 10))

(def connections-by-digit
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

(def digit-by-connections
  (invert-map connections-by-digit))

(def digits-by-connection
  (invert-map-of-sets connections-by-digit))

(def digits-by-connection-count
  (reduce-kv
   (fn [m digit connections]
     (update m (count connections) #(conj (or % #{}) digit)))
   {}
   connections-by-digit))

(defn count-unique-output-digits
  [outputs]
  (count
   (filter
    #(= 1 (count
           (get digits-by-connection-count
                (count %))))
    outputs)))

(defn known-digit?
  [connection-count]
  (= 1 (count
        (get digits-by-connection-count connection-count))))

(defn part1
  [lines]
  (->> lines
       parse-input
       (into [] (comp
                 (map :outputs)
                 cat
                 (map count)
                 (filter known-digit?)))
       count))

(println "Part 1 test answer:")
;(println (part1 test-lines))

(println "Part 1 answer:")
;(println (part1 input-lines))

(defn connection-mappings
  [connections]
  (for [combo (combinations (set connections))]
    (zipmap combo connections)))

(defn outputs->digit
  [outputs mapping]
  (let [mapped (map #(get mapping %) outputs)
        connections (into #{} mapped)]
    (get digit-by-connections connections)))

(defn full-digit-set?
  [digits]
  (let [given-digits (set digits)]
    (= given-digits (set all-digits))))

(defn full-connection-mapping
  [patterns]
  (some
   (fn [mapping]
     (let [digit-set
           (reduce
            (fn [s outputs]
              (let [digit (outputs->digit outputs mapping)]
                (if digit (conj s digit) s)))
            #{}
            patterns)]
       (when (full-digit-set? digit-set)
         mapping)))
   (connection-mappings all-connections)))

(defn digits->number
  [digits]
  (reduce (fn [n d] (+ d (* 10 n))) 0 digits))

(defn process-digits
  [{:keys [patterns outputs]}]
  (let [mapping (full-connection-mapping patterns)
        digits (map #(outputs->digit % mapping) outputs)]
    {:digits digits
     :number (digits->number digits)
     :mapping mapping}))

(defn part2
  [lines]
  (reduce + 0
          (->> lines
               parse-input
               (map process-digits)
               (map :number))))

(println "Part 2 test answer:")
;(println (part2 test-lines))

(println "Part 2 answer:")
;(println (part2 input-lines))
