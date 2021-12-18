(ns aoc-clj.day4
  (:require [aoc-clj.core :refer [load-lines str->int]]
            [clojure.string :refer [split trim]]))

(def lines (load-lines "2021/day4.txt"))

(def part1-test-items [])
(def part2-test-items part1-test-items)

(def board-size 5)

(defn coord->idx
  [x y]
  (+ x (* y board-size)))

(defn idx->coord
  [idx]
  (let [x (rem idx board-size)
        y (quot idx board-size)]
    (list x y)))

(def winning-combinations
  ;"Seqences of (x, y) coordinates for winning combinations."
  (let [dimension (range board-size)]
    (concat
     ; horizontals
     (map (fn [y] (map #(coord->idx % y) dimension)) dimension)
     ; verticals
     (map (fn [x] (map #(coord->idx x %) dimension)) dimension)
     ; diagonal NW -> SE
     #_(list (map #(coord->idx % %) dimension))
     ; diagonal SW -> NE
     #_(list (map #(coord->idx % (- (dec board-size) %)) dimension))
     ;
     )))

(defn parse-draws
  [line]
  (mapv str->int (split line #",")))

(defn parse-board-line
  [line]
  (mapv str->int (split (trim line) #" +")))

(defn parse-board-squares
  [lines]
  (assert (= board-size (count lines)))
  (let [rows (mapv parse-board-line lines)
        cells (into [] (flatten rows))]
    (assert (= (* board-size board-size)
               (count cells)))
    cells))

(defn make-board
  [squares]
  (into []
        (for [y (range board-size)
              x (range board-size)]
          {;:x x
           ;:y y
           :num (nth squares (coord->idx x y))
           :drawn false})))

(defn parse-game-input
  [lines]
  (let [draws (parse-draws (first lines))
        board-lines (subvec lines 2)
        lines-for-boards (partition board-size (inc board-size) board-lines)
        squares-for-boards (map parse-board-squares lines-for-boards)]
    {:draws draws
     :boards (mapv make-board squares-for-boards)}))

(defn winning-board?
  [board]
  (some
   (fn [combo]
     (every? #(:drawn (nth board %)) combo))
   winning-combinations))

(defn apply-draw
  [number board]
  (mapv (fn [square]
          (if (= number (:num square))
            (assoc square :drawn true)
            square))
        board))

(defn find-winner
  [{:keys [draws boards]}]
  (loop [[draw & remaining] draws
         boards boards]
    (println "Drawing" draw)
    (let [boards (mapv #(apply-draw draw %) boards)
          winning-board (some #(and (winning-board? %) %) boards)]
      (if (or winning-board
              (nil? draw))
        {:last-draw draw :board winning-board}
        (recur remaining boards)))))

(defn sum-unmarked
  [board]
  (->> board
       (filter (complement :drawn))
       (map :num)
       (reduce +)))

(def game (parse-game-input lines))

(defn print-board
  [board]
  (println "Board:")
  (doseq [y (range board-size)]
    (doseq [x (range board-size)]
      (let [square (nth board (coord->idx x y))
            {:keys [num drawn]} square]
        (if drawn
          (print (str "*" num " "))
          (print (str " " num " ")))))
    (println))
  (println ""))

(defn part1
  [game]
  (let [{:keys [board last-draw]} (find-winner game)]
    (when board
      (print-board board)
      {:answer (* last-draw (sum-unmarked board))
       :last-draw last-draw
       :picked (mapv :num (filter :drawn board))
       :unpicked (mapv :num (filter (complement :drawn) board))
       :board board})))

;; (println "Part 1 test answer:")
;; (println (part1 part1-test-items))


(println "Part 1 answer:")
(println (part1 game))

(defn find-last-winner
  [{:keys [draws boards]}]
  (loop [[draw & remaining] draws
         boards boards]
    (println "> Drawing" draw)
    (println "Boards left" (count boards))
    (let [boards (mapv #(apply-draw draw %) boards)
          grouped-boards (group-by #(boolean (winning-board? %)) boards)
          winners (get grouped-boards true [])
          non-winners (get grouped-boards false [])]
      (println "Winners" (count winners))
      (doseq [board winners]
        (print-board board))
      (println "Non-winners" (count non-winners))
      (if (or (nil? draw)
              (and (= 1 (count winners))
                   (empty? non-winners)))
        {:last-draw draw :board (first winners)}
        (recur remaining non-winners)))))

(defn part2
  [game]
  (let [{:keys [board last-draw]} (find-last-winner game)]
    (when board
      {:answer (* last-draw (sum-unmarked board))
       :last-draw last-draw
       :picked (mapv :num (filter :drawn board))
       :unpicked (mapv :num (filter (complement :drawn) board))
       :board board})))

(println "Part 2 test answer:")
;(println (part2 part2-test-items))

(println "Part 2 answer:")
(println (part2 game))
