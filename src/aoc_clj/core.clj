(ns aoc-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn load-lines
  [path]
  (->> path
       io/resource
       slurp
       split-lines))

(defn str->int [val] (Integer/parseInt val))

(defn invert-map
  [m]
  (reduce-kv
   (fn [m k v]
     (assoc m v k))
   {}
   m))

(defn invert-map-of-sets
  [m]
  (reduce
   (fn [m [k v]] (update m k
                         #(conj (or % #{}) v)))
   {}
   (for [[k s] m
         v s]
     [v k])))

(defn combinations
  [elements]
  (cond
    (empty? elements)      []
    (= 1 (count elements)) [[(first elements)]]
    :else                  (for [e elements
                                 combo (combinations (disj elements e))]
                             (conj combo e))))
