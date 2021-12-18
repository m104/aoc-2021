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
