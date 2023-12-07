(ns aoc.core
 (:gen-class)
 (:require [aoc.day5 :as day5] ))

(defn -main
  []
  (println ["result" (day5/part-ii day5/lines)]))
