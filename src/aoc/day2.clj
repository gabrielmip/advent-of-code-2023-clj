(ns aoc.day2
  (:require [clojure.string :as str]))

(def lines
  (str/split-lines (slurp "resources/day2.txt")))

(defn get-id [text]
  (Integer/parseInt (last (re-find #"Game (\d+)" text))))

(def qtds
  {"red" 12
   "green" 13
   "blue" 14})

(defn valid? [game]
  (every?
   (fn [[_ qtd-str color]]
     (>= (get qtds color) (Integer/parseInt qtd-str)))
   (re-seq #"(\d+) (red|blue|green)" game)))

; part i
(defn valid-game-ids [games]
  (filter
   (complement nil?)
   (map
    #(if (valid? %) (get-id %) nil)
    games)))

(reduce + (valid-game-ids lines)) ; => 2486

; part ii
(defn game-mins [game]
  (reduce
   (fn [agg [_ qtd-str color]]
     (assoc agg
            color
            (max (Integer/parseInt qtd-str) (get agg color 0))))
   {}
   (re-seq #"(\d+) (red|blue|green)" game)))

(reduce
 +
 (map
  #(apply * (vals (game-mins %)))
  lines)) ; => 87984
