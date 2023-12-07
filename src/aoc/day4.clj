(ns aoc.day4
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(def lines
  (str/split-lines (slurp "resources/day4.txt")))

(def data
  ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(defn read-card-line [text]
  (map
   (fn [game]
     (map
      (fn [digit] (Integer/parseInt digit))
      (re-seq #"\d+" game)))
   (str/split
    (last (str/split text #":"))
    #"\|")))

(defn count-matches [[winning game]]
  (count (filter #(contains? game %) winning)))

(defn score [[winning game]]
  (let [matches (count-matches [winning game])]
    (if (> matches 0)
      (int (math/pow 2 (- matches 1)))
      0)))

(defn part-i [source]
  (reduce + 0 (map #(score (read-card-line %)) source)))

; (= 13 (part-i data))
; (= 21919 (part-i lines))

(defn give-copies [counter card matches copies]
  (reduce
   (fn [cur-counter cur-card]
     (assoc cur-counter cur-card
            ; not found is 1 since we always have the original
            (+ copies (get cur-counter cur-card 1))))
   counter
   (range (inc card) (+ 1 card matches))))

(defn ensure-original [agg card]
  (if (contains? agg card)
    agg
    (assoc agg card 1)))

(defn count-copies [source]
  (reduce
   (fn [counter [card matches]]
     (let [with-original (ensure-original counter card)]
       (give-copies with-original
                    card
                    matches
                    (get with-original card))))
   {}
   (map-indexed
    (fn [card text] [(inc card) (count-matches (read-card-line text))])
    source)))

(defn part-ii [source]
  (reduce + 0 (vals (count-copies source))))

; (= 30 (part-ii data))
; (= 9881048 (part-ii lines))
