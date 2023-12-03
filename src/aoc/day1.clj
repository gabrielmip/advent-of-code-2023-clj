(ns aoc.day1
  (:require [clojure.string :as str]))

(def lines
  (str/split-lines (slurp "resources/day1.txt")))

(defn num-from-first-last [text]
  (let [digits (re-seq #"\d" text)]
    (Integer/parseInt
     (str
      (first digits) (last digits)))))

(defn sum-all [lines proc]
  (reduce +
          0
          (map #(num-from-first-last (proc %))
               lines)))

; first
(sum-all lines identity)

; second
(def replacemts
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def replace-regex (re-pattern (str/join "|" (keys replacemts))))

(sum-all lines #(str/replace % replace-regex replacemts))
