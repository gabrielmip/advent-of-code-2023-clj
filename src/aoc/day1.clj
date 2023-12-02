(ns aoc.day1
  (:require [clojure.string :as str]))

(def lines
  (str/split (slurp "resources/day1.txt") #"\n"))

(defn first-last-num [text]
  (reduce (fn [acc cur]
            (if (Character/isDigit cur)
              (if (nil? (first acc))
                [cur cur]
                [(first acc) cur])
              acc))
          [nil nil]
          (seq text)))

(defn sum-all [lines proc]
  (reduce +
          0
          (map (fn [x]
                 (Integer/parseInt
                  (apply str (first-last-num (proc x)))))
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
