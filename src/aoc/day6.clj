(ns aoc.day6
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(def lines
  (str/split-lines (slurp "resources/day6.txt")))

(def data
  ["Time:      7  15   30"
   "Distance:  9  40  200"])

(defn read-as-separated [text]
  (map #(Integer/parseInt %) (re-seq #"\d+" text)))

(defn read-as-joined [text]
  ; FIXME: read-string reads as a string from the reader, implying a safe input.
  ; Integer/parseInt was failing to read such big integers
  [(read-string (apply str (re-seq #"\d+" text)))])

(defn read-data [source proc]
  (let [[times distances] (map proc source)]
    (map vector times distances)))

(defn count-ways [[t d]]
  (inc
   (-
    (int (math/ceil (dec (/ (+ t (math/sqrt (- (* t t) (* 4 d)))) 2))))
    (int (math/floor (inc (/ (- t (math/sqrt (- (* t t) (* 4 d)))) 2)))))))

(defn part-i [source]
  (reduce
   *
   (map
    count-ways
    (read-data source read-as-separated))))

(= 288 (part-i data))
(= 227850 (part-i lines))

(defn part-ii [source]
  (count-ways (first (read-data source read-as-joined))))

(= 71503 (part-ii data))
(= 42948149 (part-ii lines))
