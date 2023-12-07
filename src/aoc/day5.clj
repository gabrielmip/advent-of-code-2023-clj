(ns aoc.day5
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(def data
  ["seeds: 79 14 55 13"
   ""
   "seed-to-soil map:"
   "50 98 2"
   "52 50 48"
   ""
   "soil-to-fertilizer map:"
   "0 15 37"
   "37 52 2"
   "39 0 15"
   ""
   "fertilizer-to-water map:"
   "49 53 8"
   "0 11 42"
   "42 0 7"
   "57 7 4"
   ""
   "water-to-light map:"
   "88 18 7"
   "18 25 70"
   ""
   "light-to-temperature map:"
   "45 77 23"
   "81 45 19"
   "68 64 13"
   ""
   "temperature-to-humidity map:"
   "0 69 1"
   "1 0 69"
   ""
   "humidity-to-location map:"
   "60 56 37"
   "56 93 4"])

(def lines
  (str/split-lines (slurp "resources/day5.txt")))

(defn number-reader [text]
  (map #(Integer/parseInt %) (re-seq #"\d+" text)))

(defn map-reader [text]
  (let [[destination origin length] (number-reader text)]
    [origin destination (- destination origin) length]))

(def maps [:seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light :light-to-temperature :temperature-to-humidity :humidity-to-location])

(defn try-read-mapper [text]
  (keyword (first (re-seq #"[a-z\-]+" text))))

(defn set-mapper [parsed mapper]
  (assoc parsed :mapper mapper))

(defn conj-entry [parsed entry]
  (assoc-in parsed
            [:data (:mapper parsed)]
            (conj
             (get-in parsed [:data (:mapper parsed)] [])
             entry)))

(defn read-maps [source]
  (:data (reduce
          (fn [parsed line]
            (let [mapper (try-read-mapper line)]
              (cond
                (= line "") parsed
                (not (nil? mapper)) (set-mapper parsed mapper)
                :else (conj-entry parsed (map-reader line)))))
          {:data {:seeds (number-reader (first source))}}
          (drop 2 source))))

(read-maps data)
