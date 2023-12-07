(ns aoc.day5
  (:require [clojure.set :as cljset]
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
  ; FIXME: read-string reads as a string from the reader, implying a safe input.
  ; Integer/parseInt was failing to read such big integers
  (map #(read-string %) (re-seq #"\d+" text)))

(defn map-reader [text]
  (let [[destination origin length] (number-reader text)]
    [origin destination length]))

(defn try-read-map-key [text]
  (keyword (first (re-seq #"[a-z\-]+" text))))

(defn set-map-key [parsed map-key]
  (assoc parsed :map-key map-key))

(defn conj-entry [parsed entry]
  (assoc-in parsed
            [:data (:map-key parsed)]
            (conj
             (get-in parsed [:data (:map-key parsed)] [])
             entry)))

(defn read-maps [source]
  (:data (reduce
          (fn [parsed line]
            (let [map-key (try-read-map-key line)]
              (cond
                (= line "") parsed
                (not (nil? map-key)) (set-map-key parsed map-key)
                :else (conj-entry parsed (map-reader line)))))
          {:data {:seeds (number-reader (first source))}}
          (drop 2 source))))

(def map-keys
  [:seed-to-soil
   :soil-to-fertilizer
   :fertilizer-to-water
   :water-to-light
   :light-to-temperature
   :temperature-to-humidity
   :humidity-to-location])

(defn between [item left length]
  (and (<= left item) (>= (+ left length) item)))

(defn get-mapped-value [seed mapping]
  (or
   (first
    (filter (complement nil?)
            (map (fn [[origin destination length]]
                   (if (between seed origin length)
                     (+ destination (- seed origin))
                     nil))
                 mapping)))
   seed))

(defn get-location [seed mappings]
  (println seed)
  (reduce
   (fn [cur-mapped map-key]
     (get-mapped-value cur-mapped (get mappings map-key)))
   seed
   map-keys))

(defn find-min-location [seeds mappings]
  (apply min (map #(get-location % mappings) seeds)))

(defn part-i [source]
  (let [mappings (read-maps source)]
    (find-min-location (:seeds mappings) mappings)))

; (= 35 (part-i data))
; (= 177942185 (part-i lines))

(defn get-ranged-seeds [seed-input]
  (map
   (fn [[seed length]] (range seed (+ seed length)))
   (partition 2 seed-input)))

(defn part-ii [source]
  (let [mappings (read-maps source)
        seed-packs (take 1 (get-ranged-seeds (:seeds mappings)))]
    (reduce
     (fn [cur-min pack]
       (if (nil? cur-min)
         (find-min-location pack mappings)
         (min cur-min (find-min-location pack mappings))))
     nil
     seed-packs)))
