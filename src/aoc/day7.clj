(ns aoc.day7
  (:require [clojure.string :as str]))

(def lines
  (str/split-lines (slurp "resources/day7.txt")))

(def data
  ["32T3K 765"
   "T55J5 684"
   "KK677 28"
   "KTJJT 220"
   "QQQJA 483"])

(def original-card-value
  (into
   {}
   (map-indexed
    (fn [idx card] [card idx])
    [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A])))

(def joker-card-value
  (assoc original-card-value \J -1))

(defn get-cards-values [hand consider-joker?]
  (let [card-value (if consider-joker? joker-card-value original-card-value)]
    (map #(get card-value %) hand)))

(def possible-games
  [[1 '(1 1 1 1 1)]
   [2 '(2 1 1 1)]
   [3 '(2 2 1)]
   [4 '(3 1 1)]
   [5 '(3 2)]
   [6 '(4 1)]
   [7 '(5)]])

(defn get-counts [hand]
  (into {} (map (fn [%] {(first %) (count (second %))})
                (group-by identity (seq hand)))))

(defn get-top-card-no-joker [counts]
  (first
   (first
    (filter
     #(not= \J (first %))
     (reverse (sort-by #(second %) counts))))))

(defn group-cards [hand]
  (let [counts (get-counts hand)]
    {:counts counts
     :top-card (get-top-card-no-joker counts)}))

(defn replace-joker [{counts :counts top-card :top-card}]
  {:top-card top-card
   :counts
   ; all jokers
   (if (nil? top-card)
     counts
     (assoc counts
            \J 0
            top-card (+ (get counts top-card) (get counts \J 0))))})

(defn sorted-counts [counts]
  (filter #(< 0 %) (sort > (map second counts))))

; TODO: use injection for score getter and card values instead of boolean
(defn get-score [hand consider-joker?]
  (let [groups (group-cards hand)
        replaced (if consider-joker? (replace-joker groups) groups)
        counts (sorted-counts (:counts replaced))]
    (first (first (filter #(= counts (second %)) possible-games)))))

; TODO: use injection for score getter and card values instead of boolean
(defn read-input [source consider-joker?]
  (map
   (fn [text]
     (let [[_ hand bid-str] (re-find #"(\w+) (\d+)" text)]
       ; this has to be vector otherwise I cannot use it in the "compare" sort fn
       {:compare (apply vector (concat [(get-score hand consider-joker?)]
                                       (get-cards-values hand consider-joker?)))
        :hand hand
        :bid (Integer/parseInt bid-str)}))
   source))

(defn get-winnings [input]
  (reduce
   +
   (map-indexed
    (fn [idx {bid :bid}] (* (inc idx) bid))
    (sort
     #(compare (:compare %1) (:compare %2))
     input))))

(defn part-i [source]
  (get-winnings (read-input source false)))

(= 6440 (part-i data))
(= 245794640 (part-i lines))

(defn part-ii [source]
  (get-winnings (read-input source true)))

(= 5905 (part-ii data))
(= 247899149 (part-ii lines))
