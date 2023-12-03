(ns aoc.day3
  (:require [clojure.string :as str]))

(def lines
  (str/split-lines (slurp "resources/day3.txt")))

(def data
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(re-find #"^\d+" (first data))

(defn walk-assoc [agg group-key word]
  (assoc agg
         :index (+ (:index agg) (count word))
         group-key (conj (get agg group-key)
                         {:text word
                          :start (:index agg)
                          :end (+ -1 (:index agg) (count word))})))

(defn find-words [text]
  (loop [agg {:index 0
              :numbers []
              :symbols []}]
    (if (>= (:index agg) (count text))

      (select-keys agg [:numbers :symbols])

      (let [substring (subs text (:index agg))
            number (re-find #"^\d+" substring)
            symb (re-find #"^[^\w\.]" substring)]
        (recur
         (cond
           (not (nil? number)) (walk-assoc agg :numbers number)
           (not (nil? symb)) (walk-assoc agg :symbols symb)
           :else (assoc agg :index (+ 1 (:index agg)))))))))

; part i
(defn find-part-numbers [line pre post]
  (filter
   (fn [word]
     (let [left (dec (:start word))
           right (inc (:end word))]
       (some
        (fn [{index :start}]
          (and (>= index left)
               (<= index right)))
        (apply concat (map :symbols [line pre post])))))
   (:numbers line)))

(let [data-words (map find-words lines)]
  (reduce
   +
   0
   (map
    #(Integer/parseInt (:text %))
    (apply
     concat
     (filter
      (complement empty?)
      (map-indexed
       (fn [idx words]
         (find-part-numbers
          words
          (nth data-words (dec idx) nil)
          (nth data-words (inc idx) nil)))
       data-words)))))) ; => 528819

; part ii
(defn find-gears [line pre post]
  (filter
   #(= 2 (count %))
   (map
    (fn [symb]
      (if-not (= (:text symb) "*")
        (list)
        (filter
         (fn [number]
           (let [left (dec (:start number))
                 right (inc (:end number))
                 index (:start symb)]
             (and (>= index left)
                  (<= index right))))
         (apply concat (map :numbers [line pre post])))))
    (:symbols line))))

(let [data-words (map find-words lines)]
  (reduce
   +
   0
   (map
    (fn [[left right]]
      (* (Integer/parseInt (:text right))
         (Integer/parseInt (:text left))))
    (apply
     concat
     (filter
      (complement empty?)
      (map-indexed
       (fn [idx line]
         (find-gears
          line
          (nth data-words (dec idx) nil)
          (nth data-words (inc idx) nil)))
       data-words)))))) ; => 80403602
