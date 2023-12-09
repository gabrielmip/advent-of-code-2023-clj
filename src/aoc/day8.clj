(ns aoc.day8
  (:require [clojure.string :as str]))

(def lines
  (str/split-lines (slurp "resources/day8.txt")))

(def data
  ["LLR"
   ""
   "AAA = (BBB, BBB)"
   "BBB = (AAA, ZZZ)"
   "ZZZ = (ZZZ, ZZZ)"])

(def ghost-town
  ["LR"
   ""
   "11A = (11B, XXX)"
   "11B = (XXX, 11Z)"
   "11Z = (11B, XXX)"
   "22A = (22B, XXX)"
   "22B = (22C, 22C)"
   "22C = (22Z, 22Z)"
   "22Z = (22B, 22B)"
   "XXX = (XXX, XXX)"])

(defn read-directions [source]
  (re-seq #"\w" (first source)))

(defn read-tree [source]
  (into
   {}
   (map (fn [line]
          (let [[_ origin left right]
                (first (re-seq #"(\w+) = \((\w+), (\w+)\)" line))]
            [origin {"L" left "R" right}]))
        (drop 2 source))))

(defn count-steps [tree directions start finish?]
  (loop [current start
         steps 0
         instructions (cycle directions)]
    (if (finish? current)
      steps
      (recur (get-in tree [current (first instructions)])
             (inc steps)
             (rest instructions)))))

(defn part-i [source]
  (let [directions (read-directions source)
        tree (read-tree source)]
    (count-steps tree directions "AAA" #(= % "ZZZ"))))

(= 6 (part-i data))
(= 16579 (part-i lines))

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))
(defn lcm [v] (reduce #(/ (* %1 %2) (gcd %1 %2)) v))

(defn part-ii [source]
  (let [directions (read-directions source)
        tree (read-tree source)]
    (lcm
     (map (fn [start]
            (count-steps tree directions start #(str/ends-with? % "Z")))
          (filter #(str/ends-with? % "A") (keys tree))))))

(= 6 (part-ii ghost-town))
(= 12927600769609 (part-ii lines))
