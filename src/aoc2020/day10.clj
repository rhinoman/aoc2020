(ns aoc2020.day10
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]
            [clojure.math.combinatorics :refer [count-permutations combinations subsets]]))

(def day10input
  (->> (io/resource "day10input")
       (slurp)
       (split-lines)
       (into [] (map #(Integer/parseInt %)))))

(def sample1 [16 10 15 5 1 11 7 19 6 12 4])

(def sample2 [28
              33
              18
              42
              31
              14
              46
              20
              48
              47
              24
              23
              49
              45
              19
              38
              39
              11
              1
              32
              25
              35
              8
              17
              7
              9
              4
              2
              34
              10
              3])

(defn calc-diffs [adapters]
  (loop [prev 0
         a (first adapters)
         as (rest adapters)
         acc []]
    (if (nil? a)
      (conj acc 3)
      (recur a (first as) (rest as) (conj acc (- a prev))))))

(defn solve-part-1 [input]
  (let [adiffs (calc-diffs (sort input))
        ones (count (filter #(= 1 %) adiffs))
        threes (count (filter #(= 3 %) adiffs))]
    (println (str ones ":" threes))
    (* ones threes)))

(def memo (atom {}))

(defn solve-part-2* [idx input]
  (cond (some? (get @memo idx)) (get @memo idx)
        (= idx (dec (count input))) 1
        :else
        (loop [i (inc idx)
               sum 0]
          (cond
            (>= i (count input)) sum
            (<= (- (get input i) (get input idx)) 3)
              (let [nums (solve-part-2* i input)]
                (swap! memo assoc i nums)
                (recur (inc i) (+ sum nums)))
            :else sum))))

(defn solve-part-2 [input]
  (reset! memo {})
  (println (str "ALL:" (solve-part-2* 0 (into [] (sort (conj input 0)))))))
