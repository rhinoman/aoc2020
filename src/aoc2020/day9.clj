(ns aoc2020.day9
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]
            [clojure.math.combinatorics :refer [combinations subsets]]))


(def day9input
  (->> (io/resource "day9input")
       (slurp)
       (split-lines)
       (map #(Long/parseLong %))
       (into [])))

(def sample1
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn solve-part1 [data plength]
  (loop [idx plength]
    (let [x (get data idx)
          px (take plength (drop (- idx plength) data))
          psums (map #(+ (first %) (second %)) (combinations px 2))]
      (if (not (some #(= % x) psums))
        x
        (recur (inc idx))))))

(defn attempt-sum [data goal]
  (loop [start-idx 0]
    ))

(defn find-sum-set [data plength]
  (let [goal (solve-part1 data plength)]
    (loop [numt 1
           xs data]
      (let [cs (take numt xs)
            sum (apply + cs)]
        (cond
          (= sum goal) cs
          (> sum goal) (recur 1 (rest xs))
          :else (recur (inc numt) xs))))))

(defn solve-part2 [data plength]
  (let [sumset (sort (find-sum-set data plength))]
    (+ (first sumset) (last sumset))))