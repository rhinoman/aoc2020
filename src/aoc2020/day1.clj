(ns aoc2020.day1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]))

(def sample1 [1721
              979
              366
              299
              675
              1456])

(def day1input
  (->> (io/resource "day1input")
       (slurp)
       (split-lines)
       (map #(Integer/parseInt %))
       (into [])))

(defn find2Sum [xs* sum]
  (let [xs (take-while #(< % sum) (sort xs*))]
    (loop [x (first xs)
           ys (rest xs)]
      (if (empty? ys)
        nil
        (let [z (- sum x)]
          (if (some?(some #{z} ys))
            [x z]
            (recur (first ys) (rest ys))))))))

(defn check-triple [x ys sum]
  (let [z (- sum x)
        dbl (find2Sum ys z)]
    (if (some? dbl)
      (conj dbl x)
      nil)))

(defn find3Sum [xs* sum]
  (let [xs (take-while #(< % sum) (sort xs*))]
    (loop [x (first xs)
           ys (rest xs)]
      (let [triple (check-triple x ys sum)]
        (if (some? triple)
          triple
          (recur (first ys) (rest ys)))))))