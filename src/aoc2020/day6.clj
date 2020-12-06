(ns aoc2020.day6
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split includes?]]))

(def sample1 "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

(defn parse-groups [input] (split input #"\n\n"))

(defn parse-group [group] (split-lines group))

(def day6input
  (->> (io/resource "day6input")
       (slurp)))

(defn count-qas [group-answers]
  (let [ans (flatten (map #(seq %) group-answers))]
    (count (distinct ans))))

(defn sum-qas [input]
  (let [groups (parse-groups input)]
    (apply + (map (fn [g] (count-qas (parse-group g))) groups))))

(defn part1 []
  (sum-qas day6input))

(defn find-common-qs [group]
  (let [ans (into [](map #(set %) group))
        common (apply clojure.set/intersection ans)]
    (count common)))

(defn sum-common-qs [input]
  (let [groups (parse-groups input)]
    (apply + (map (fn [g] (find-common-qs (parse-group g))) groups))))

(defn part2 []
  (sum-common-qs day6input))