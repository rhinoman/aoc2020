(ns aoc2020.day7
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]))


(def day7input
  (->> (io/resource "day7input")
       (slurp)))

(def sample "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")


(defn parse-rule [rule]
  (let [rule* (clojure.string/replace rule "contain" ",")
        sections (into [] (map #(split % #" ") (map trim (split rule* #","))))
        rbag (into [] (take 2 (first sections)))]
    (if (= "no" (first (second sections)))
                 nil
                 (into {} (map (fn [s] [(str (get s 1) " " (get s 2)) (Integer/parseInt (first s))])
                               (drop 1 sections))))))

(defn parse-rules [input]
  (into {}
        (map (fn [r]
               (let [fc (take 2 (split r #" "))
                     this-col (str (first fc) " " (second fc))]
                 [this-col (parse-rule r)]))
             (split-lines input))))

(def part1-count (atom 0))
(def colors-examined (atom {}))

(defn rule-contains? [rule color]
  (let [cont (filter #(= color (:color %)) (:contains rule))]
    (seq cont)))

(defn color-ex [rule colors-ex]
  (let [co (filter #(not (contains? colors-ex (:color %))) (:contains rule))]
    (seq co)))

(defn solve-1** [cc sc rule rules acc]
  (if (nil? rule)
    acc
    (reduce (fn [accb c]
           (let [nr (first (filter #(= (first %) (first c)) rules))
                 nacc (if (= (first c) sc) (inc accb) accb)]
             (solve-1** (first c) sc nr rules nacc))) acc (second rule))))

(defn solve-1* [rules color]
  (reduce (fn [acc r]
            (let [rs (if (> (solve-1** (first r) color r rules 0) 0) 1 0)]
              (println (str (first r) ": " rs))
              (+ acc rs))) 0 rules))

(defn solve-1 [input color]
  (println (solve-1* (parse-rules input) color)))

(def sample2 "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.\n")

(defn solve-2* [sr rules acc]
  (if (nil? sr)
    acc
    (reduce (fn [accb b]
              (let [nb (second b)
                    nr (first (filter #(= (first %) (first b)) rules))]
                (+ accb (* nb (solve-2* nr rules accb))))) 1 (second sr))))

(defn solve-2 [input color]
  (let [rules (parse-rules input)
        start-rule (first (filter #(= (first %) color) rules))]
    (dec (solve-2* start-rule rules 1))))

