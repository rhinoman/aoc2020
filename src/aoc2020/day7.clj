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
    {:rbag (join " " rbag)
     :contains (if (= "no" (first (second sections)))
                 nil
                 (into [] (map (fn [s] {:num (Integer/parseInt (first s))
                                        :color (str (get s 1) " " (get s 2))}) (drop 1 sections))))}))

(defn parse-rules [input]
  (map parse-rule (split-lines input)))

(def part1-count (atom 0))
(def colors-examined (atom {}))

(defn rule-contains? [rule color]
  (let [cont (filter #(= color (:color %)) (:contains rule))]
    (seq cont)))

(defn color-ex [rule colors-ex]
  (let [co (filter #(not (contains? colors-ex (:color %))) (:contains rule))]
    (seq co)))

(defn solve-1* [rules color]
  ;(clojure.pprint/pprint rules)
  (println @colors-examined)
  (if (contains? @colors-examined color)
    (println "NOPE")
    (let [;direct (filter (fn [r] (= color (:rbag r))) rules)
          n (filter #(color-ex % @colors-examined) rules)
          n1 (filter #(rule-contains? % color) n)]
      ;(println (count direct))
      ;(println (count n1))
      (swap! colors-examined #(assoc % color true))
      (swap! part1-count #(+ (count n1) %))
      (if (> (count n1) 0)
        (seq (map (fn [n2] (solve-1* rules (:rbag n2))) n1))))))

(defn solve-1 [input color]
  (reset! part1-count 0)
  (reset! colors-examined {})
  (solve-1* (parse-rules input) color)
  (println @part1-count))