(ns aoc2020.day3
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split trim]]))

(def sample "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#")

(def day3input
  (->> (io/resource "day3input")
       (slurp)))

(defn parse-terrain [terr]
  (let [lines (split-lines terr)]
    {:terr-lines lines
     :width (count (first lines))
     :height (count lines)}))

(defn part-1 [slope-r slope-d terr]
  (let [pr (parse-terrain terr)]
    (loop [pos {:x 0 :y 0}
           num-trees 0]
      (if (> (:y pos) (:height pr))
        {:num-trees num-trees}
        (let [x (mod (:x pos) (:width pr))
              curt (str (get-in (:terr-lines pr) [(:y pos) x]))
              new-pos {:x (+ (:x pos) slope-r) :y (+ (:y pos) slope-d)}]
          (if (= curt "#")
            (recur new-pos (inc num-trees))
            (recur new-pos num-trees)))))))

(defn part-2 [terr]
  (let [s1 (part-1 1 1 terr)
        s2 (part-1 3 1 terr)
        s3 (part-1 5 1 terr)
        s4 (part-1 7 1 terr)
        s5 (part-1 1 2 terr)
        trees (map #(:num-trees %) [s1 s2 s3 s4 s5])]
    (apply * trees)))