(ns aoc2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split trim]]))

(def sample
  "1-3 a: abcde
  1-3 b: cdefg
  2-9 c: ccccccccc")

(def sample2 "1-3 a: abcde")
(def sample3 "1-3 b: cdefg")
(def sample4 "2-9 c: ccccccccc")

(defn parse-password [pstr]
  (let [segments (split pstr #" ")
        minmax (split (get segments 0) #"-")
        pchar (first (split (get segments 1) #":"))
        pass (last segments)]
    {:min (Integer/parseInt (get minmax 0))
     :max (Integer/parseInt (get minmax 1))
     :char pchar
     :pass pass}))

(defn parse-password-list [ps]
  (map trim (split-lines ps)))

(defn validate [pass-str]
  (let [pass (parse-password pass-str)
        numchar (count (filter #(= % (:char pass)) (split (:pass pass) #"")))]
    (and (<= numchar (:max pass)) (>= numchar (:min pass)))))

(defn validate2 [pass-str]
  (let [pass (parse-password pass-str)
        parr (split (:pass pass) #"")
        min-contains (= (:char pass) (get parr (- (:min pass) 1)))
        max-contains (= (:char pass) (get parr (- (:max pass) 1)))]
    (and (not (and min-contains max-contains))
         (or min-contains max-contains))))

(def day2input
  (->> (io/resource "day2input")
       (slurp)
       (parse-password-list)))

(defn part1 []
  (count (filter true? (map validate day2input))))

(defn part2 []
  (count (filter true? (map validate2 day2input))))
