(ns aoc2020.day5
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split includes?]]))

(def sample1 "FBFBBFFRLR")

(def boarding-passes
  (->> (io/resource "day5input")
       (slurp)
       (split-lines)))

(defn decode-row [rowb]
  (loop [cur (first rowb)
         rs (rest rowb)
         range {:min 0 :max 127}]
    (if (nil? cur)
      (:min range)
      (let [nrange (if (= cur \F)
                     {:min (:min range) :max (quot (+ (:max range) (:min range)) 2)}
                     {:min (+ (quot (+ (:max range) (:min range)) 2) 1) :max (:max range)})]
        (recur (first rs) (rest rs) nrange)))))

(defn decode-col [colb]
  (loop [cur (first colb)
         rs (rest colb)
         range {:min 0 :max 7}]
    (if (nil? cur)
      (:min range)
      (let [nrange (if (= cur \L)
                     {:min (:min range) :max (quot (+ (:max range) (:min range)) 2)}
                     {:min (+ (quot (+ (:max range) (:min range)) 2) 1) :max (:max range)})]
        (recur (first rs) (rest rs) nrange)))))

(defn decode-bp [bp]
  (let [rowb (subs bp 0 7)
        colb (subs bp 7 10)]
    (let [row (decode-row rowb)
          col (decode-col colb)]
      {:row row
       :column col
       :id (+ (* row 8) col)})))

(defn part1 []
  (let [ids (map #(:id (decode-bp %)) boarding-passes)]
    (apply max ids)))

(defn part2 []
  (let [ids (sort (map #(:id (decode-bp %)) boarding-passes))
        all-ids (into #{} (range 0 1023))
        idset (into #{} ids)]
    (sort (clojure.set/difference all-ids idset))))