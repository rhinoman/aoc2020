(ns aoc2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split includes?]]))

(defn parse-passports [pl]
  (map #(split % #"\n| ") pl))

(def passports
  (-> (io/resource "day4input")
      (slurp)
      (split #"\n\n")
      (parse-passports)))

(defn parse-passport-fields [passport]
  (let [;fields (split passport #" ")
        pairs (map #(split % #":") passport)]
    (into (sorted-map) pairs)))

(defn validate-passport-1 [passport]
  (let [keys (parse-passport-fields passport)]
    (println keys)
    (and
      (contains? keys "byr")
      (contains? keys "iyr")
      (contains? keys "eyr")
      (contains? keys "hgt")
      (contains? keys "hcl")
      (contains? keys "ecl")
      (contains? keys "pid"))))

(defn in-range [num min max]
  (let [numi (Integer/parseInt num)]
    (and (>= numi min) (<= numi max))))

(defn height [height]
  (let [y (apply str (drop-last 2 height))]
    (cond
      (includes? height "cm") (in-range y 150 193)
      (includes? height "in") (in-range y 59 76)
      :else false)))

(defn eye-color [ecl]
  (or
    (= ecl "amb")
    (= ecl "blu")
    (= ecl "brn")
    (= ecl "gry")
    (= ecl "grn")
    (= ecl "hzl")
    (= ecl "oth")))

(defn hair-color [hcl]
  (re-matches #"^#([A-Fa-f0-9]{6})" hcl))

(defn validate-passowrd-2 [passport]
  (let [keys (parse-passport-fields passport)]
    (println keys)
    (try
       (and
         (in-range (get keys "byr") 1920 2002)
         (in-range (get keys "iyr") 2010 2020)
         (in-range (get keys "eyr") 2020 2030)
         (height (get keys "hgt"))
         (hair-color (get keys "hcl"))
         (eye-color (get keys "ecl"))
         (= (count (get keys "pid")) 9))
       (catch Exception e
         false
         ))))

(defn solve-1 []
  (let [vresults (map validate-passport-1 passports)]
    (count (filter true? vresults))))

(defn solve-2 []
  (let [vresults (map validate-passowrd-2 passports)]
    (count (filter true? vresults))))