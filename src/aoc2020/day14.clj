(ns aoc2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]
            [clojure.math.combinatorics :as combo]))



(defn parse-command [cstr]
  (let [mv (split cstr #" = ")]
    {:mem_addr (Long/parseLong (apply str (filter #(Character/isDigit %) (first mv))))
     :value (Long/parseLong (second mv))}))

(defn parse-program [input]
  (let [sections (rest (split input #"mask = "))
        psections (map split-lines sections)]
    (map (fn [p] {:mask (first p)
                  :commands (map parse-command (rest p))})
         psections)))

(def sample "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0\n")

(def sample2 "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1\n")

(def pad (join (map (fn [_] "0") (range 36))))

(def day14input
  (->> (io/resource "day14input")
       (slurp)))

(def memory (atom {}))

(defn apply-mask [mask value]
  (apply
    str
    (map-indexed
      (fn [idx v]
        (case (get mask idx)
          \X v
          \0 0
          \1 1))
      value)))

(defn apply-mask-2 [mask addr]
  (apply
    str
    (map-indexed
      (fn [idx v]
        (case (get mask idx)
          \1 1
          \0 v
          \A 0))
      addr)))

(defn apply-masks-2 [masks addr]
  (map #(apply-mask-2 % addr) masks))

(defn gen-mask [mask poss]
  (loop [i 0
         pn 0
         acc []]
    (if (>= i (count mask))
      (apply str acc)
      (case (get mask i)
        \0 (recur (inc i) pn (conj acc \0))
        \1 (recur (inc i) pn (conj acc \1))
        \X (recur (inc i) (inc pn) (conj acc (get (into [] poss) pn)))))))

(defn gen-masks [mask]
  (let [numx (count (filter #(= % \X) mask))
        poss (combo/selections [\1 \A] numx)]
    (map #(gen-mask mask %) poss)))

(defn gen-addrs [masks addr]
  (let [bin-string* (Long/toBinaryString addr)
        lbs (count bin-string*)
        bin-string (apply str (concat (take (- 36 lbs) pad) bin-string*))]
    (apply-masks-2 masks bin-string)))

(defn execute-command [mask command]
  (let [bin-string* (Long/toBinaryString (:value command))
        lbs (count bin-string*)
        bin-string (apply str (concat (take (- 36 lbs) pad) bin-string*))
        memaddr (:mem_addr command)
        masked-value (Long/parseLong (apply-mask mask bin-string) 2)]
    (swap! memory assoc memaddr masked-value)))

(defn execute-command-2 [masks command]
  (let [memaddr (:mem_addr command)
        faddrs (gen-addrs masks memaddr)]
    (doseq [addr faddrs] (swap! memory assoc addr (:value command)))))

(defn ex-section [section]
  (let [mask (:mask section)
        commands (:commands section)]
    (doseq [cmd commands] (execute-command mask cmd))))

(defn ex-section-2 [section]
  (let [masks (gen-masks (:mask section))
        commands (:commands section)]
    (println "EXEC SECTION")
    (doseq [cmd commands] (execute-command-2 masks cmd))))

(defn solve-part1 [input]
  (reset! memory {})
  (let [program (parse-program input)]
    (clojure.pprint/pprint @memory)
    (doseq [section program] (ex-section section))
    (println (str "SUM: " (apply + (map (fn [m] (second m)) @memory))))))

(defn solve-part2 [input]
  (reset! memory {})
  (let [program (parse-program input)]
    (doseq [section program] (ex-section-2 section))
    (println (str "SUM: " (apply + (map (fn [m] (second m)) @memory))))))