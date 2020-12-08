(ns aoc2020.day8
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]))

(def pc (atom 0))
(def acv (atom 0))

(def sample "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n")

(def day8input
  (->> (io/resource "day8input")
       (slurp)))

(defn parse-instruction [idx instruction]
  (let [lex (split instruction #" ")]
    [idx {:op (first lex)
          :params (into [] (rest lex))}]))

(defn parse-program [input]
  (let [lines (split-lines input)]
    (into {} (map-indexed (fn [idx in] (parse-instruction idx in)) lines))))

(defn nx []
  (swap! pc inc))

(defn psi
  "Parsed a sign integer string"
  [x]
  (let [sign (first x)
        num (Integer/parseInt (subs x 1))]
    (if (= \- sign) (- num) num)))

(defn jmp [params]
  (let [jp (psi (first params))]
    (swap! pc #(+ % jp))))

(defn acc [params]
  (let [values (into [] (map psi params))]
    (swap! acv #(apply + % values))
    (nx)))

(defn nop [params] (nx))

(defn run-program [program]
  (reset! pc 0)
  (reset! acv 0)
  (loop [executed {}]
    (let [cur-op (get program @pc)]
      (cond (nil? cur-op) 0
            (contains? executed @pc) -1
            :else (let [inst (resolve (symbol (:op cur-op)))
                        n-ex (assoc executed @pc cur-op)]
                    (inst (:params cur-op))
                    (recur n-ex))))))

(defn solve-part1 [input]
  (let [program (parse-program input)
        ret-val (run-program program)]
    (println (str "EXIT CODE " ret-val))
    (println (str "Final PC value: " @pc))
    (println (str "Final ACC Value: " @acv))))

(defn filter-opcode [program op]
  (reduce-kv (fn [m k v] (if (= (:op v) op) (assoc m k v) m)) {} program))

(defn gen-jmp-to-nop [program]
  (let [jmps (filter-opcode program "jmp")]
    (clojure.pprint/pprint jmps)
    (for [j jmps]
      (assoc program (first j) (assoc (second j) :op "nop")))))

(defn gen-nop-to-jmp [program]
  (let [nops (filter-opcode program "nop")]
    (for [n nops]
      (assoc program (first n) (assoc (second n) :op "jmp")))))

(defn solve-part2 [input]
  (let [program (parse-program input)
        jmp-to-nop (gen-jmp-to-nop program)
        nop-to-jmp (gen-nop-to-jmp program)]
    (loop [progs (concat jmp-to-nop nop-to-jmp)]
      (let [cur-prog (first progs)
            ret-val (run-program cur-prog)]
        (if (>= ret-val 0)
          (do
            (println (str "EXIT CODE " ret-val))
            (println (str "Final PC value: " @pc))
            (println (str "Final ACC Value: " @acv)))
          (recur (rest progs)))))))
