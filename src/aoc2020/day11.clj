(ns aoc2020.day11
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]
            [clojure.set :refer [difference]]))

(def sample "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

(def vis-test ".......#.\n...#.....\n.#.......\n.........\n..#L....#\n....#....\n.........\n#........\n...#.....")

(def vis-test-2 ".##.##.\n#.#.#.#\n##...##\n...L...\n##...##\n#.#.#.#\n.##.##.\n")

(def vis-test-3 ".............\n.L.L.#.#.#.#.\n.............\n")

(defn parse-seating-chart [input]
  (into {}
        (for [[i row] (map-indexed list (split-lines input))
              [j cell] (map-indexed list (split row #""))]
          [{:x i :y j} cell])))

(def day11input
  (->> (io/resource "day11input")
       (slurp)))

(defn get-adjacent [coords seating]
  (let [crows (into [] (range (- (:x coords) 1) (+ (:x coords) 2)))
        ccols (into [] (range (- (:y coords) 1) (+ (:y coords) 2)))]
    (filter
      #(not= coords (first %))
      (for [i crows
            j ccols]
        [{:x i :y j} (get seating {:x i :y j})]))))

(defn nextdir [direction]
  (case direction
        :north     :northeast
        :northeast :east
        :east      :southeast
        :southeast :south
        :south     :southwest
        :southwest :west
        :west      :northwest
        :northwest :finished))

(defn slope [direction]
  (case direction
    :north {:x 0 :y -1}
    :northeast {:x 1 :y -1}
    :east {:x 1 :y 0}
    :southeast {:x 1 :y 1}
    :south {:x 0 :y 1}
    :southwest {:x -1 :y 1}
    :west {:x -1 :y 0}
    :northwest {:x -1 :y -1}))

(defn get-visible [coord seats limits]
  (let []
    (loop [x (:x coord)
           y (:y coord)
           vis {}
           direction :north]
      (cond
        (= direction :finished) vis
        (or (> x (:xmax limits)) (< x (:xmin limits))
            (> y (:ymax limits)) (< y (:ymin limits)))
            (recur (:x coord) (:y coord) vis (nextdir direction))
        :else
        (let [slope (slope direction)
              xs (+ x (:x slope))
              ys (+ y (:y slope))
              tcoord (get seats {:x xs :y ys})]
          (cond
            (= tcoord "L") (recur (:x coord) (:y coord) vis (nextdir direction))
            (= tcoord "#") (recur (:x coord) (:y coord) (assoc vis {:x xs :y ys} tcoord) (nextdir direction))
            :else (recur xs ys vis direction)))))))

(def changes (atom 0))

(defn empty-rule [seat seating]
  (let [coords (first seat)
        adjacent (get-adjacent coords seating)
        can-sit (empty? (filter (fn [[c v]] (= v "#")) adjacent))]
    (if can-sit
      (do
        (swap! changes inc)
        [coords "#"])
      seat)))

(defn empty-rule-2 [seat seating limits]
  (let [coords (first seat)
        visible (get-visible coords seating limits)
        can-sit (empty? visible)]
    ;(clojure.pprint/pprint visible)
    (if can-sit
      (do
        (swap! changes inc)
        [coords "#"])
      seat)))

(defn occupied-rule [seat seating tolerance]
  (let [coords (first seat)
        adjacent (get-adjacent coords seating)
        must-rise (>= (count (filter (fn [[c v]] (= v "#")) adjacent)) tolerance)]
    (if must-rise
      (do
        (swap! changes inc)
        [coords "L"])
      seat)))

(defn occupied-rule-2 [seat seating tolerance limits]
  (let [coords (first seat)
        visible (get-visible coords seating limits)
        must-rise (>= (count (filter (fn [[c v]] (= v "#")) visible)) tolerance)]
    (if must-rise
      (do
        (swap! changes inc)
        [coords "L"])
      seat)))

(defn apply-rules [seating]
  (into {}
        (map (fn [seat]
               (let [status (second seat)]
                 (cond (= status "L") (empty-rule seat seating)
                       (= status "#") (occupied-rule seat seating 4)
                       :else seat))) seating)))

(defn apply-rules-2 [seating limits]
  (into {}
        (map (fn [seat]
               (let [status (second seat)]
                 (cond (= status "L") (empty-rule-2 seat seating limits)
                       (= status "#") (occupied-rule-2 seat seating 5 limits)
                       :else seat))) seating)))

(defn print-grid [seating limits]
  (print
    (clojure.string/join
      (for [i (range (:xmin limits) (inc (:xmax limits)))
            j (range (:ymin limits) (inc (:ymax limits)))]
        (str (get seating {:x i :y j}) (if (= j (:ymax limits)) "\n"))))))

(defn do-moves [input]
  (let [seating (parse-seating-chart input)]
    (loop [iteration 0
           current-seating seating]
      (reset! changes 0)
      (println (str "ITERATION: " iteration))
      (let [updated-seating (apply-rules current-seating)]
        (if (= @changes 0)
          (do (println (str "FINAL NUM ITERATIONS: " iteration))
              updated-seating)
          (recur (inc iteration) updated-seating))))))

(defn do-moves-2 [input]
  (let [seating (parse-seating-chart input)
        limits {:xmax (apply max (map (fn [[c v]] (:x c)) seating))
                :xmin (apply min (map (fn [[c v]] (:x c)) seating))
                :ymax (apply max (map (fn [[c v]] (:y c)) seating))
                :ymin (apply min (map (fn [[c v]] (:y c)) seating))}]
    ;(print-grid seating limits)
    (println limits)
    (loop [iteration 0
           current-seating seating]
      (reset! changes 0)
      (println (str "ITERATION: " iteration))
      (let [updated-seating (apply-rules-2 current-seating limits)]
        ;(print-grid updated-seating limits)
        (if (= @changes 0)
          (do (println (str "FINAL NUM INTERATIONS: " iteration))
              updated-seating)
          (recur (inc iteration) updated-seating))))))

(defn solve-part-1 [input]
  (reset! changes 0)
  (let [seated (do-moves input)
        num-occupied (count (filter (fn [[c v]] (= v "#")) seated))]
    (println (str "number occupied seats: " num-occupied))))

(defn solve-part-2 [input]
  (reset! changes 0)
  (let [seated (do-moves-2 input)
        num-occupied (count (filter (fn [[c v]] (= v "#")) seated))]
    (println (str "number occupied seats: " num-occupied))))
