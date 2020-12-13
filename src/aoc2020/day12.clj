(ns aoc2020.day12
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]))

(defn parse-command [cmd]
  {:cmd (subs cmd 0 1)
   :value (Integer/parseInt (subs cmd 1))})

(def sample
  (split-lines "F10\nN3\nF7\nR90\nF11\n"))

(def day12input
  (->> (io/resource "day12input")
       (slurp)
       (split-lines)))

(def state (atom {:heading 90
                  :position [0 0]}))


(defn calc-radius [x y] (Math/sqrt (+ (Math/pow x 2.0) (Math/pow y 2.0))))

;; Waypoint given in polar coordinates

(defn calc-phi [x y]
  (Math/atan2 y x))

(def way-state (atom {:phi (calc-phi 10 1)
                      :radius (calc-radius 10 1)
                      :position [10 1]}))

(defn to-cart [phi]
  [(* (get @way-state :radius) (Math/cos phi))
   (* (get @way-state :radius) (Math/sin phi))])

(defn get-pos []
  {:x (get-in @state [:position 0])
   :y (get-in @state [:position 1])})

(defn get-heading []
  (get @state :heading))

(defn set-position [new-pos]
  (swap! state assoc :position new-pos))

(defn set-way-phi [new-phi]
  (swap! way-state assoc :phi new-phi))

(defn set-way-position [new-pos]
  (swap! way-state assoc :position new-pos)
  (swap! way-state assoc :radius (calc-radius (first new-pos) (second new-pos)))
  (set-way-phi (calc-phi (first new-pos) (second new-pos))))

(defn get-way-pos []
  {:x (get-in @way-state [:position 0])
   :y (get-in @way-state [:position 1])})

(defn get-way-phi []
  (get @way-state :phi))

(defn set-heading [new-heading]
  (swap! state assoc :heading (mod new-heading 360)))

(defn get-cardinal [heading]
  (case heading
    0 "N"
    90 "E"
    180 "S"
    270 "W"
    360 "N"))

(defn execute [cmd]
  (let [position (get-pos)
        heading (get-heading)]
   (case (:cmd cmd)
    "N" (set-position [(:x position) (+ (:y position) (:value cmd))])
    "S" (set-position [(:x position) (- (:y position) (:value cmd))])
    "E" (set-position [(+ (:x position) (:value cmd)) (:y position)])
    "W" (set-position [(- (:x position) (:value cmd)) (:y position)])
    "L" (set-heading (- heading (:value cmd)))
    "R" (set-heading (+ heading (:value cmd)))
    "F" (execute {:cmd (get-cardinal heading) :value (:value cmd)})
    :else nil)))

(defn rotate-waypoint [deg]
  (let [rad (Math/toRadians deg)
        cur-phi (get-way-phi)
        new-phi (+ rad cur-phi)]
    (set-way-position (to-cart new-phi))
    (set-way-phi new-phi)
    @way-state))

(defn goto-waypoint [n]
  (doall
    (for [i (range n)]
      (let [way-pos (get-way-pos)
            ship-pos (get-pos)
            xs (:x ship-pos)
            ys (:y ship-pos)
            xw (:x way-pos)
            yw (:y way-pos)]
        (set-position [(+ xs xw) (+ ys yw)]))))
  @state)

(defn execute-2 [cmd]
  (let [position (get-way-pos)]
    (case (:cmd cmd)
      "N" (set-way-position [(:x position) (+ (:y position) (:value cmd))])
      "S" (set-way-position [(:x position) (- (:y position) (:value cmd))])
      "E" (set-way-position [(+ (:x position) (:value cmd)) (:y position)])
      "W" (set-way-position [(- (:x position) (:value cmd)) (:y position)])
      "L" (rotate-waypoint (+ (:value cmd)))
      "R" (rotate-waypoint (- (:value cmd)))
      "F" (goto-waypoint (:value cmd))
      :else nil)))

(defn solve-part-1 [input]
  (reset! state {:heading 90 :position [0 0]})
  (let [cmds (into [] (map parse-command input))]
    (into [] (for [cmd cmds] (execute cmd))))
  (clojure.pprint/pprint @state)
  (println (str "SUM: " (+ (Math/abs (:x (get-pos))) (Math/abs (:y (get-pos)))))))

(defn solve-part-2 [input]
  (reset! state {:heading 90 :position [0 0]})
  (reset! way-state {:phi (calc-phi 10 1)
                     :radius (calc-radius 10 1)})
  (swap! way-state assoc :position (to-cart (calc-phi 10 1)))
  (println @way-state)
  (println @state)
  (let [cmds (into [] (map parse-command input))]
    (into [] (for [cmd cmds] (execute-2 cmd))))
  (clojure.pprint/pprint @state)
  (println (str "SUM: " (+ (Math/abs (:x (get-pos))) (Math/abs (:y (get-pos)))))))
