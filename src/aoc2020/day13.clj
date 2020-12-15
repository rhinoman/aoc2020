(ns aoc2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines trim split includes? join]]))


(defn parse-input [input]
  (let [sl (split-lines input)]
    {:earliest (Integer/parseInt (first sl))
     :busses (map #(try (Integer/parseInt %) (catch NumberFormatException e %))
                  (split (second sl) #","))}))

(def sample (parse-input "939\n7,13,x,x,59,x,31,19"))

(def day13input
  (->> (io/resource "day13input")
       (slurp)
       (parse-input)))

(defn earliest-departure [bus earliest]
  (let [periods (int (/ earliest bus))
        prev-dep (* periods bus)
        next-dep (+ prev-dep bus)
        min-wait (- next-dep earliest)]
    [bus {:next-departure next-dep :wait min-wait}]))

(defn solve-part-1 [input]
  (let [busses (filter int? (:busses input))
        departures (into {} (map #(earliest-departure % (:earliest input)) busses))
        ordered-departures (sort-by (fn [x] (get (second x) :next-departure)) departures)
        earliest (first ordered-departures)]
    (clojure.pprint/pprint earliest)
    (* (first earliest) (get (second earliest) :wait))))

(defn prep-bus-list [input]
  (sort-by #(:index %)
           (into [] (map-indexed
                      (fn [idx b] {:index idx :id b})
                      (:busses input)))))

(defn prep-bus-list* [input]
  (map
    #(assoc % :offset (mod (- (:index %)) (:id %)))
    (filter #(int? (:id %)) (prep-bus-list input))))

(defn check-timestamp [ts busses lindex]
  (loop [bus-ls busses]
    (if (empty? bus-ls)
      true
      (let [bus (first bus-ls)
            index (:index bus)
            id (:id bus)
            bus-ts (+ ts (- index lindex))]
        (cond
          (= id "x") (recur (rest bus-ls))
          (not= (mod bus-ts id) 0) false
          :else (recur (rest bus-ls)))))))

(defn solve-part-2 [input]
  (let [busses (prep-bus-list input)
        largest (apply max (filter int? (:busses input)))
        lindex (:index (first (filter #(= (:id %) largest) busses)))
        tstamps (take-nth largest (range))]
    (pmap #(map (fn [ts]
            (if (check-timestamp ts busses lindex)
              (println (str "FOUND TIMESTAMP: " (- ts lindex)))))
          %)
         (partition 500 tstamps))))

(defn comp-next-bus [off next-bus root-bus]
  (loop [offset off]
    (if (= (mod offset (:id next-bus)) (:offset next-bus))
      offset
      (recur (+ offset root-bus)))))

(defn solve-part-2* [input]
  (let [busses (prep-bus-list* input)
        bus-nums (reverse (sort-by #(:offset %) busses))
        bus (:id (first bus-nums))]
    (loop [offset (:offset (first bus-nums))
           rootbus bus
           bs (rest bus-nums)]
      (if (empty? bs)
        offset
        (let [next-bus (first bs)
              next-offset (comp-next-bus offset next-bus rootbus)]
          (recur next-offset (* rootbus (:id next-bus)) (rest bs)))))))