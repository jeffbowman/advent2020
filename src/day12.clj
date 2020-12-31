(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hashp.core]))

(defmacro read-data [filename & forms]
  `(->> ~filename
        io/resource
        slurp
        ~@forms))

(def sample-data (read-data "../data/sample12.txt"
                            str/split-lines
                            (map #(first (re-seq #"([A-Z])(\d+)" %)))
                            (map (fn [[_ b c]] [(keyword b) (read-string c)]))))

(def data (read-data "../data/day12.txt"
                     str/split-lines
                     (map #(first (re-seq #"([A-Z])(\d+)" %)))
                     (map (fn [[_ b c]] [(keyword b) (read-string c)]))))

;; (def data (->> "../data/day12.txt"
;;               io/resource
;;               slurp
;;               str/split-lines
;;               (map #(first ( re-seq #"([A-Z])(\d+)" %)))
;;               (map (fn [[_ b c]] [(keyword b) (read-string c)]))
;;               ))

(def cardinal-vals {:E [-1 0] :W [1 0] :N [0 1] :S [0 -1]} )
(def dirs { 0 :E 90 :N 180 :W 270 :S})
(def state {:pos [0 0] :dir 0 :waypoint [-10 1]})

(defn update-pos [cardinal-dir]
  (fn [{:keys [pos dir] :as state} opt] (assoc state :pos (map + pos (map #(* opt %) (cardinal-dir cardinal-vals))))))

(defn update-dir [op]
  (fn [{:keys [pos dir] :as state} opt] (assoc state :dir (mod (op dir opt) 360))))

(def rules-part1
  {:E (update-pos :E)
   :W (update-pos :W)
   :N (update-pos :N)
   :S (update-pos :S)
   :F (fn [{:keys [pos dir] :as state} opt] (assoc state :pos (map + pos (map #(* opt %) ((get dirs dir) cardinal-vals)))))
   :L (update-dir +)
   :R (update-dir -)})

(def part1-ans (->>
 sample-data
 (reduce (fn [state [cmd opt]] ( (rules-part1 cmd) state opt)) state)
:pos
 (map #( Math/abs %))
 (apply +)))


(defn deg->rad [degree] (* degree (/ Math/PI 180)))

(defn rotate [ [px py] [x y] degrees]
  (let [theta (deg->rad degrees)
        x' (- (* x (Math/cos  theta)) (* y (Math/sin theta)))
        y' (+ (* y (Math/cos theta)) (* x (Math/sin theta)))
        ]
    [(Math/round x') (Math/round y')]))


(defn update-pos-2 [cardinal-dir]
  (fn [{:keys [pos dir waypoint] :as state} opt] (assoc state :waypoint (map + waypoint (map #(* opt %) (cardinal-dir cardinal-vals))))))

(defn update-dir-2 [op]
  (fn [{:keys [pos dir waypoint] :as state} opt] (assoc state :waypoint (rotate pos waypoint (* op opt)))))

(def rules-part2 {:E (update-pos-2 :E)
                  :W (update-pos-2 :W)
                  :N (update-pos-2 :N)
                  :S (update-pos-2 :S)
                  :F (fn [{:keys [pos dir waypoint] :as state} opt] (assoc state :pos (map + pos (map #(* opt %) waypoint))))
                  :L (update-dir-2 -1)
                  :R (update-dir-2 1)})

(def part2-ans  (->>
                 sample-data
                 (reduce (fn [state [cmd opt]] ( (rules-part2 cmd) state opt)) state)
                 :pos
                 (map #( Math/abs %))
                 (apply +)))


