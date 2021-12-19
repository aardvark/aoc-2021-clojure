;; # --- Day 5: Hydrothermal Venture ---
(ns aoc2021.05
  (:require [nextjournal.clerk :as clerk]))

;; Input lines are defined by x,y pairs
;; So defition of 1,1 -> 1,3 is transformed into line like that:
;;```
;;  1 2 3
;; 1x . .
;; 2x . .
;; 3x . .
;;```
;;
;; Goal is to understand find how many lines have intersecting points
;; and how many of those points is on the field.

;; ## Naive approach
;; 1. Define two dimensional array
;; 2. fill array with zeroes
;; 3. Go through all lines and increment existing array counter for given coordinate

;; ## Naive map approach
;; 1. Take line
;; 2. Unroll line into points
;; 3. Put point into hashmap with x,y as key and value is 1
;; 4. If point is already present in hashmap increment its counter value.

(slurp "resources/05_example.txt")

;; Line parsing function that is also create a h-or-v predicate for us:
(defn line [s]
  (let [[x1 y1 x2 y2] (map #(Integer/parseInt %) (re-seq #"\d+" s))]
    {:line [x1 y1 x2 y2] :type (cond (= x1 x2) :h
                                     (= y1 y2) :v
                                     :else :x)}))

(line "0,9  -> 5,9")
(line "6,4  -> 2,0")

;; Now we need to transform line to the sequence of points, so
;; ```0,9 -> 5,9``` would become a ```[0,9] [1,9] [2,9]... etc```
;; vertical lane:
(defn vline->points
  [line]
  (let [[x1 y1 x2 _] line
        xs (range (min x1 x2) (inc (max x1 x2)))
        ys (repeat (count xs) y1)
        ]
    (loop [r  (transient [])
           xs xs
           ys ys]
      (if (and xs ys)
        (recur (conj! r [(first xs) (first ys)])
               (next xs)
               (next ys))
        (persistent! r)))))

(vline->points (:line (line "0,9 -> 5,9")))

;; For horizontal line we would have a same algorithm, expect we would need to
;; move repeat and range to the different coordinates:
(defn hline->points
  [line]
  (let [[x1 y1 _ y2] line
        ys (range (min y1 y2) (inc (max y1 y2)))
        xs (repeat (count ys) x1)
        ]
    (loop [r  (transient [])
           xs xs
           ys ys]
      (if (and xs ys)
        (recur (conj! r [(first xs) (first ys)])
               (next xs)
               (next ys))
        (persistent! r)))))

(hline->points (:line (line "0,1 -> 0,9")))

;; And now we can do a generic dispatch based on a line :type
(defn line->points
  [line]
  (let [{:keys [line type]} line]
    (case type
      :v (vline->points line)
      :h (hline->points line)
      [])))

(line->points (line "0,1 -> 0,9"))
(line->points (line "0,9 -> 5,9"))

;; Vent map generation:
(defn vent-map-points
  [points]
  (loop [m  (transient {})
         ks points]
    (if (empty? ks)
      (persistent! m)
      (let [k (first ks)]
        (recur (assoc! m k (inc (get m k 0)))
               (rest ks))))))

(vent-map-points (line->points (line "0,9 -> 5,9")))

(vent-map-points
  (concat
    (line->points (line "0,9 -> 5,9"))
    (line->points (line "2,9 -> 5,9"))))

(defn vent-map
  [name]
  (vent-map-points
    (transduce
      (map (comp line->points line))
      concat (clojure.string/split-lines (slurp (str "resources/05_"name".txt"))))))

;; A simple dot graph for vents where marker is number of went lines at the point
(defn dot-graph [points]
  (clerk/vl
    {:width    800 :height 800
     :data     {:values (map (fn [[[x y] n]] {:x x :y y :n n}) points)}
     :encoding {:x    {:field "x" :type "ordinal"}
                :y    {:field "y" :type "ordinal"}}

     :layer [
             ;{:mark "text"
             ;:encoding {:x    {:field "x" :type "quantitative"}
             ;           :y    {:field "y" :type "quantitative"}
             ;           :text {:field "n" :type "quantitative"}
             ;           }}
             {:mark "circle"
              :encoding {:color {:field "n" :type "ordinal" :impute {:value 0}}
                         :x {:field "x" :type "ordinal" }
                         :y {:field "y" :type "ordinal" }
                         }}

             ]
     }))

(dot-graph (vent-map "example"))

(defn answer [name]
  (reduce (fn [acc add] (if (> add 1) (inc acc) acc)) 0
          (vals
            (vent-map-points
              (transduce
                (map (comp line->points line))
                concat (clojure.string/split-lines (slurp (str "resources/05_" name ".txt"))))))))

(answer "example")

(answer "puzzle")

(dot-graph (vent-map "puzzle"))

;; For part 2 we need to implement a diagonal line parsing
(defn ladder
  [x1 x2]
  (if (< x1 x2)
    (range x1 (inc x2))
    (reverse (range x2 (inc x1)))))

(defn xline->points
  [line]
  (let [[x1 y1 x2 y2] line
        ys (ladder y1 y2)
        xs (ladder x1 x2)
        ]
    (loop [r  (transient [])
           xs xs
           ys ys]
      (if (and xs ys)
        (recur (conj! r [(first xs) (first ys)])
               (next xs)
               (next ys))
        (persistent! r)))))

(xline->points (:line (line "9,7 -> 7,9")))

;; Redefine parsing function to add diagonal line parser
(defn line->points
  [line]
  (let [{:keys [line type]} line]
    (case type
      :v (vline->points line)
      :h (hline->points line)
      :x (xline->points line))))

(line->points (line "9,7 -> 7,9"))
