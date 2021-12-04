;; # Part one
(ns aoc2021.01
  (:require [nextjournal.clerk.viewer :as v]
            [nextjournal.clerk :as clerk]))

;;For example, suppose you had the following report:
(def example-report (list 199 200 208 210 200 207 240 269 260 263))

;;We can split this report into overlapping group by two
(partition 2 1 example-report)

;;Now we can check if values in pairs are increasing
(map (fn [[a b]] (< a b)) (partition 2 1 example-report))

;;We are only interested in true values, so lets leave only them
(filter true?
        (map (fn [[a b]] (< a b))
             (partition 2 1 example-report)))

;;And we only interested in number of those values, so we count
(defn solution
  [xs]
  (count
    (filter true?
            (map (fn [[a b]] (< a b))
                 (partition 2 1 xs)))))

(solution example-report)

;;And now we only need to load input from file
(defn load-data [name]
  (map #(Integer/parseInt %)
       (clojure.string/split-lines (slurp (str "resources/" name ".txt")))))

(load-data "01_example")

(solution (load-data "01_example"))

(solution (load-data "01_puzzle"))

;; Part one completed. now we go for a part two
;; # Part two. Diving window.

;; Input stays the same. We only need to define a "sliding window" of tries.
;; Can partition be an answer again?
(solution
  (map #(apply + %)
       (partition 3 1 (load-data "01_puzzle"))))

;; Yep it is!
;; Some graphing:

(defn as-graph-map
  [data]
  (map (fn [x y] {:x x :y y})
       (range 0 (count data))
       data))

(as-graph-map (load-data "01_example"))

;; ## Dive plot
(defn dive-plot [name]
  (clerk/vl
    {:width 650 :height 400
     :data {:values (as-graph-map (load-data name))}
     :mark {:type "line"
            :interpolate "step-after" }
     :encoding {:y {:field "y" :type "quantitative" :sort "descending"}
                :x {:field "x" :type "quantitative"}}}))

;; Using example data:

(dive-plot "01_example")

;; Using main puzzle plot data:

(dive-plot "01_puzzle")



