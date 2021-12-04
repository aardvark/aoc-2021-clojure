;; # Day 2. Dive
(ns aoc2021.02
  (:require [nextjournal.clerk :as clerk]))

;; Initial example:
;; forward 5
;; down 5
;; forward 8
;; up 3
;; down 8
;; forward 2

;; ## Loading data
;; Again we can load line by line, split each line into
;; key value pair and return list of maps
(defn load-data [name]
  (map (fn [s] (let [[k v] (clojure.string/split s #"\s")]
                     [(keyword k) (Integer/parseInt v)]))
       (clojure.string/split-lines (slurp (str "resources/" name ".txt")))))

;; Result:
(load-data "02_example")

;; ## Encoding movement
;;
;; So we have two coordinates to take account now *x* and *y*.
;; Given that each movement doing something functional with one coordinates
;; we can use this as a simple function calls that would return change
;; so ```forward 5``` ==> ```{:x 5}``` , ```down 5 ```==> ```{:y 5}``` and ```up 3``` ==> ```{:y -3}```.
;;
;; Easiest way to get result is to just run reduction on whole sequence of movements
;; where each step is transformed into increment and reduction step just add those increments
;; to the current value.

;; Reduction function:
(defn new-coordinates
  [current step]
    (let [[direction change] step
          [coordinate change]
          (case direction
            :forward [:x change]
            :up [:y (- change)]
            :down [:y change]
            )]
      (update current coordinate + change))
    )

;; Reduction itself:
(reduce new-coordinates {:x 0 :y 0} (load-data "02_example"))

;; A more interesting is if we can just see a whole route using coordinate changes.
;; This allows us to plot a whole route.
;; To do that we can save a state on each step and only do map without any reduce:
(defn route [name]
  (cons {:x 0 :y 0}
        (let [state (atom {:x 0 :y 0})]
          (map
            (fn [x]
              (reset! state (new-coordinates @state x)))
            (load-data name)))))

(route "02_example")

;; And we can reuse plot function from Day 1.
(defn dive-plot [name]
  (clerk/vl
    {:width 650 :height 400
     :data {:values (route name)}
     :mark {:type "line"
            :interpolate "step-after" }
     :encoding {:y {:field "y" :type "quantitative" :sort "descending"}
                :x {:field "x" :type "quantitative"}}}))

(dive-plot "02_example")

;; And now answer part 1 puzzle itself:
(apply * (vals (reduce new-coordinates {:x 0 :y 0} (load-data "02_puzzle"))))

;; And dive-plot:
(dive-plot "02_puzzle")

;; # Part two. Aim
;;
;; So we need now to track aim and change how forward _function_ works.
;; Redefine coordinates change as is:

(defn new-coordinates2
  [current step]
  (let [[direction change] step
        step-update (case direction
                      :forward {:x change :y (* (:aim current) change)}
                      :up {:aim (- change)}
                      :down {:aim change}
                      )]
    (merge-with + current step-update)))

;; And redefine route function to also have :aim state
(defn route2 [name]
  (cons {:x 0 :y 0 :aim 0}
        (let [state (atom {:x 0 :y 0 :aim 0})]
          (map
            (fn [x]
              (reset! state (new-coordinates2 @state x)))
            (load-data name)))))

(route2 "02_example")

;; And finally redefine dive plot to use new route function

(defn dive-plot2 [name]
  (clerk/vl
    {:width 650 :height 400
     :data {:values (route2 name)}
     :mark {:type "line"
            :interpolate "step-after" }
     :encoding {:y {:field "y" :type "quantitative" :sort "descending"}
                :x {:field "x" :type "quantitative"}}}))

(dive-plot2 "02_example")

;; Answer to the puzzle part 2 is:
(apply * (vals (select-keys (last (route2 "02_puzzle")) [:x :y])))

;; And dive plot for part 2 is:
(dive-plot2 "02_puzzle")
