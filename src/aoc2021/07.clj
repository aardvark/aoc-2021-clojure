; # --- Day 7: The Treachery of Whales ---
(ns aoc2021.07
  (:require [clojure.string :as string]
            [nextjournal.clerk :as clerk]))

(def example [16 1 2 0 4 2 7 1 2 14])
(def puzzle (map #(Integer/parseInt %)
                 (string/split (slurp "resources/07_puzzle.txt") #",")))

(defn linear-fuel-cost
  [point input]
  (reduce (fn [acc x] (+ acc (if (neg? x) (- x) x)))
          0
          (map #(- point %) input)))

(defn solution
  [fuel-function input]
  (apply min-key val
         (into {} (map (fn [p] {p (fuel-function p input)}))
               (keys (frequencies input)))))

(defn part1 [input]
  ((memoize solution)
   linear-fuel-cost input))

;; For part 2 fuel is computed arithmetically
(defn arithmetic-fuel-cost
  [point input]
  (reduce + 0 (map (fn [x]
                     (let [shifts (- point x)
                           shifts (if (neg? shifts) (- shifts) shifts)
                           cost (quot (* shifts (+ shifts 1)) 2)]
                       cost))
                   input)))

(defn part2 [input]
  ((memoize solution)
   arithmetic-fuel-cost input))

;; Answers are:

(part1 example)

(part2 example)

(time
 (part1 puzzle))
(time
 (part2 puzzle))

;; And now when puzzle is done we an try to visually see how this finding of minumim movement is looks
;; Lets first create a graph for two points 16 and 1 and we are going to do a linear fuel cost move:

(defn linear-cost-f [input]
  (reduce (fn [a b] (str a " + " b)) (map (fn [x] (str "abs(" x " - datum.x)")) input)))

(defn cost-graph
  [input costf solutionf]
  (clerk/vl
   {:width 300
    :height 160
    :data {:sequence {:start 0 :stop (apply max input) :step 1 :as "x"}}
    :transform [{:calculate (costf input) :as "y"}]
    :layer [{:mark "line"
             :encoding {:x {:field "x" :type "quantitative"}
                        :y {:field "y" :type "quantitative"}}}
            {:mark "point"
             :encoding {:x {:field "x" :aggregate {:argmin "y"} :type "quantitative"}
                        :y {:field "y" :aggregate "min"}}}
            {:mark {:type "text"
                    :dy 10}
             :encoding {:text {:value (apply format "[x:%s, y:%s]" (solutionf input)) :type "quantitative"}
                        :x {:field "x" :aggregate {:argmin "y"} :type "quantitative"}
                        :y {:field "y" :aggregate "min"}}}]}))

;; And now we can graph a part 1 solutions with minimum points

(cost-graph example linear-cost-f part1)

(cost-graph puzzle linear-cost-f part1)

;; Now we need to create a graph input data for part 2 fuel cost calculation:

(defn arithmetic-fuel-cost-f [input]
  (map (fn [x] {:x x :y (arithmetic-fuel-cost x input)}) (range 0 (inc (apply max input)))))

(defn cost-graph-part2
  [input]
  (clerk/vl
   {:width 300
    :height 160
    :data {:values (arithmetic-fuel-cost-f input)}
    :layer [{:mark "line"
             :encoding {:x {:field "x" :type "quantitative"}
                        :y {:field "y" :type "quantitative"}}}
            {:mark "point"
             :encoding {:x {:field "x" :aggregate {:argmin "y"} :type "quantitative"}
                        :y {:field "y" :aggregate "min"}}}
            {:mark {:type "text"
                    :dy 10}
             :encoding {:text {:value (apply format "[x:%s, y:%s]" (part2 input)) :type "quantitative"}
                        :x {:field "x" :aggregate {:argmin "y"} :type "quantitative"}
                        :y {:field "y" :aggregate "min"}}}]}))

(cost-graph-part2 example)
(cost-graph-part2 puzzle)
