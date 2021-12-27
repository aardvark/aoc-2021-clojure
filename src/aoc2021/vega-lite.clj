(ns aoc2021.vega-lite 
  (:require [nextjournal.clerk :as clerk]))

(defn sine-plot []
  (clerk/vl
   {:width 300
    :height 160
    :data {:sequence {:start 0 :stop 16 :step 0.1 :as "x"}}
    :transform [{:calculate "abs(16 - datum.x)" :as "16"}
                {:calculate "abs(2 - datum.x)" :as "2"}
                {:calculate "abs(1 - datum.x)" :as "1"}
                {:calculate "abs(0 - datum.x)" :as "0"}]
    :layer [{:mark "line"
             :encoding {:x {:field "x" :type "quantitative"}
                        :y {:field "16" :type "quantitative"}}}
            {:mark "line"
             :encoding {:x {:field "x" :type "quantitative"}
                        :y {:field "2" :type "quantitative"}}}
            {:mark "line"
             :encoding {:x {:field "x" :type "quantitative"}
                        :y {:field "1" :type "quantitative"}}}
            {:mark "line"
             :encoding {:x {:field "x" :type "quantitative"}
                        :y {:field "0" :type "quantitative"}}}]}))