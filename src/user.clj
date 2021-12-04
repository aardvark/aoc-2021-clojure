(ns user
  (:require [nextjournal.clerk :as clerk]))

;; start Clerk's buit-in webserver on the default port 7777, opening the browser when done
(clerk/serve! {:browse? true})

(clerk/serve! {:watch-paths ["src/aoc2021"]})

(comment
  (clerk/show! "src/aoc2021/03.clj"))