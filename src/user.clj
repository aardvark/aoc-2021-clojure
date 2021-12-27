(ns user
  (:require [nextjournal.clerk :as clerk]))

;; start Clerk's buit-in webserver on the default port 7777, opening the browser when done
(clerk/serve! {:browse? true})

(clerk/serve! {:watch-paths ["src/aoc2021"]})

(comment
  '(require [nextjournal.clerk :as clerk])
  (clerk/show! "src/aoc2021/07.clj")
  (clerk/build-static-app! {:paths ["src/aoc2021/01.clj"
                                    "src/aoc2021/02.clj"
                                    "src/aoc2021/03.clj"
                                    "src/aoc2021/04.clj"
                                    "src/aoc2021/05.clj"
                                    "src/aoc2021/06.clj"
                                    "src/aoc2021/07.clj"
                                    ]}))