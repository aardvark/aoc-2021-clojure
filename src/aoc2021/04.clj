;; # Day 4: Giant Squid
(ns aoc2021.04
  (:require [clojure.string]))

;; Parsing input to the collection of input sequence and boards:
(let [[input & boards]
      (into []
            (comp
              (partition-by #(= % ""))
              (remove #(= % [""])))
            (clojure.string/split-lines
              (slurp "resources/04_example.txt")))]
  input
  )


;; Parsing board input into
(defn board [s]
  (let [rows (map #(map (fn [x] (Integer/parseInt x)) (re-seq #"\d+" %)) s)
        cols (list (map #(nth % 0) rows) (map #(nth % 1) rows) (map #(nth % 2) rows)
                   (map #(nth % 3) rows) (map #(nth % 4) rows))]
    {:rows rows :cols cols}))

;; Joint it all together:
(let [[[input] & boards]
      (into []
            (comp
              (partition-by #(= % ""))
              (remove #(= % [""])))
            (clojure.string/split-lines
              (slurp "resources/04_example.txt")))]
  {:input  (map #(Integer/parseInt %) (re-seq #"\d+" input))
   :boards (map board boards)})

;; So bingo is played by turns, each turn we play a number and check if board win
;; Board is count as winner if column or row is fully exhausted
;; We will count exhaustion by just removing number from row or column

(defn play
  [board number]
  (let [{:keys [rows cols win]} board]
    {:rows (map (fn [row] (remove #(= % number) row)) rows)
     :cols (map (fn [col] (remove #(= % number) col)) cols)}))

(play {:rows [[1 2 3] [1 1 1]] :cols [[2 3 1] [3 3 3]]} 2)

;; Create win 'predicate' that check if either of rows or cols if fully exhausted
(defn win?
  [board]
  (let [{:keys [rows cols]} board]
      (or ((complement empty?) (filter empty? rows))
          ((complement empty?) (filter empty? cols)))))

(win? {:rows [[123]] :cols [[231] []]})

;; Now we create a recursive function that will play one round and check if any
;; board is a winner.
;; In case it is a winner it will return winning board and winning number,
;; so we can calculate a score.
(defn bingo-winner
  [name]
  (let [[[input] & boards]
        (into []
              (comp
                (partition-by #(= % ""))
                (remove #(= % [""])))
              (clojure.string/split-lines
                (slurp (str "resources/04_" name ".txt"))))
        state {:input  (map #(Integer/parseInt %) (re-seq #"\d+" input))
               :boards (map board boards)}]
    (loop [state         state
           played-number 0]
      (if (every? false? (map win? (:boards state)))
        (recur {:boards (map #(play % (first (:input state))) (:boards state))
                :input  (rest (:input state))}
               (first (:input state))
               )
        {:winner (first (get (group-by win? (:boards state)) true))
         :number played-number}))))

(let [result (bingo-winner "example")]
  (* (:number result) (reduce + (dedupe (sort (flatten (vals (:winner result))))))))

(let [result (bingo-winner "puzzle")]
  (* (:number result) (reduce + (dedupe (sort (flatten (vals (:winner result))))))))

;; # Day 4: Giant Squid. Part two.

;; Second part require us to find a last board to win.
;; So we can change recursive part of bingo-winner function to continue recursion in case
;; if win and only stop when win is triggered *and* there is no more boards left.

;; Firstly, we can adapt win check here by doing a grouping by `win?` and pass
;; only non-win board to the next recursion step
(defn last-bingo-winner
  [name]
  (let [[[input] & boards]
        (into []
              (comp
                (partition-by #(= % ""))
                (remove #(= % [""])))
              (clojure.string/split-lines
                (slurp (str "resources/04_" name ".txt"))))
        state {:input  (map #(Integer/parseInt %) (re-seq #"\d+" input))
               :boards (map board boards)}]
    (loop [state         state
           played-number 0]
      (let [boards-by-win-lose (group-by win? (:boards state))
            win-boards (get boards-by-win-lose true)
            boards-in-play (get boards-by-win-lose false)
            next-number (first (:input state))]
        (if (and (empty? boards-in-play) (= 1 (count win-boards)))
          {:winner (first win-boards) :number played-number}
          (recur {:boards (map #(play % next-number) boards-in-play)
                :input (rest (:input state))}
                 next-number))))))

;; And here we go with Day 4 solution.

(last-bingo-winner "example")

(let [result (last-bingo-winner "example")]
  (* (:number result) (reduce + (dedupe (sort (flatten (vals (:winner result))))))))

(let [result (last-bingo-winner "puzzle")]
  (* (:number result) (reduce + (dedupe (sort (flatten (vals (:winner result))))))))
