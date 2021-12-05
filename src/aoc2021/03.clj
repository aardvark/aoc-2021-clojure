; # Day 3: Binary Diagnostic
(ns aoc2021.03
  (:require [nextjournal.clerk :as clerk]))

;; Given input of this binary numbers:
(slurp "resources/03_example.txt")

;; we are expected to do some clever trick with binary numbers
;; and powers of two, which is too clever for my taste.
;; I will just read each symbols of string into separate collection,
;; and then I can just count numbers of zeroes and ones in each collection
;; and return dominant bit in collection. So just count zeroes and nulls

(clojure.string/split-lines (slurp "resources/03_example.txt"))

;; This is going to be our initial state of reducer:
(comment
  {0 {:zero 0 :one 0}
   1 {:zero 0 :one 0}
   2 {:zero 0 :one 0}
   3 {:zero 0 :one 0}
   4 {:zero 0 :one 0}})

;; Reduction of ```00100``` will result in next state:
(comment
  {0 {:zero 1 :one 0}
   1 {:zero 1 :one 0}
   2 {:zero 0 :one 1}
   3 {:zero 1 :one 0}
   4 {:zero 1 :one 0}})

;; Now how can we transform a ```00100``` into update for reducer?

;; First we would split string into sequence of characters:
(seq "00100")

;; Now we can produce indexed sequence from it:
(map-indexed (fn [idx itm] {idx (if (= itm \0) :zero :one)}) "00100")

;; And if we can do update via merge, then "we are golden":
(merge-with (fn [val-in-result val-in-later]
              (if (= :zero val-in-later)
                (update val-in-result :zero inc)
                (update val-in-result :one inc)
                )
              )
            {0 {:zero 1 :one 0}
             1 {:zero 0 :one 0}} {0 :zero} {0 :one} {1 :zero})

;; So our sequence of actions become:
;; 1. Load a string
;; 2. Transform it into indexed sequence
;; 3. Reduce indexed sequence
;; 4. Loop

;; Sequence of updates:
(mapcat (fn [x] (map-indexed (fn [idx itm] {idx (if (= itm \0) :zero :one)}) x))
        (clojure.string/split-lines (slurp "resources/03_example.txt")))

;; Resulting function:
(defn result []
  (apply merge-with
         (fn [val-in-result val-in-later]
           (if (= :zero val-in-later)
             (update val-in-result :zero inc)
             (update val-in-result :one inc)))
         {0 {:zero 0 :one 0}
          1 {:zero 0 :one 0}
          2 {:zero 0 :one 0}
          3 {:zero 0 :one 0}
          4 {:zero 0 :one 0}}
         (mapcat (fn [x] (map-indexed (fn [idx itm] {idx (if (= itm \0) :zero :one)}) x))
                 (clojure.string/split-lines (slurp "resources/03_example.txt")))))

(result)

;; Now we need to determine a dominant and recessive digit:
(map (fn [[k v]] {k {:gamma (if (> (:one v)(:zero v)) "1" "0")
                   :epsilon (if (> (:one v)(:zero v)) "0" "1")
                   }}) (result))

;; And we need to "combine" digits to form binary:
(def gamma-epsilon-carry
  (apply merge-with str
         (vals
           (mapcat (fn [[k v]] {k {:gamma   (if (> (:one v) (:zero v)) "1" "0")
                                   :epsilon (if (> (:one v) (:zero v)) "0" "1")
                                   }}) (result)))))

;; And finally we need to multiply those numbers together
(apply * (map #(Integer/parseInt % 2) (vals gamma-epsilon-carry)))

;; And now we can adapt whole pipeline for a puzzle input.
;; Firstly we now have a 12 length of binary numbers,
;; so our starting reducer value need to be changed to adapt to this:
(defn initial-reducer-value [x]
  (into (sorted-map)
        (zipmap (range 0 x) (repeat x {:zero 0 :one 0}))))

;; So calling this one is broken in clerk for some reason:
;; ```(initial-reducer-value 12)```

;; Now we rewrite result function to be able to take both filename and expected number
;; of digits to generate result
(defn result [name x]
  (apply merge-with
         (fn [val-in-result val-in-later]
           (if (= :zero val-in-later)
             (update val-in-result :zero inc)
             (update val-in-result :one inc)))
         (initial-reducer-value x)
         (mapcat (fn [x] (map-indexed (fn [idx itm] {idx (if (= itm \0) :zero :one)}) x))
                 (clojure.string/split-lines (slurp (str "resources/" name ".txt"))))))

;; And create function to expose gamma + epsilon result
(defn gamma-epsilon [coll]
  (apply merge-with str
         (vals
           (mapcat (fn [[k v]] {k {:gamma   (if (> (:one v) (:zero v)) "1" "0")
                                   :epsilon (if (> (:one v) (:zero v)) "0" "1")
                                   }}) coll))))

(gamma-epsilon (result "03_example" 5))

;; Check final result for an example:
(apply * (map #(Integer/parseInt % 2) (vals (gamma-epsilon (result "03_example" 5)))))

;; And generate final result for an puzzle entry
(apply * (map #(Integer/parseInt % 2) (vals (gamma-epsilon (result "03_puzzle" 12)))))

;; # Part two. What the actual binary hell.
;;
;; It looks like we can do a binary by bit filter against the initial inputs.
;; And it looks that we need to do this in a recursive mode.
;; Expected sequence of actions:
;; 1. Calculate prevalent leftmost bit

(defn result [name x]
  (apply merge-with
         (fn [val-in-result val-in-later]
           (if (= :zero val-in-later)
             (update val-in-result :zero inc)
             (update val-in-result :one inc)))
         (initial-reducer-value x)
         (mapcat (fn [x] (map-indexed (fn [idx itm] {idx (if (= itm \0) :zero :one)}) x))
                 (clojure.string/split-lines (slurp (str "resources/" name ".txt"))))))

;; Create prevalent map:
(defn prevalent-map
  [input]
  (let [f (fn [x] (apply merge (map-indexed (fn [idx itm] {idx (if (= itm \0) :zero :one)}) x)))]
    (map f input)))

(defn frequencies-for-idx [m idx]
  (frequencies (map #(get % idx) m)))

;; Compute oxygen generator number:
(loop [input (clojure.string/split-lines (slurp (str "resources/03_example.txt")))
       idx   0]
  (let [freq       (frequencies-for-idx (prevalent-map input) idx)
        filter-bit (if (<= (:zero freq) (:one freq)) \1 \0)
        result     (filter (fn [x] (= (nth x idx) filter-bit)) input)]
    (if (nil? (second result))
      (first result)
      (recur result (inc idx)))))

;; To compute co2 scrubber number we only need to select uncommon number:
(loop [input (clojure.string/split-lines (slurp (str "resources/03_example.txt")))
       idx   0]
  (let [freq       (frequencies-for-idx (prevalent-map input) idx)
        filter-bit (if (<= (:zero freq) (:one freq)) \0 \1)
        result     (filter (fn [x] (= (nth x idx) filter-bit)) input)]
    (if (nil? (second result))
      (first result)
      (recur result (inc idx)))))

;; Now we need to extract common part into function and have ability to pass "metadata" in.
;; Metadata in our case is a file name and length of numbers.
;; We can actually read a first number and infer length of numbers instead of hard-coding it.

(defn find-rating
  [type name]
  (let [filter-bit (fn [freq]
                     (if (= type :o2)
                       (if (<= (:zero freq) (:one freq)) \1 \0)
                       (if (<= (:zero freq) (:one freq)) \0 \1)))
        input (clojure.string/split-lines (slurp (str "resources/" name ".txt")))]
    (loop [input input
           idx   0]
      (let [freq       (frequencies-for-idx (prevalent-map input) idx)
            filter-bit (filter-bit freq)
            result     (filter (fn [x] (= (nth x idx) filter-bit)) input)]
        (if (nil? (second result))
          (first result)
          (recur result (inc idx)))))))

(find-rating :o2 "03_example")
(find-rating :co2 "03_example")

(Integer/parseInt (find-rating :o2 "03_example") 2)
(Integer/parseInt (find-rating :co2 "03_example") 2)


(find-rating :o2 "03_puzzle")
(find-rating :co2 "03_puzzle")


(Integer/parseInt (find-rating :o2 "03_puzzle") 2)
(Integer/parseInt (find-rating :co2 "03_puzzle") 2)

;; Puzzle result
(* (Integer/parseInt (find-rating :o2 "03_puzzle") 2)
   (Integer/parseInt (find-rating :co2 "03_puzzle") 2))

