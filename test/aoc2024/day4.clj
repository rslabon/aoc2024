(ns aoc2024.day4
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def puzzle-input (slurp "resources/day4.txt"))

(defn into-graph [input]
  (let [lines (str/split input #"\n")
        cells (map-indexed (fn [row line]
                             (map-indexed (fn [col value] [row col value]) (str/split line #""))
                             ) lines)
        cells (apply concat cells)
        g (reduce (fn [m [row col value]] (assoc m [row col] value)) {} cells)]
    g
    )
  )

(defn next-left [[r c]]
  [r (dec c)]
  )

(defn next-right [[r c]]
  [r (inc c)]
  )

(defn next-down [[r c]]
  [(inc r) c]
  )

(defn next-up [[r c]]
  [(dec r) c]
  )

(defn next-diag-down-right [[r c]]
  [(inc r) (inc c)]
  )

(defn next-diag-down-left [[r c]]
  [(inc r) (dec c)]
  )

(defn next-diag-up-left [[r c]]
  [(dec r) (dec c)]
  )

(defn next-diag-up-right [[r c]]
  [(dec r) (inc c)]
  )

(defn take-word [next-fn n row col graph]
  (let [coords (loop [i 1
                      coords [[row col]]]
                 (if (= i n)
                   coords
                   (recur (inc i) (conj coords (next-fn (last coords))))
                   ))
        chars (mapv #(get graph %) coords)
        chars (filterv #(not (nil? %)) chars)]
    (if (= (count chars) n)
      (apply str chars)
      nil
      )
    ))

(defn find-word [word row col graph]
  (let [directions [next-left next-right next-down next-up next-diag-down-right next-diag-down-left next-diag-up-left next-diag-up-right]
        words (mapv #(take-word % (count word) row col graph) directions)
        words (filterv not-empty words)
        found-words (filterv #(= word %) words)]
    (count found-words)
    )
  )

(defn part1 [input]
  (let [graph (into-graph input)
        coords (keys graph)
        occurrences (mapv (fn [[r c]] (find-word "XMAS" r c graph)) coords)]
    (apply + occurrences))
  )

(defn part2 [input]
  (let [word "MAS"
        graph (into-graph input)
        coords (keys graph)
        occurrences (mapv (fn [[r c]]
                   (let [diag-word-x (take-word next-diag-down-right (count word) r c graph)
                         diag-word-y (take-word next-diag-down-left (count word) r (dec (+ (count word) c)) graph)]
                     (if (or (nil? diag-word-x) (nil? diag-word-y))
                       0
                       (if (and
                             (or (= word diag-word-x) (= word (str/reverse diag-word-x)))
                             (or (= word diag-word-y) (= word (str/reverse diag-word-y)))
                             )
                         1
                         0)
                       )
                     )
                   ) coords)]
    (apply + occurrences)
  ))

(deftest a-test
  (testing "into-graph"
    (is (= (into-graph "..X...\n.SAMX.\n.A..A.\nXMAS.S\n.X....") {[0 0] "."
                                                                  [0 1] "."
                                                                  [0 2] "X"
                                                                  [0 3] "."
                                                                  [0 4] "."
                                                                  [0 5] "."
                                                                  [1 0] "."
                                                                  [1 1] "S"
                                                                  [1 2] "A"
                                                                  [1 3] "M"
                                                                  [1 4] "X"
                                                                  [1 5] "."
                                                                  [2 0] "."
                                                                  [2 1] "A"
                                                                  [2 2] "."
                                                                  [2 3] "."
                                                                  [2 4] "A"
                                                                  [2 5] "."
                                                                  [3 0] "X"
                                                                  [3 1] "M"
                                                                  [3 2] "A"
                                                                  [3 3] "S"
                                                                  [3 4] "."
                                                                  [3 5] "S"
                                                                  [4 0] "."
                                                                  [4 1] "X"
                                                                  [4 2] "."
                                                                  [4 3] "."
                                                                  [4 4] "."
                                                                  [4 5] "."}))
    )
  (testing "take-word"
    (let [graph (into-graph "....XXMAS.\n.SAMXMS...\n...S..A...\n..A.A.MS.X\nXMASAMX.MM\nX.....XA.A\nS.S.S.S.SS\n.A.A.A.A.A\n..M.M.M.MM\n.X.X.XMASX")]
      (is (= (take-word next-right 10 0 0 graph) "....XXMAS."))
      (is (= (take-word next-right 9 0 0 graph) "....XXMAS"))
      (is (= (take-word next-right 11 0 0 graph) nil))
      ))
  (testing "part1"
    (is (= (part1 puzzle-input) 2591))
    )
  (testing "part2"
    (is (= (part2 ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n..........") 9))
    (is (= (part2 puzzle-input) 1880))
    )
  )
