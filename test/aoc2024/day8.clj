(ns aoc2024.day8
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "..........\n...#......\n..........\n....a.....\n..........\n.....a....\n..........\n......#...\n..........\n..........")
(def example-input2 "..........\n...#......\n#.........\n....a.....\n........a.\n.....a....\n..#.......\n......#...\n..........\n..........")
(def example-input3 "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............")
(def example-input4 "T.........\n...T......\n.T........\n..........\n..........\n..........\n..........\n..........\n..........\n..........")
(def puzzle-input (slurp "resources/day8.txt"))

(defn parse-graph [input]
  (let [lines (vec (str/split input #"\n"))
        lines (mapv #(str/split % #"") lines)
        cells (map-indexed (fn [row line] (map-indexed (fn [col cell] [[row col] cell]) line)) lines)
        cells (apply concat cells)]
    (into {} cells)
    )
  )

(defn diff [[ax ay] [bx by]]
  [(- ax bx) (- ay by)]
  )

(defn by-cells [graph]
  (into {} (for [[k v] (group-by first (mapv (fn [val] (vec (reverse val))) (into [] graph)))] [k (vec (apply concat (mapv rest v)))]))
  )

(defn repeat-antinode [graph x y dx dy]
  (loop [stop false
         current [x y]
         antinodes (set [])]
    (if stop
      antinodes
      (let [[x y] current
            next [(+ x dx) (+ y dy)]]
        (if (contains? graph next)
          (recur false next (conj antinodes next))
          (recur true next antinodes)
          )
        )
      )
    )
  )

(defn find-antinodes [repeatable graph graph-by-cells [x y]]
  (let [freq (get graph [x y])
        other-cells (filterv (fn [[ox oy]] (not= [x y] [ox oy])) (graph-by-cells freq))
        antinodes (mapv (fn [[ox oy]] (let [[dx dy] (diff [x y] [ox oy])]
                                        (if repeatable
                                          (repeat-antinode graph x y dx dy)
                                          [[(+ x dx) (+ y dy)]]
                                          )
                                        )) other-cells)]
    (apply concat antinodes)
    ))

(defn find-all-antinodes [repeatable graph graph-by-cells]
  (loop [cells (filterv (fn [[[x y] freq]] (not= "." freq)) (into [] graph))
         antinodes (set [])]
    (if (empty? cells)
      antinodes
      (let [[[x y]] (first cells)
            freq-antinodes (find-antinodes repeatable graph graph-by-cells [x y])
            freq-antinodes (filter #(contains? graph %) freq-antinodes)]
        (recur (rest cells) (into antinodes freq-antinodes))
        )
      )
    )
  )

(defn print-result [graph antinodes]
  (let [max-x (apply max (mapv first (keys graph)))
        max-y (apply max (mapv second (keys graph)))]
    (doseq [x (range (inc max-x))]
      (doseq [y (range (inc max-y))]
        (if (contains? antinodes [x y])
          (print "#")
          (print (get graph [x y]))
          )
        )
      (println)
      )
    )
  )

(defn part1 [input]
  (let [graph (parse-graph input)
        graph-by-cells (by-cells graph)
        all-antinodes (find-all-antinodes false graph graph-by-cells)
        all-antinodes (set all-antinodes)
        ;_ (print-result graph all-antinodes)
        ]
    (count all-antinodes)
    )
  )

(defn part2 [input]
  (let [graph (parse-graph input)
        graph-by-cells (by-cells graph)
        antennas (mapv first (filterv (fn [[[x y] freq]] (not= "." freq)) (into [] graph)))
        all-antinodes (find-all-antinodes true graph graph-by-cells)
        all-antinodes (set all-antinodes)
        ;_ (print-result graph all-antinodes)
        all-antinodes (into all-antinodes antennas)]
    (count all-antinodes)
    )
  )

(deftest a-test
  (testing "parse-graph"
    (is (= (parse-graph example-input) {[0 0] "." [0 1] "." [0 2] "." [0 3] "." [0 4] "." [0 5] "." [0 6] "." [0 7] "." [0 8] "." [0 9] "."
                                        [1 0] "." [1 1] "." [1 2] "." [1 3] "#" [1 4] "." [1 5] "." [1 6] "." [1 7] "." [1 8] "." [1 9] "."
                                        [2 0] "." [2 1] "." [2 2] "." [2 3] "." [2 4] "." [2 5] "." [2 6] "." [2 7] "." [2 8] "." [2 9] "."
                                        [3 0] "." [3 1] "." [3 2] "." [3 3] "." [3 4] "a" [3 5] "." [3 6] "." [3 7] "." [3 8] "." [3 9] "."
                                        [4 0] "." [4 1] "." [4 2] "." [4 3] "." [4 4] "." [4 5] "." [4 6] "." [4 7] "." [4 8] "." [4 9] "."
                                        [5 0] "." [5 1] "." [5 2] "." [5 3] "." [5 4] "." [5 5] "a" [5 6] "." [5 7] "." [5 8] "." [5 9] "."
                                        [6 0] "." [6 1] "." [6 2] "." [6 3] "." [6 4] "." [6 5] "." [6 6] "." [6 7] "." [6 8] "." [6 9] "."
                                        [7 0] "." [7 1] "." [7 2] "." [7 3] "." [7 4] "." [7 5] "." [7 6] "#" [7 7] "." [7 8] "." [7 9] "."
                                        [8 0] "." [8 1] "." [8 2] "." [8 3] "." [8 4] "." [8 5] "." [8 6] "." [8 7] "." [8 8] "." [8 9] "."
                                        [9 0] "." [9 1] "." [9 2] "." [9 3] "." [9 4] "." [9 5] "." [9 6] "." [9 7] "." [9 8] "." [9 9] "."}))
    )
  (testing "find-antinodes"
    (let [graph (parse-graph example-input2)]
      (is (= (find-antinodes false graph (by-cells graph) [3 4]) [[2 0] [1 3]]))
      (is (= (find-antinodes false graph (by-cells graph) [5 5]) [[7 6] [6 2]]))
      (is (= (find-antinodes false graph (by-cells graph) [4 8]) [[5 12] [3 11]]))
      )
    )
  (testing "part1"
    (is (= (part1 example-input3) 14))
    (is (= (part1 puzzle-input) 376))
    )
  (testing "part2"
    (is (= (part2 example-input3) 34))
    (is (= (part2 example-input4) 9))
    (is (= (part2 puzzle-input) 1352))
    )
  )