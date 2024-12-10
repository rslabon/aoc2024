(ns aoc2024.day10
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input1 "0123\n1234\n8765\n9876")
(def example-input2 "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732")
(def puzzle-input (slurp "resources/day10.txt"))

(defn parse-graph [input]
  (let [lines (str/split input #"\n")
        lines (map #(str/split % #"") lines)
        lines (apply concat (map-indexed (fn [row line]
                                           (map-indexed (fn [col cell]
                                                          [[row col] (read-string cell)]
                                                          ) line)) lines))]
    (into {} lines)
    )
  )

(defn adj [graph [[x y] height]]
  (let [dirs [[1 0] [-1 0] [0 1] [0 -1]]
        coords (mapv (fn [[dx dy]] [(+ x dx) (+ y dy)]) dirs)
        coords (filterv #(contains? graph %) coords)
        cells (mapv #(vec [% (get graph %)]) coords)]
    (filterv (fn [[[_ _] next-height]] (= (- next-height height) 1)) cells)
    ))

(defn find-trails-count [graph [[x y] height]]
  (let [next-cells (adj graph [[x y] height])]
    (cond
      (= height 9) [[[x y] height]]
      (empty? next-cells) nil
      :else (set (apply concat (mapv #(find-trails-count graph %) next-cells)))
      )
  ))

(defn part1[input]
  (let [graph (parse-graph input)
        graph-by-height (group-by second (into [] graph))
        start-cells (get graph-by-height 0)
        trails (mapv #(find-trails-count graph %) start-cells)
        scores (mapv count trails)]
    (apply + scores)
    )
  )

(deftest a-test
  (let [example-graph {[0 0] 0 [0 1] 1 [0 2] 2 [0 3] 3
                       [1 0] 1 [1 1] 2 [1 2] 3 [1 3] 4
                       [2 0] 8 [2 1] 7 [2 2] 6 [2 3] 5
                       [3 0] 9 [3 1] 8 [3 2] 7 [3 3] 6}]
    (testing "parse-graph"
      (is (= (parse-graph example-input1) example-graph))
      )
    (testing "adj"
      (is (= (adj example-graph [[0 0] 0]) [[[1 0] 1]
                                            [[0 1] 1]]))
      (is (= (adj example-graph [[1 2] 3]) [[[1 3] 4]]))
      )
    (testing "part1"
      (is (= (part1 example-input1) 1))
      (is (= (part1 example-input2) 36))
      (is (= (part1 puzzle-input) 822))
      )
    )
  )