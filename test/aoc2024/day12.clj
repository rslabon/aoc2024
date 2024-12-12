(ns aoc2024.day12
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "AAAA\nBBCD\nBBCC\nEEEC")
(def example-input2 "OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO")
(def example-input3 "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE")
(def puzzle-input (slurp "resources/day12.txt"))

(defn parse-graph [input]
  (let [lines (str/split input #"\n")
        lines (mapv #(str/split % #"") lines)
        cells (map-indexed (fn [row line]
                             (map-indexed (fn [col cell]
                                            [[row col] cell]
                                            ) line)) lines)
        cells (apply concat cells)]
    (into {} cells)
    ))

(defn by-cell [graph]
  (group-by second (into [] graph))
  )

(defn adj? [[[ax ay]] [[bx by]]]
  (= 1 (+ (abs (- ax bx)) (abs (- ay by))))
  )

(defn group-by-region [cells-of-same-type]
  (loop [cells cells-of-same-type
         regions (set (mapv #(set [%]) cells-of-same-type))]
    (if (empty? cells)
      regions
      (let [cell (first cells)
            other-cells (remove #{cell} (set cells-of-same-type))
            adj-cells (filterv #(adj? cell %) other-cells)
            regions-to-merge (set (filterv #(not-empty (set/intersection % (into #{cell} adj-cells))) regions))
            regions (remove regions-to-merge regions)
            regions (conj regions (set (apply concat regions-to-merge)))]
        (recur (rest cells) regions)
        )
      )
    )
  )

(defn perimeter [graph [[x y] cell]]
  (let [dir [[1 0] [-1 0] [0 1] [0 -1]]
        adj-cells (mapv (fn [[dx dy]] (get graph [(+ x dx) (+ y dy)])) dir)
        out-of-bound-cells (count (filterv nil? adj-cells))
        adj-cells (filterv some? adj-cells)
        other-cells (filterv #(not= % cell) adj-cells)]
    (+ (count other-cells) out-of-bound-cells)
    )
  )

(defn perimeter-of-region [graph region]
  (let [perimeters (mapv #(perimeter graph %) region)]
    (apply + perimeters)
    )
  )

(defn part1 [input]
  (let [graph (parse-graph input)
        graph-by-cell (by-cell graph)
        types (set (vals graph))
        prices (flatten
                 (mapv (fn [type]
                         (let [regions (group-by-region (get graph-by-cell type))]
                           (mapv (fn [region] (let [perimeter (perimeter-of-region graph region)
                                                    area (count region)]
                                                (* area perimeter))) regions)
                           )) types))]
    (apply + prices)
    ))

(defn sides-of-region [graph region]
  -1
  )

(defn part2 [input]
  (let [graph (parse-graph input)
        graph-by-cell (by-cell graph)
        types (set (vals graph))
        prices (flatten
                 (mapv (fn [type]
                         (let [regions (group-by-region (get graph-by-cell type))]
                           (mapv (fn [region] (let [sides (sides-of-region graph region)
                                                    area (count region)]
                                                (* area sides))) regions)
                           )) types))]
    (apply + prices)
    ))

(deftest a-test
  (testing "part1"
    (is (= (part1 example-input) 140))
    (is (= (part1 example-input2) 772))
    (is (= (part1 example-input3) 1930))
    (is (= (part1 puzzle-input) 1533024))
    )
  (testing "part2"
    (is (= (part2 example-input) 80))
    ;(is (= (part1 example-input2) 772))
    ;(is (= (part1 example-input3) 1930))
    ;(is (= (part1 puzzle-input) 1533024))
    )
  )