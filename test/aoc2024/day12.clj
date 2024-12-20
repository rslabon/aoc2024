(ns aoc2024.day12
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "AAAA\nBBCD\nBBCC\nEEEC")
(def example-input2 "OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO")
(def example-input3 "EEEEE\nEXXXX\nEEEEE\nEXXXX\nEEEEE")
(def example-input4 "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE")
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

(defn condense [items]
  (cond
    (empty? items) []
    (= (count items) 1) items
    :else (loop [result [(first items)]
                 prev (first items)
                 items (rest items)]
            (if (empty? items)
              result
              (if (= 1 (apply + (mapv - (first items) prev)))
                (recur result (first items) (rest items))
                (recur (conj result (first items)) (first items) (rest items))
                )
              )
            )
    )
  )

(defn sides-above-and-bellow [graph points [dx dy]]
  (if (empty? points)
    0
    (let [type (get graph (first points))
          above (mapv (fn [[x y]] [[x y] (get graph [(- x dx) (- y dy)] :outside)]) points)
          above (filterv #(not= type (second %)) above)
          above (mapv first above)
          above (condense above)
          bellow (mapv (fn [[x y]] [[x y] (get graph [(+ x dx) (+ y dy)] :outside)]) points)
          bellow (filterv #(not= type (second %)) bellow)
          bellow (mapv first bellow)
          bellow (condense bellow)]
      (+ (count above) (count bellow))
      )
    )
  )

(defn sides-of-region [graph region]
  (let [points (mapv first region)
        hpoints (group-by first points)
        hpoints (sort-by first hpoints)
        hpoints (mapv #(sort-by second %) (vals hpoints))
        hsides (apply + (mapv #(sides-above-and-bellow graph % [1 0]) hpoints))
        vpoints (group-by second points)
        vpoints (sort-by second vpoints)
        vpoints (mapv #(sort-by first %) (vals vpoints))
        vsides (apply + (mapv #(sides-above-and-bellow graph % [0 1]) vpoints))]
    (+ hsides vsides)
    )
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
    (is (= (part1 example-input4) 1930))
    (is (= (part1 puzzle-input) 1533024))
    )
  (testing "part2"
    (is (= (part2 example-input) 80))
    (is (= (part2 example-input2) 436))
    (is (= (part2 example-input3) 236))
    (is (= (part2 example-input4) 1206))
    (is (= (part2 puzzle-input) 910066))
    )
  )