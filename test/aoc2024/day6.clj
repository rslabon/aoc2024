(ns aoc2024.day6
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#...")

(def puzzle-input (slurp "resources/day6.txt"))

(defn parse-graph [input]
  (let [lines (str/split input #"\n")
        cells (mapv #(str/split % #"") lines)
        cells (map-indexed (fn [row-idx row] (map-indexed (fn [col-idx cell] [[row-idx col-idx] cell]) row)) cells)
        cells (apply concat cells)
        graph (into {} cells)]
    graph
    ))

(defn turn-right [dir]
  (condp = dir "^" ">"
               ">" "v"
               "v" "<"
               "<" "^"
               ))

(defn move [[x y] dir]
  (condp = dir "^" [(- x 1) y]
               ">" [x (+ y 1)]
               "v" [(+ x 1) y]
               "<" [x (- y 1)]
               )
  )

(defn find-guard [graph]
  (let [[[x y] dir] (first (filterv #(= (second %) "^") graph))]
    {:coord [x y] :dir dir}
    )
  )

(defn next-step [guard graph]
  (let [next-coord (move (:coord guard) (:dir guard))
        cell (get graph next-coord)]
    (cond
      (= cell "#") (assoc guard :dir (turn-right (:dir guard)))
      (= cell ".") (assoc guard :coord next-coord)
      (= cell "^") (assoc guard :coord next-coord)
      :else nil
      )
    )
  )

(defn find-path [graph]
  (let [guard (find-guard graph)
        ;_ (println graph)
        ]
    (loop [prev-guard guard
           path [guard]]
      (let [next-guard (next-step prev-guard graph)
            ;_ (println prev-guard next-guard)
            ]
        (if (nil? next-guard)
          (mapv :coord path)
          (recur next-guard (conj path next-guard))
          )
        )
      )
    )
  )

(defn part1 [input]
  (let [graph (parse-graph input)
        path (find-path graph)]
    (count (set path))
    )
  )

(defn cycle? [path]
  (let [coords-freq (frequencies path)]
    (> (apply max (vals coords-freq)) 10)
    )
  )

(defn leads-to-cycle? [graph]
  (let [guard (find-guard graph)]
    (loop [prev-guard guard
           path [guard]]
      (let [next-guard (next-step prev-guard graph)]
        (cond
          (nil? next-guard) false
          (cycle? (mapv :coord path)) true
          :else (recur next-guard (conj path next-guard))
          )
        )
      )
    )
  )

(defn has-obstacle? [graph [x y]]
  (let [vertical-obstacle (some (fn [[gx gy]] (and (= x gx) (= (get graph [gx gy]) "#"))) (keys graph))
        horizontal-obstacle (some (fn [[gx gy]] (and (= y gy) (= (get graph [gx gy]) "#"))) (keys graph))]
    (or vertical-obstacle horizontal-obstacle)
    )
  )

(defn part2 [input]
  (let [graph (parse-graph input)
        all_positions (filterv #(and (not= (get graph %) "#") (not= (get graph %) "^")) (keys graph))
        guard (find-guard graph)
        path (vec (set (find-path graph)))
        path (filterv #(not= (:coord guard) %) path)
        ]
    (loop [positions path
           cycle-count 0
           i 1]
      (if (empty? positions)
        cycle-count
        (let [position (first positions)
              new-graph (assoc graph position "#")
              _ (println "check " i "/" (count path))]
          (if (leads-to-cycle? new-graph)
            (recur (rest positions) (inc cycle-count) (inc i))
            (recur (rest positions) cycle-count (inc i))
            )
          )
        )
      )
    )
  )

(let [example-graph {[0 0] "." [0 1] "." [0 2] "." [0 3] "." [0 4] "#" [0 5] "." [0 6] "." [0 7] "." [0 8] "." [0 9] "."
                     [1 0] "." [1 1] "." [1 2] "." [1 3] "." [1 4] "." [1 5] "." [1 6] "." [1 7] "." [1 8] "." [1 9] "#"
                     [2 0] "." [2 1] "." [2 2] "." [2 3] "." [2 4] "." [2 5] "." [2 6] "." [2 7] "." [2 8] "." [2 9] "."
                     [3 0] "." [3 1] "." [3 2] "#" [3 3] "." [3 4] "." [3 5] "." [3 6] "." [3 7] "." [3 8] "." [3 9] "."
                     [4 0] "." [4 1] "." [4 2] "." [4 3] "." [4 4] "." [4 5] "." [4 6] "." [4 7] "#" [4 8] "." [4 9] "."
                     [5 0] "." [5 1] "." [5 2] "." [5 3] "." [5 4] "." [5 5] "." [5 6] "." [5 7] "." [5 8] "." [5 9] "."
                     [6 0] "." [6 1] "#" [6 2] "." [6 3] "." [6 4] "^" [6 5] "." [6 6] "." [6 7] "." [6 8] "." [6 9] "."
                     [7 0] "." [7 1] "." [7 2] "." [7 3] "." [7 4] "." [7 5] "." [7 6] "." [7 7] "." [7 8] "#" [7 9] "."
                     [8 0] "#" [8 1] "." [8 2] "." [8 3] "." [8 4] "." [8 5] "." [8 6] "." [8 7] "." [8 8] "." [8 9] "."
                     [9 0] "." [9 1] "." [9 2] "." [9 3] "." [9 4] "." [9 5] "." [9 6] "#" [9 7] "." [9 8] "." [9 9] "."}]
  (deftest a-test
    (testing "parse-graph"
      (is (= (parse-graph example-input) example-graph))
      )
    (testing "turn-right"
      (is (= (turn-right "^") ">"))
      (is (= (turn-right ">") "v"))
      (is (= (turn-right "v") "<"))
      (is (= (turn-right "<") "^"))
      )
    (testing "find-guard"
      (is (= (find-guard example-graph) {:coord [6 4] :dir "^"}))
      )
    (testing "part1"
      (is (= (part1 example-input) 41))
      (is (= (part1 puzzle-input) 5153))
      )
    ;(testing "cycle?"
    ;  (is (= (cycle? [[0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [0 8] [0 9]]) false))
    ;  (is (= (cycle? [[0 1] [0 2] [0 3] [0 1] [0 2] [0 3] [0 7] [0 8] [0 9]]) false))
    ;  (is (= (cycle? [[0 1] [0 2] [0 3] [0 4] [0 1] [0 2] [0 3] [0 4]]) true))
    ;  (is (= (cycle? [[0 1] [0 2] [0 3] [0 4] [1 1] [1 2] [1 3] [1 5] [0 1] [0 2] [0 4]]) false))
    ;  )
    (testing "part2"
      (is (= (part2 example-input) 6))
      (is (= (part2 puzzle-input) 0))
      )
    ))
