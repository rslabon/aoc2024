(ns aoc2024.day15
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<")
(def example-input2 "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")
(def puzzle-input (slurp "resources/day15.txt"))

(defn parse [input]
  (let [[grid movements] (str/split input #"\n\n")
        grid-lines (str/split grid #"\n")
        grid-lines (mapv #(str/split % #"") grid-lines)
        grid (map-indexed (fn [y line]
                            (map-indexed (fn [x cell]
                                           [[x y] cell]
                                           ) line)
                            ) grid-lines)
        grid (apply concat grid)
        graph (into {} grid)
        movements (str/replace movements "\n" "")
        movements (str/split movements #"")
        ]
    [graph movements]
    )
  )

(defn find-empty-right [graph [px py]]
  (let [nodes (filterv (fn [[[x y]]] (and (> x px) (= y py))) graph)
        nodes (sort-by first nodes)
        nodes (take-while (fn [[_ cell]] (not= cell "#")) nodes)
        nodes (filterv (fn [[_ cell]] (= cell ".")) nodes)
        points (mapv first nodes)]
    (if (empty? points)
      nil
      (first points)
      )
    )
  )

(defn find-empty-left [graph [px py]]
  (let [nodes (filterv (fn [[[x y]]] (and (< x px) (= y py))) graph)
        nodes (reverse (sort-by first nodes))
        nodes (take-while (fn [[_ cell]] (not= cell "#")) nodes)
        nodes (filterv (fn [[_ cell]] (= cell ".")) nodes)
        points (mapv first nodes)]
    (if (empty? points)
      nil
      (first points)
      )
    )
  )

(defn find-empty-down [graph [px py]]
  (let [nodes (filterv (fn [[[x y]]] (and (= x px) (> y py))) graph)
        nodes (sort-by first nodes)
        nodes (take-while (fn [[_ cell]] (not= cell "#")) nodes)
        nodes (filterv (fn [[_ cell]] (= cell ".")) nodes)
        points (mapv first nodes)]
    (if (empty? points)
      nil
      (first points)
      )
    )
  )

(defn find-empty-up [graph [px py]]
  (let [nodes (filterv (fn [[[x y]]] (and (= x px) (< y py))) graph)
        nodes (reverse (sort-by first nodes))
        nodes (take-while (fn [[_ cell]] (not= cell "#")) nodes)
        nodes (filterv (fn [[_ cell]] (= cell ".")) nodes)
        points (mapv first nodes)]
    (if (empty? points)
      nil
      (first points)
      )
    )
  )

(defn find-robot [graph]
  (first (first (filterv (fn [[_ cell]] (= cell "@")) graph)))
  )

(defn print-graph [graph]
  (let [points (mapv first graph)
        xmax (apply max (mapv first points))
        ymax (apply max (mapv second points))]
    (println (apply str (mapv (fn [y]
                                (str (apply str (mapv (fn [x]
                                                        (get graph [x y] " ")
                                                        ) (range (inc xmax)))) "\n")
                                ) (range (inc ymax)))))
    ))

(defn move-robot [graph move]
  (let [[rx ry] (find-robot graph)
        [nx ny] (condp = move
                  "<" [(+ rx -1) ry]
                  ">" [(+ rx 1) ry]
                  "v" [rx (+ ry 1)]
                  "^" [rx (+ ry -1)]
                  )
        next-cell (get graph [nx ny])]
    (cond
      (= next-cell "#") graph
      (= next-cell ".") (-> graph
                            (assoc [rx ry] ".")
                            (assoc [nx ny] "@"))
      :else (let [empty-space (condp = move
                                "<" (find-empty-left graph [rx ry])
                                ">" (find-empty-right graph [rx ry])
                                "v" (find-empty-down graph [rx ry])
                                "^" (find-empty-up graph [rx ry])
                                )]
              (if (nil? empty-space)
                graph
                (-> graph
                    (assoc [rx ry] ".")
                    (assoc [nx ny] "@")
                    (assoc empty-space next-cell))
                )
              )
      )
    ))

(defn sum-GPS-coordinates [graph]
  (let [boxes (filterv (fn [[_ cell]]
                         (= cell "O")
                         ) graph)
        boxes (mapv first boxes)
        boxes (mapv (fn [[x y]] (+ (* y 100) x)) boxes)]
    (apply + boxes)
    ))

(defn part1 [input]
  (let [[graph movements] (parse input)
        graph (loop [graph graph
                     movements movements]
                (if (empty? movements)
                  graph
                  (recur (move-robot graph (first movements)) (rest movements))
                  )
                )
        _ (print-graph graph)
        ]
    (sum-GPS-coordinates graph)
    ))

(deftest a-test
  (testing "part1"
    (is (= (part1 example-input) 2028))
    (is (= (part1 example-input2) 10092))
    (is (= (part1 puzzle-input) 1430536))
    )
  )