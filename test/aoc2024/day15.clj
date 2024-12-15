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

(defn move-left [graph [rx ry] [sx _]]
  (let [nodes (filterv (fn [[[x y]]] (and (<= x rx) (> x sx) (= y ry))) graph)
        nodes (mapv (fn [[[x y] cell]] [[(- x 1) y] cell]) nodes)
        graph (reduce (fn [g [k v]] (assoc g k v)) graph nodes)
        graph (assoc graph [rx ry] ".")]
    graph
    ))

(defn move-right [graph [rx ry] [sx sy]]
  (let [nodes (filterv (fn [[[x y]]] (and (>= x rx) (< x sx) (= y ry))) graph)
        nodes (mapv (fn [[[x y] cell]] [[(+ x 1) y] cell]) nodes)
        graph (reduce (fn [g [k v]] (assoc g k v)) graph nodes)
        graph (assoc graph [rx ry] ".")]
    graph
    ))

(defn move-vertically [graph dy current-points all-prev-points]
  (let [current-points (sort-by first current-points)
        next-points (mapv (fn [[x y]] [x (+ y dy)]) current-points)
        next-cells (mapv (fn [[x y]] (get graph [x y])) next-points)]
    (cond
      (contains? (set next-cells) "#") graph
      (= (set next-cells) (set ["."])) (let [all-prev-points (into (set all-prev-points) current-points)
                                             new-graph (reduce (fn [g p] (assoc g p ".")) graph all-prev-points)
                                             new-graph (reduce (fn [g [x y]] (assoc g [x (+ y dy)] (get graph [x y]))) new-graph all-prev-points)]
                                         new-graph
                                         )
      :else (let [start-cell (first next-cells)
                  [sx sy] (first next-points)
                  end-cell (last next-cells)
                  [ex ey] (last next-points)
                  next-points (if (= start-cell "]")
                                (conj next-points [(- sx 1) sy])
                                next-points
                                )
                  next-points (if (and (= end-cell "[") (not= [sx sy] [ex ey]))
                                (conj next-points [(+ ex 1) ey])
                                next-points
                                )
                  next-points (if (and (= start-cell "[") (= [sx sy] [ex ey]))
                                (conj next-points [(+ ex 1) ey])
                                next-points
                                )
                  next-points (if (and (= start-cell "]") (= [sx sy] [ex ey]))
                                (conj next-points [(- ex 1) ey])
                                next-points
                                )
                  next-points (filterv #(not= "." (get graph %)) next-points)
                  all-prev-points (into (set all-prev-points) current-points)
                  ]
              (move-vertically graph dy (set next-points) all-prev-points)
              )
      )
    )
  )

(defn move-robot2 [graph move]
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
      :else (condp = move
              "<" (let [space (find-empty-left graph [rx ry])]
                    (if (nil? space)
                      graph
                      (move-left graph [rx ry] space)
                      )
                    )
              ">" (let [space (find-empty-right graph [rx ry])]
                    (if (nil? space)
                      graph
                      (move-right graph [rx ry] space)
                      ))
              "v" (move-vertically graph 1 [[rx ry]] [[rx ry]])
              "^" (move-vertically graph -1 [[rx ry]] [[rx ry]])
              )
      )
    ))

(defn sum-GPS-coordinates2 [graph]
  (let [boxes (filterv (fn [[_ cell]]
                         (= cell "[")
                         ) graph)
        boxes (mapv first boxes)
        boxes (mapv (fn [[x y]] (+ (* y 100) x)) boxes)]
    (apply + boxes)
    ))

(defn part2 [input]
  (let [input (-> input
                  (str/replace "." "..")
                  (str/replace "#" "##")
                  (str/replace "O" "[]")
                  (str/replace "@" "@.")
                  )
        [graph movements] (parse input)
        graph (loop [graph graph
                     movements movements]
                (if (empty? movements)
                  graph
                  (recur (move-robot2 graph (first movements)) (rest movements))
                  )
                )
        _ (print-graph graph)
        ]
    (sum-GPS-coordinates2 graph)
    ))

(deftest a-test
  (testing "part1"
    (is (= (part1 example-input) 2028))
    (is (= (part1 example-input2) 10092))
    (is (= (part1 puzzle-input) 1430536))
    )
  (testing "part2"
    (is (= (part2 example-input2) 9021))
    (is (= (part2 puzzle-input) 1452348))
    )
  )