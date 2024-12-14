(ns aoc2024.day14
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3")
(def puzzle-input (slurp "resources/day14.txt"))

(defn parse-robots [input]
  (let [lines (str/split input #"\n")
        lines (mapv #(-> %
                         (str/replace "p=" "")
                         (str/replace " v=" ",")
                         ) lines)
        lines (mapv #(str/split % #",") lines)
        lines (mapv #(mapv read-string %) lines)
        ]
    lines
    )
  )

(defn fold [n nmin nmax]
  (if (and (>= n nmin) (< n nmax))
    n
    (cond (< n nmin) (+ nmax n)
          (>= n nmax) (mod n nmax))
    )
  )

(defn move-robot [px py vx vy xmin xmax ymin ymax seconds]
  (if (= seconds 0)
    [px py]
    (move-robot (fold (+ px vx) xmin xmax) (fold (+ py vy) ymin ymax) vx vy xmin xmax ymin ymax (dec seconds))
    )
  )

(defn in-quadrant? [xmin xmax ymin ymax [x y]]
  (and
    (>= x xmin)
    (< x xmax)
    (>= y ymin)
    (< y ymax)
    )
  )

(defn part1 [input xmax ymax]
  (let [robots (parse-robots input)
        xmid (int (Math/floor (/ xmax 2)))
        ymid (int (Math/floor (/ ymax 2)))
        robots-positions (mapv (fn [[px py vx vy]] (move-robot px py vx vy 0 xmax 0 ymax 100)) robots)
        q1-robots (filterv #(in-quadrant? 0 xmid 0 ymid %) robots-positions)
        q2-robots (filterv #(in-quadrant? (inc xmid) xmax 0 ymid %) robots-positions)
        q3-robots (filterv #(in-quadrant? 0 xmid (inc ymid) ymax %) robots-positions)
        q4-robots (filterv #(in-quadrant? (inc xmid) xmax (inc ymid) ymax %) robots-positions)
        robots-counts (mapv #(apply + (vals (frequencies %))) [q1-robots, q2-robots, q3-robots, q4-robots])

        ;robots-positions (frequencies q4-robots)
        ;_ (println (apply str (mapv (fn [y]
        ;                              (str (apply str (mapv (fn [x]
        ;                                                      (get robots-positions [x y] ".")
        ;                                                      ) (range xmax))) "\n")
        ;                              ) (range ymax))))
        ]
    (apply * robots-counts)
    )
  )

(deftest a-test
  (testing "move-robot"
    (is (= (move-robot 2 4 2 -3 0 11 0 7 1) [4 1]))
    (is (= (move-robot 4 1 2 -3 0 11 0 7 1) [6 5]))
    (is (= (move-robot 6 5 2 -3 0 11 0 7 1) [8 2]))
    (is (= (move-robot 8 2 2 -3 0 11 0 7 1) [10 6]))
    (is (= (move-robot 10 6 2 -3 0 11 0 7 1) [1 3]))
    (is (= (move-robot 2 4 2 -3 0 11 0 7 5) [1 3]))
    )
  (testing "part1"
    (is (= (part1 example-input 11 7) 12))
    (is (= (part1 puzzle-input 101 103) 230436441))
    )
  )