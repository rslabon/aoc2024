(ns aoc2024.day13
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279")
(def puzzle-input (slurp "resources/day13.txt"))

(defn cost-of-prize [[ax ay] [bx by] [X Y]]
  (let [possible (set (for [i (range (inc 100))
                            j (range (inc 100))
                            :when (and (= X (+ (* (bigint i) ax) (* (bigint j) bx))) (= Y (+ (* (bigint i) ay) (* (bigint j) by))))
                            ] [(bigint i) (bigint j)]))]
    (if (empty? possible)
      nil
      (first (sort (mapv (fn [[a b]] (+ (* a 3) (* b 1))) possible)))
      )
    )
  )

(defn parse-configurations [input]
  (let [blocks (str/split input #"\n\n")
        configurations (mapv (fn [block]
                               (let [[a-line b-line prize-line] (str/split block #"\n")
                                     a-line (-> a-line
                                                (str/replace "Button A: X+" "")
                                                (str/replace " Y+" "")
                                                (str/split #",")
                                                )
                                     b-line (-> b-line
                                                (str/replace "Button B: X+" "")
                                                (str/replace " Y+" "")
                                                (str/split #",")
                                                )
                                     prize-line (-> prize-line
                                                    (str/replace "Prize: X=" "")
                                                    (str/replace " Y=" "")
                                                    (str/split #",")
                                                    )
                                     ]
                                 [(mapv read-string a-line) (mapv read-string b-line) (mapv read-string prize-line)]
                                 )
                               ) blocks)]
    configurations
    )
  )

(defn part1 [input]
  (let [configurations (parse-configurations input)
        costs (filterv some? (map-indexed
                               (fn [idx configuration]
                                 (do
                                   (println "processing " idx "/" (count configurations) " configuration=" configuration)
                                   (apply cost-of-prize configuration))
                                 ) configurations))
        ]
    (apply + costs)
    )
  )

(deftest a-test
  (testing "cost-of-prize"
    (is (= (cost-of-prize [94 34] [22 67] [8400 5400]) 280))
    (is (= (cost-of-prize [26 66] [67 21] [12748 12176]) nil))
    (is (= (cost-of-prize [17 86] [84 37] [7870 6450]) 200))
    (is (= (cost-of-prize [69 23] [27 71] [18641 10279]) nil))
    )
  (testing "part1"
    (is (= (part1 example-input) 480))
    (is (= (part1 puzzle-input) 35997))
    )
  )