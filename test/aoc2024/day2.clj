(ns aoc2024.day2
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def puzzle-input (slurp "resources/day2.txt"))

(defn safe? [levels]
  (let [diff (map - levels (rest levels))
        min-level (apply min (mapv abs diff))
        max-level (apply max (mapv abs diff))]
    (and (every? #(not= % 0) diff)
         (or (every? pos? diff)
             (every? neg? diff))
         (>= min-level 1)
         (<= max-level 3))
    ))

(defn tolerate-safe? [levels]
  (let [possible-levels (map-indexed (fn [i v] (concat (take i levels)
                                                       (drop (inc i) levels))) levels)]
    (true? (some safe? possible-levels))
    ))

(defn parse-input [input]
  (mapv #(mapv read-string (str/split % #"\s")) (str/split input #"\n")))

(defn part1 [input]
  (let [levels (parse-input input)
        safe-levels (filter safe? levels)]
    (count safe-levels)))

(defn part2 [input]
  (let [levels (parse-input input)
        safe-levels (filter tolerate-safe? levels)]
    (count safe-levels)))

(deftest a-test
  (testing "safe?"
    (is (= (safe? [7 6 4 2 1]) true))
    (is (= (safe? [1 2 7 8 9]) false))
    (is (= (safe? [9 7 6 2 1]) false))
    (is (= (safe? [1 3 2 4 5]) false))
    (is (= (safe? [8 6 4 4 1]) false))
    (is (= (safe? [1 3 6 7 9]) true))
    )
  (testing "part1"
    (is (= (part1 "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9") 2))
    (is (= (part1 puzzle-input) 591))
    )
  (testing "tolerate-safe??"
    (is (= (tolerate-safe? [7 6 4 2 1]) true))
    (is (= (tolerate-safe? [1 2 7 8 9]) false))
    (is (= (tolerate-safe? [9 7 6 2 1]) false))
    (is (= (tolerate-safe? [1 3 2 4 5]) true))
    (is (= (tolerate-safe? [8 6 4 4 1]) true))
    (is (= (tolerate-safe? [1 3 6 7 9]) true))
    )
  (testing "part2"
    (is (= (part2 "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9") 4))
    (is (= (part2 puzzle-input) 621))
    )
  )
