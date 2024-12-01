(ns aoc2024.day1
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def puzzle-input (slurp "resources/day1.txt"))

(defn parse-input [input]
  (let [lines (mapv (fn [line] (filterv not-empty (str/split line #"\s")))
                    (filterv not-empty (str/split input #"\n")))
        left-line (mapv (comp read-string first) lines)
        right-line (mapv (comp read-string second) lines)]
    [left-line right-line]))

(defn part1 [input]
  (let [[left-line right-line] (parse-input input)
        left-line (sort left-line)
        right-line (sort right-line)
        diff (mapv (comp abs -) right-line left-line)]
    (apply + diff)))

(defn part2 [input]
  (let [[left-line right-line] (parse-input input)
        left-line (sort left-line)
        freq-score (frequencies right-line)
        similarity-score (mapv #(* (get freq-score % 0) %) left-line)]
    (apply + similarity-score)))

(deftest a-test
  (testing "part1"
    (is (= (part1 "3   4\n4   3\n2   5\n1   3\n3   9\n3   3") 11))
    (is (= (part1 puzzle-input) 936063))
    )
  (testing "part2"
    (is (= (part2 "3   4\n4   3\n2   5\n1   3\n3   9\n3   3") 31))
    (is (= (part2 puzzle-input) 23150395))
    )
  )