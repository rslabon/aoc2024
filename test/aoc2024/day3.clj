(ns aoc2024.day3
  (:require [clojure.test :refer :all]))

(def puzzle-input (slurp "resources/day3.txt"))

(defn sanitize [input]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)" input)
        instructions (mapv (fn [[_ arg1 arg2]] (str "(*" " " arg1 " " arg2 ")")) matches)]
    instructions
    ))

(defn evaluator [instructions]
  (apply + (mapv (comp eval read-string) instructions))
  )

(defn part1 [input]
  (let [instructions (sanitize input)]
    (evaluator instructions))
  )

(defn sanitize2 [input]
  (let [matches (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" input)
        instructions (mapv (fn [[group arg1 arg2]]
                             (cond
                               (.contains group "mul") (str "(*" " " arg1 " " arg2 ")")
                               (= group "do()") "do"
                               (= group "don't()") "don't")
                             ) matches)
        instructions (concat ["do"] instructions ["don't"])]
    instructions
    ))

(defn evaluator2 [instructions]
  (loop [instructions instructions
         skip false
         result 0]
    (if (empty? instructions)
      result
      (let [instruction (first instructions)]
        (cond (= instruction "do") (recur (rest instructions) false result)
              (= instruction "don't") (recur (rest instructions) true result)
              skip (recur (rest instructions) skip result)
              :else (recur (rest instructions) skip (+ result (eval (read-string instruction)))))
        )
      )
    ))

(defn part2 [input]
  (let [instructions (sanitize2 input)]
    (evaluator2 instructions))
  )

(deftest a-test
  (testing "sanitize"
    (is (= (sanitize "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))") ["(* 2 4)" "(* 5 5)" "(* 11 8)" "(* 8 5)"]))
    )
  (testing "evaluator"
    (is (= (evaluator ["(* 2 4)" "(* 5 5)" "(* 11 8)" "(* 8 5)"]) 161))
    )
  (testing "part1"
    (is (= (part1 puzzle-input) 168539636))
    )
  (testing "sanitize2"
    (is (= (sanitize2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))") ["do" "(* 2 4)" "don't" "(* 5 5)" "(* 11 8)" "do" "(* 8 5)" "don't"]))
    )
  (testing "evaluator2"
    (is (= (evaluator2 ["do" "(* 2 4)" "don't" "(* 5 5)" "(* 11 8)" "do" "(* 8 5)" "don't"]) 48))
    )
  (testing "part2"
    (is (= (part2 puzzle-input) 97529391))
    )
  )