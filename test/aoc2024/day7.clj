(ns aoc2024.day7
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20")

(def puzzle-input (slurp "resources/day7.txt"))

(defn compute [operators numbers]
  (let [operators (vec operators)
        numbers (vec numbers)
        result (apply (first operators) (take 2 numbers))]
    (loop [operators (rest operators)
           numbers (drop 2 numbers)
           result result]
      (if (empty? operators)
        result
        (recur (rest operators) (rest numbers) ((first operators) result (first numbers)))
        )
      )
    )
  )

(defn concat-op [a b]
  (read-string (str a b))
  )

(def selection
  (memoize (fn [coll k]
             (cond
               (= k 0) []
               (= k 1) (mapv list coll)
               :else (for [i coll
                           j (selection coll (dec k))]
                       (into [i] j))
               ))))

(defn valid?
  ([op-fns result numbers]
   (let [n (dec (count numbers))
         possible-operators (selection op-fns n)]
     (true? (some (fn [operators] (= result (compute operators numbers))) possible-operators))
     )
   )
  )

(defn parse-equations [input]
  (let [input (str/replace input ":" "")
        lines (str/split input #"\n")
        lines (mapv #(str/split % #" ") lines)
        lines (mapv #(mapv read-string %) lines)]
    lines
    )
  )

(defn part1 [input]
  (let [equations (parse-equations input)
        valid-equations (filterv #(valid? [+ *] (first %) (rest %)) equations)]
    (apply + (mapv first valid-equations))
    )
  )

(defn part2 [input]
  (let [equations (parse-equations input)
        valid-equations (filterv #(valid? [+ * concat-op] (first %) (rest %)) equations)]
    (apply + (mapv first valid-equations))
    )
  )

(deftest a-test
  (testing "selection"
    (is (= (selection [0 1] 1) (combo/selections [0 1] 1)))
    (is (= (selection [0 1] 2) (combo/selections [0 1] 2)))
    (is (= (selection [0 1] 3) (combo/selections [0 1] 3)))
    (is (= (selection [0 1] 4) (combo/selections [0 1] 4)))
    (is (= (selection [0 1] 5) (combo/selections [0 1] 5)))
    (is (= (selection [0 1] 6) (combo/selections [0 1] 6)))
    (is (= (selection [0 1] 7) (combo/selections [0 1] 7)))
    (is (= (selection [0 1] 8) (combo/selections [0 1] 8)))
    (is (= (selection [0 1] 9) (combo/selections [0 1] 9)))
    (is (= (selection [0 1] 10) (combo/selections [0 1] 10)))
    (is (= (selection [0 1] 11) (combo/selections [0 1] 11)))
    )
  (testing "valid?"
    (is (= (valid? [+ *] 3267 [81 40 27]) true))
    (is (= (valid? [+ *] 292 [11 6 16 20]) true))
    (is (= (valid? [+ *] 21037 [9 7 18 13]) false))
    )
  (testing "parse-equations"
    (is (= (parse-equations example-input) [[190 10 19]
                                            [3267 81 40 27]
                                            [83 17 5]
                                            [156 15 6]
                                            [7290 6 8 6 15]
                                            [161011 16 10 13]
                                            [192 17 8 14]
                                            [21037 9 7 18 13]
                                            [292 11 6 16 20]]))
    )
  (testing "part1"
    (is (= (part1 example-input) 3749))
    (is (= (part1 puzzle-input) 538191549061))
    )
  (testing "part2"
    (is (= (part2 example-input) 11387))
    (is (= (part2 puzzle-input) 34612812972206))
    )
  )