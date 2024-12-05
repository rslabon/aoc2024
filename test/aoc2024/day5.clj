(ns aoc2024.day5
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(def example-input "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47")

(def puzzle-input (slurp "resources/day5.txt"))

(defn parse-update-manual [input]
  (let [[rules updates] (str/split input #"\n\n")
        rules (mapv #(str/split % #"\|") (str/split rules #"\n"))
        rules (mapv #(mapv read-string %) rules)
        updates (mapv #(str/split % #",") (str/split updates #"\n"))
        updates (mapv #(mapv read-string %) updates)]
    {:rules rules :updates updates}
    ))

(defn filter-rules-for-update [rules update]
  (let [update-numbers (set update)
        rules-for-update (filterv #(set/subset? (set %) update-numbers) rules)
        ]
    rules-for-update
    ))

(defn numbers-by-index [update]
  (into {} (map-indexed (fn [i v] [v i]) update)))

(defn valid-update? [rules update]
  (let [indexes (numbers-by-index update)
        rules-for-update (filter-rules-for-update rules update)]
    (every? (fn [rule]
              (let [first-number-place (get indexes (first rule))
                    second-number-place (get indexes (second rule))]
                (< first-number-place second-number-place)
                )
              ) rules-for-update)
    ))

(defn filter-valid-updates [rules updates]
  (filterv #(valid-update? rules %) updates)
  )

(defn middle-pager-number [numbers]
  (let [middle (int (Math/floor (/ (count numbers) 2)))]
    (nth numbers middle)
    )
  )

(defn part1 [input]
  (let [{:keys [rules updates]} (parse-update-manual input)
        valid-updates (filter-valid-updates rules updates)
        middle-pager-numbers (mapv middle-pager-number valid-updates)]
    (apply + middle-pager-numbers)
    )
  )

(defn filter-invalid-updates [rules updates]
  (filterv #(not (valid-update? rules %)) updates)
  )

(defn fix-update [rules invalid-update]
  (let [rules-for-update (filter-rules-for-update rules invalid-update)
        indexes (numbers-by-index invalid-update)]
    (sort-by identity (fn [a b]
                        (let [rule (first (filterv #(set/subset? (set [a b]) (set %)) rules-for-update))
                              first-number-place (get indexes (first rule))
                              second-number-place (get indexes (second rule))]
                          (> first-number-place second-number-place)
                          )
                        ) invalid-update)
    ))


(defn part2 [input]
  (let [{:keys [rules updates]} (parse-update-manual input)
        invalid-updates (filter-invalid-updates rules updates)
        fixed-updates (mapv #(fix-update rules %) invalid-updates)
        middle-pager-numbers (mapv middle-pager-number fixed-updates)]
    (apply + middle-pager-numbers)
    )
  )

(let [example-manual {:rules   [[47 53] [97 13] [97 61] [97 47] [75 29] [61 13] [75 53] [29 13] [97 29] [53 29] [61 53] [97 53] [61 29] [47 13] [75 47] [97 75] [47 61] [75 61] [47 29] [75 13] [53 13]]
                      :updates [[75 47 61 53 29]
                                [97 61 53 29 13]
                                [75 29 13]
                                [75 97 47 61 53]
                                [61 13 29]
                                [97 13 75 29 47]]}]
  (deftest a-test
    (testing "parse-update-manual"
      (is (= (parse-update-manual example-input) example-manual))
      )
    (testing "valid-update?"
      (is (= (valid-update? (:rules example-manual) [75, 47, 61, 53, 29]) true))
      (is (= (valid-update? (:rules example-manual) [97, 61, 53, 29, 13]) true))
      (is (= (valid-update? (:rules example-manual) [75, 29, 13]) true))
      (is (= (valid-update? (:rules example-manual) [75, 97, 47, 61, 53]) false))
      (is (= (valid-update? (:rules example-manual) [61, 13, 29]) false))
      (is (= (valid-update? (:rules example-manual) [97, 13, 75, 29, 47]) false))
      )
    (testing "filter-valid-updates"
      (is (= (filter-valid-updates (:rules example-manual) (:updates example-manual)) [[75 47 61 53 29]
                                                                                       [97 61 53 29 13]
                                                                                       [75 29 13]]))
      )
    (testing "middle-pager-number"
      (is (= (middle-pager-number [75 47 61 53 29]) 61))
      (is (= (middle-pager-number [97 61 53 29 13]) 53))
      (is (= (middle-pager-number [75 29 13]) 29))
      )
    (testing "part1"
      (is (= (part1 example-input) 143))
      (is (= (part1 puzzle-input) 4924))
      )
    (testing "fix-update"
      (is (= (fix-update (:rules example-manual) [75, 97, 47, 61, 53]) [97, 75, 47, 61, 53]))
      (is (= (fix-update (:rules example-manual) [61, 13, 29]) [61, 29, 13]))
      (is (= (fix-update (:rules example-manual) [97, 13, 75, 29, 47]) [97, 75, 47, 29, 13]))
      )
    (testing "part2"
      (is (= (part2 example-input) 123))
      (is (= (part2 puzzle-input) 6085))
      )
    ))
