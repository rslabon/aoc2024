(ns aoc2024.day9
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def puzzle-input (str/trim (slurp "resources/day9.txt")))

(defn parse-disk-map [line]
  (loop [digits (mapv read-string (str/split line #""))
         file-id 0
         file true
         result []]
    (if (empty? digits)
      (filterv not-empty result)
      (let [digit (first digits)]
        (if file
          (recur (rest digits) (inc file-id) false (vec (concat result (take digit (repeat (str file-id))))))
          (recur (rest digits) file-id true (vec (concat result (take digit (repeat ".")))))
          )
        ))))

(defn compress [disk-map]
  (let [disk-map-reverted (reverse (filterv #(not= % ".") disk-map))
        file-space (count (filterv #(not= "." %) disk-map))]
    (loop [disk-map-reverted disk-map-reverted
           disk-map disk-map
           result []]
      (if (empty? disk-map)
        (vec (take file-space result))
        (if (= (first disk-map) ".")
          (recur (rest disk-map-reverted) (rest disk-map) (conj result (first disk-map-reverted)))
          (recur disk-map-reverted (rest disk-map) (conj result (first disk-map)))
          )
        )
      )
    )
  )

(defn checksum [disk-map]
  (let [disk-map (filterv #(not= % ".") disk-map)]
    (loop [acc 0
           i 0
           disk-map disk-map]
      (if (empty? disk-map)
        acc
        (recur (+ acc (* i (read-string (first disk-map)))) (inc i) (rest disk-map))
        )
      )
    ))

(defn part1 [input]
  (let [disk-map (parse-disk-map input)
        compressed-disk-map (compress disk-map)]
    (checksum compressed-disk-map)
    )
  )

(defn parse-disk-map-block [line]
  (loop [digits (mapv read-string (str/split line #""))
         file-id 0
         file true
         result []]
    (if (empty? digits)
      (filterv (fn [[digit]] (not= digit 0)) result)
      (let [digit (first digits)]
        (if file
          (recur (rest digits) (inc file-id) false (conj result [digit (str file-id)]) )
          (recur (rest digits) file-id true (conj result [digit "."]))
          )
        ))))

(defn fragmentation
  ([blocks file-blocks]
   (if (empty? file-blocks)
     blocks
     (let [file-block (first file-blocks)
           [file-block-digit] file-block
           free-blocks (filterv (fn [[digit id]] (and (= id ".") (>= digit file-block-digit))) blocks)]
       (if (empty? free-blocks)
         (recur blocks (rest file-blocks))
         (let [free-block (first free-blocks)
               [free-block-space _] free-block
               lefts-space (- free-block-space file-block-digit)
               free-block-place (.indexOf blocks free-block)
               file-block-place (.indexOf blocks file-block)]
           (if (< free-block-place file-block-place)
             (if (= lefts-space 0)
               (recur (mapv #(cond
                               (identical? % free-block) file-block
                               (identical? % file-block) free-block
                               :else %
                               ) blocks) (rest file-blocks))
               (recur (apply concat
                             (mapv #(cond
                                      (identical? % free-block) [file-block [lefts-space "."]]
                                      (identical? % file-block) [[file-block-digit "."]]
                                      :else [%]
                                      ) blocks)) (rest file-blocks))

               )
             (recur blocks (rest file-blocks))
             )
           )
         )
       )))
  ([disk-map]
   (loop [disk-map disk-map]
     (let [file-blocks (reverse (filterv (fn [[digit block]] (not= "." block)) disk-map))]
       (fragmentation (vec disk-map) (vec file-blocks))
       )
     )
   ))

(defn disk-map-as-string [diskmap-block]
  (apply str (apply concat (mapv #(repeat (first %) (second %)) diskmap-block))))

(defn checksum-block [disk-map-block]
  (loop [acc 0
         i 0
         disk-map-block disk-map-block]
    (if (empty? disk-map-block)
      acc
      (let [block (first disk-map-block)
            [digit id] block]
        (recur (if (= id ".")
                 acc
                 (let [value (mapv #(* (+ i %) (read-string id)) (range digit))]
                   (+ acc (apply + value)))
                 ) (+ i digit) (rest disk-map-block))
        )
      )
    )
  )

(defn part2 [input]
  (let [disk-map (parse-disk-map-block input)]
    (checksum-block (fragmentation disk-map))
    )
  )

(deftest a-test
  (testing "disk-map"
    (is (= (parse-disk-map "03") ["." "." "."]))
    (is (= (parse-disk-map "01") ["."]))
    (is (= (parse-disk-map "10") ["0"]))
    (is (= (parse-disk-map "30") ["0" "0" "0"]))
    (is (= (parse-disk-map "2333133121414131402") ["0" "0" "." "." "." "1" "1" "1" "." "." "." "2" "." "." "." "3" "3" "3" "." "4" "4" "." "5" "5" "5" "5" "." "6" "6" "6" "6" "." "7" "7" "7" "." "8" "8" "8" "8" "9" "9"]))
    )
  (testing "compress"
    (is (= (compress (parse-disk-map "2333133121414131402")) ["0" "0" "9" "9" "8" "1" "1" "1" "8" "8" "8" "2" "7" "7" "7" "3" "3" "3" "6" "4" "4" "6" "5" "5" "5" "5" "6" "6"]))
    )
  (testing "checksum"
    (is (= (checksum (compress (parse-disk-map "2333133121414131402"))) 1928))
    )
  (testing "part1"
    (is (= (part1 "2333133121414131402") 1928))
    ;(is (= (part1 puzzle-input) 6415184586041))
    )
  (testing "parse-disk-map-block"
    (is (= (parse-disk-map-block "03") [[3 "."]]))
    (is (= (parse-disk-map-block "30") [[3 "0"]]))
    (is (= (parse-disk-map-block "312") [[3 "0"] [1 "."] [2 "1"]]))
    )
  (testing "fragmentation"
    (is (= (disk-map-as-string (fragmentation (parse-disk-map-block "2333133121414131402"))) "00992111777.44.333....5555.6666.....8888.."))
    )
  (testing "checksum-block"
    (is (= (checksum-block (fragmentation (parse-disk-map-block "2333133121414131402"))) 2858))
    (is (= (checksum-block [[1 "."] [1 "0"]]) 0))
    (is (= (checksum-block [[1 "."] [1 "1"]]) 1))
    (is (= (checksum-block [[1 "."] [1 "1"] [1 "2"]]) 5))
    )
  (testing "part2"
    (is (= (part2 "2333133121414131402") 2858))
    ;(is (= (part2 puzzle-input) 6436819084274))
    )
  )




