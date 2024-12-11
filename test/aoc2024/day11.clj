(ns aoc2024.day11
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

(def apply-rule
  (memoize (fn [number blink]
             (if (= blink 0)
               number
               (let [number-str (str number)
                     length (count number-str)
                     half-length (int (Math/floor (/ length 2)))
                     ;_ (println number blink)
                     ]
                 (cond
                   (= number 0) (apply-rule 1 (dec blink))
                   (even? (count number-str)) [(apply-rule (bigint (apply str (take half-length number-str))) (dec blink))
                                               (apply-rule (bigint (apply str (take half-length (drop half-length number-str)))) (dec blink))]
                   :else (apply-rule (* number 2024) (dec blink))
                   ))
               )
             )))

(defn part1 [line blinks]
  (let [stones (str/split line #" ")
        stones (mapv bigint stones)
        stones (mapv #(apply-rule % blinks) stones)
        stones (flatten stones)
        ;_ (println stones)
        ]
    (count stones)
    )
  )

(deftest a-test
  (testing "part1"
    (is (= (part1 "125 17" 6) 22))
    (is (= (part1 "125 17" 25) 55312))
    (is (= (part1 "6563348 67 395 0 6 4425 89567 739318" 25) 184927))
    )
  )