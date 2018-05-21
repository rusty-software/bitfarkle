(ns bitfarkle.game-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [bitfarkle.game :as game]))

(deftest test-three-pairs?
  (testing "Any three pairs"
    (is (game/three-pairs? [2 2 4 4 6 6]))
    (is (game/three-pairs? [1 3 5 1 3 5])))
  (testing "Four of a kind and another pair"
    (is (game/three-pairs? [2 2 2 2 6 6])))
  (testing "Missing a pair"
    (is (not (game/three-pairs? [2 2 4 4 3 4]))))
  (testing "Five dice is false by default"
    (is (not (game/three-pairs? [2 2 4 4 3])))))

(deftest test-scoring
  (testing "Single-dice. Also, I loathe that English now accepts 'dice' as the singular form..."
    (is (= 50 (game/calculate-score [5])))
    (is (= 100 (game/calculate-score [1]))))
  (testing "Three-of-a-kind"
    (is (= 200 (game/calculate-score [2 2 2])))
    (is (= 300 (game/calculate-score [3 3 3])))
    (is (= 400 (game/calculate-score [4 4 4])))
    (is (= 500 (game/calculate-score [5 5 5])))
    (is (= 600 (game/calculate-score [6 6 6]))))
  (testing "Two-dice"
    (is (= 100 (game/calculate-score [5 5])))
    (is (= 150 (game/calculate-score [1 5]))))
  (testing "Three-dice"
    (is (= 200 (game/calculate-score [5 1 5])))
    (is (= 200 (game/calculate-score [1 5 5])))
    (is (= 250 (game/calculate-score [1 5 1])))
    (is (= 250 (game/calculate-score [1 1 5]))))
  (testing "Special cases"
    (is (= 1000 (game/calculate-score [1 1 1])))
    (is (= 1500 (game/calculate-score [1 2 3 4 5 6])))
    (is (= 1500 (game/calculate-score [2 2 4 4 6 6]))))
  (testing "Four-of-a-kind"
    (is (= 400 (game/calculate-score [2 2 2 2])))
    (is (= 600 (game/calculate-score [3 3 3 3])))
    (is (= 800 (game/calculate-score [4 4 4 4])))
    (is (= 1000 (game/calculate-score [5 5 5 5])))
    (is (= 1200 (game/calculate-score [6 6 6 6])))
    (is (= 2000 (game/calculate-score [1 1 1 1]))))
  (testing "Five-of-a-kind"
    (is (= 800 (game/calculate-score [2 2 2 2 2])))
    (is (= 1200 (game/calculate-score [3 3 3 3 3])))
    (is (= 1600 (game/calculate-score [4 4 4 4 4])))
    (is (= 2000 (game/calculate-score [5 5 5 5 5])))
    (is (= 2400 (game/calculate-score [6 6 6 6 6])))
    (is (= 4000 (game/calculate-score [1 1 1 1 1]))))
  (testing "Six-of-a-kind"
    (is (= 1600 (game/calculate-score [2 2 2 2 2 2])))
    (is (= 2400 (game/calculate-score [3 3 3 3 3 3])))
    (is (= 3200 (game/calculate-score [4 4 4 4 4 4])))
    (is (= 4000 (game/calculate-score [5 5 5 5 5 5])))
    (is (= 4800 (game/calculate-score [6 6 6 6 6 6])))
    (is (= 8000 (game/calculate-score [1 1 1 1 1 1])))))

(deftest test-scorable?
  (is (game/scorable [5 2 3 4 6 6]) "Should be scorable with 5")
  (is (game/scorable [1 2 3 4 6 6]) "Should be scorable with 1")
  (is (game/scorable [4 2 3 4 2 2]) "Should be scorable with trips")
  (is (game/scorable [2 2 6 4 6 4]) "Should be scorable with three pair")
  (is (not (game/scorable [2 2 3 3 4 6]))))

(deftest test-roll
  (let [legit-value? (fn [dice] (every? #(and (< 0 %) (> 7 %)) dice))
        six-dice (game/roll 6)
        three-dice (game/roll 3)]
    (is (= 6 (count six-dice)))
    (is (legit-value? six-dice))
    (is (= 3 (count three-dice)))
    (is (legit-value? three-dice))))