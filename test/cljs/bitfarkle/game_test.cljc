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
    (is (not (game/three-pairs? [2 2 4 4 3 4])))))

(deftest test-scoring
  (testing "Single-dice. Also, I loathe that English now accepts 'dice' as the singular form..."
    (is (= 50 (game/score [5])))
    (is (= 100 (game/score [1]))))
  (testing "Three-of-a-kind"
    (is (= 200 (game/score [2 2 2])))
    (is (= 300 (game/score [3 3 3])))
    (is (= 400 (game/score [4 4 4])))
    (is (= 500 (game/score [5 5 5])))
    (is (= 600 (game/score [6 6 6]))))
  (testing "Two-dice"
    (is (= 100 (game/score [5 5])))
    (is (= 150 (game/score [1 5]))))
  (testing "Three-dice"
    (is (= 200 (game/score [5 1 5])))
    (is (= 200 (game/score [1 5 5])))
    (is (= 250 (game/score [1 5 1])))
    (is (= 250 (game/score [1 1 5]))))
  (testing "Special cases"
    (is (= 1000 (game/score [1 1 1])))
    (is (= 1500 (game/score [1 2 3 4 5 6])))
    (is (= 1500 (game/score [2 2 4 4 6 6]))))
  (testing "Four-of-a-kind"
    (is (= 400 [2 2 2 2]))
    (is (= 600 [3 3 3 3]))
    (is (= 800 [4 4 4 4]))
    (is (= 1000 [5 5 5 5]))
    (is (= 1200 [6 6 6 6]))
    (is (= 2000 [1 1 1 1])))
  (testing "Five-of-a-kind"
    (is (= 800 [2 2 2 2 2]))
    (is (= 1200 [3 3 3 3 3]))
    (is (= 1600 [4 4 4 4 4]))
    (is (= 2000 [5 5 5 5 5]))
    (is (= 2400 [6 6 6 6 6]))
    (is (= 4000 [1 1 1 1 1])))
  (testing "Six-of-a-kind"
    (is (= 1600 [2 2 2 2 2 2]))
    (is (= 2400 [3 3 3 3 3 3]))
    (is (= 3200 [4 4 4 4 4 4]))
    (is (= 4000 [5 5 5 5 5 5]))
    (is (= 4800 [6 6 6 6 6 6]))
    (is (= 8000 [1 1 1 1 1 1])))
  )
