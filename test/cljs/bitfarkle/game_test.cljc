(ns bitfarkle.game-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [bitfarkle.game :as game]))

(deftest test-scoring
  (testing "Single-dice scoring. Also, I loathe that English now accepts 'dice' as the singular form..."
    (is (= 50 (game/score [5])))
    (is (= 100 (game/score [1]))))
  (testing "Three-of-a-kind scoring."
    (is (= 200 (game/score [2 2 2])))
    (is (= 300 (game/score [3 3 3])))
    (is (= 400 (game/score [4 4 4])))
    (is (= 500 (game/score [5 5 5])))
    (is (= 600 (game/score [6 6 6]))))
  (testing "Two-dice scoring."
    (is (= 100 (game/score [5 5])))
    (is (= 150 (game/score [1 5]))))
  (testing "Three-dice scoring."
    (is (= 200 (game/score [5 1 5])))
    (is (= 200 (game/score [1 5 5])))
    (is (= 250 (game/score [1 5 1])))
    (is (= 250 (game/score [1 1 5]))))
  )
