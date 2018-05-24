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

(deftest test-roll-dice
  (let [player {:available-dice 6
                :rolled []
                :held []}
        game {:current-player player}
        updated-game (game/roll-dice game)
        updated-player (:current-player updated-game)]
    (is (= 6 (:available-dice updated-player)))
    (is (= 6 (count (:rolled updated-player))))
    (is (zero? (count (:held updated-player))))))

(deftest test-score-player
  (let [player {:held-score 1500
                :total-score 3500}
        updated-player (game/score-player player)]
    (is (= 5000 (:total-score updated-player)))
    (is (zero? (:held-score updated-player)))))

(deftest test-hold-dice
  (testing "Dice still available"
    (let [player {:held-score 100
                  :total-score 1000
                  :to-hold [1]
                  :available-dice 4}
          updated-player (game/hold player)]
      (is (= 200 (:held-score updated-player)))
      (is (= 1000 (:total-score updated-player)))
      (is (= [] (:to-hold updated-player)))
      (is (= 3 (:available-dice updated-player)))))
  (testing "Holding all dice"
    (let [player {:held-score 100
                  :total-score 1000
                  :to-hold [1]
                  :available-dice 1}
          updated-player (game/hold player)]
      (is (= 200 (:held-score updated-player)))
      (is (= 1000 (:total-score updated-player)))
      (is (= [] (:to-hold updated-player)))
      (is (= 6 (:available-dice updated-player)))))
  (testing "Illegal hold attempt"
    (let [player {:held-score 100
                  :total-score 1000
                  :to-hold [6]
                  :available-dice 3}
          updated-player (game/hold player)]
      (is (= 100 (:held-score updated-player)))
      (is (= 1000 (:total-score updated-player)))
      (is (= [6] (:to-hold updated-player)))
      (is (= 3 (:available-dice updated-player)))
      (is (= "Dice must be scorable in order to hold!" (:error updated-player))))))

(deftest test-initialize-player
  (let [player (game/initialize-player {:name "player1"})]
    (is (zero? (:held-score player)))
    (is (zero? (:total-score player)))
    (is (= 6 (:available-dice player)))
    (is (= [] (:to-hold player)))
    (is (= "player1" (:name player)))))

(defn player-initialized? [p]
  (let [{:keys [total-score held-score available-dice to-hold]} p]
    (and (= 0 total-score held-score (count to-hold))
         (= 6 available-dice))))

(deftest test-initialize-game
  (let [game {:players [{:name "player1"} {:name "player2"}]}
        initialized (game/initialize-game game)]
    (is (= 2 (count (:players initialized))))
    (is (player-initialized? (get-in initialized [:players 0])))
    (is (player-initialized? (get-in initialized [:players 1])))
    (is (= "player1" (get-in initialized [:players 0 :name])))
    (is (= "player2" (get-in initialized [:players 1 :name])))
    (is (= (get-in initialized [:players 0]) (:current-player initialized)))))

(deftest test-best-basic-scorable-from-idx
  (testing "dice has scorable"
    (is (= [5] (game/best-basic-scorable-from-idx [2 2 3 3 4 5] 5)))
    (is (= [1] (game/best-basic-scorable-from-idx [1 2 2 3 3 4] 0)))
    (is (= [1] (game/best-basic-scorable-from-idx [1 1 2 3 3 4] 1)))
    (is (= [1 1 1] (game/best-basic-scorable-from-idx [1 1 1 2 3 5] 0)))
    (is (= [1 1 1] (game/best-basic-scorable-from-idx [1 1 1 2 3 5] 1)))
    (is (= [1 1 1] (game/best-basic-scorable-from-idx [1 1 1 2 3 5] 2)))
    (is (= [2 2 2] (game/best-basic-scorable-from-idx [1 2 2 2 3 5] 1)))
    (is (= [2 2 2] (game/best-basic-scorable-from-idx [1 2 2 2 3 5] 2)))
    (is (= [2 2 2] (game/best-basic-scorable-from-idx [1 2 2 2 3 5] 3)))
    (is (= [2 2 4 4 6 6] (game/best-basic-scorable-from-idx [2 2 4 4 6 6] 0)))
    (is (= [2 2 4 4 6 6] (game/best-basic-scorable-from-idx [2 2 4 4 6 6] 1)))
    (is (= [2 2 4 4 6 6] (game/best-basic-scorable-from-idx [2 2 4 4 6 6] 2)))
    (is (= [2 2 4 4 6 6] (game/best-basic-scorable-from-idx [2 2 4 4 6 6] 3)))
    (is (= [2 2 4 4 6 6] (game/best-basic-scorable-from-idx [2 2 4 4 6 6] 4)))
    (is (= [2 2 4 4 6 6] (game/best-basic-scorable-from-idx [2 2 4 4 6 6] 5)))
    (is (= [1 1 4 4 5 5] (game/best-basic-scorable-from-idx [1 1 4 4 5 5] 0)))
    (is (= [1 2 3 4 5 6] (game/best-basic-scorable-from-idx [1 2 3 4 5 6] 0)))
    (is (= [1 2 3 4 5 6] (game/best-basic-scorable-from-idx [1 2 3 4 5 6] 4)))
    (is (= [1 2 3 4 5 6] (game/best-basic-scorable-from-idx [1 2 3 4 5 6] 5))))
  (testing "non-scorable dice"
    (is (= [] (game/best-basic-scorable-from-idx [2 2 3 3 4 5] 0)))
    (is (= [] (game/best-basic-scorable-from-idx [2 2 3 3 4 5] 1)))
    (is (= [] (game/best-basic-scorable-from-idx [2 2 3 3 4 5] 2)))
    (is (= [] (game/best-basic-scorable-from-idx [2 2 3 3 4 5] 3)))
    (is (= [] (game/best-basic-scorable-from-idx [2 2 3 3 4 5] 4)))))

#_(deftest test-best-score
  (is (= [1 1 1] (game/best-score [1 1 1 2 3 5])))
  (is (= [1 1 1] (game/best-score [1 1 1 2 3 5])))
  )
