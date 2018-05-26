(ns bitfarkle.game-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [bitfarkle.game :as game]))

(deftest test-has-at-least-trips?
  (is (game/has-at-least-trips? [1 1 1 2 2 3]))
  (is (game/has-at-least-trips? [1 1 1 1 2 3]))
  (is (not (game/has-at-least-trips? [1 1 2 2 3 3])))
  (is (not (game/has-at-least-trips? []))))

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

(deftest test-initialize-player
  (let [player (game/initialize-player {:name "player1"})]
    (is (zero? (:held-score player)))
    (is (zero? (:total-score player)))
    (is (= 6 (:available-dice player)))
    (is (= [] (:held player)))
    (is (= [] (:roll-holds player)))
    (is (= "player1" (:name player)))))

(defn player-initialized? [p]
  (let [{:keys [total-score held-score available-dice held]} p]
    (and (= 0 total-score held-score (count held))
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
    (is (= [3 3 3] (game/best-basic-scorable-from-idx [1 1 1 3 3 3] 4)))
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

(deftest test-calculate-score
  (testing "1s"
    (is (= 50 (game/calculate-score [5])))
    (is (= 100 (game/calculate-score [1]))))
  (testing "2s, 1s+1s"
    (is (= 100 (game/calculate-score [5 5])))
    (is (= 200 (game/calculate-score [1 1]))))
  (testing "1s+1s"
    (is (= 150 (game/calculate-score [1 5]))))
  (testing "3k"
    (is (= 200 (game/calculate-score [2 2 2])))
    (is (= 300 (game/calculate-score [3 3 3])))
    (is (= 400 (game/calculate-score [4 4 4])))
    (is (= 500 (game/calculate-score [5 5 5])))
    (is (= 600 (game/calculate-score [6 6 6])))
    (is (= 1000 (game/calculate-score [1 1 1]))))
  (testing "2s+1s"
    (is (= 200 (game/calculate-score [1 5 5])))
    (is (= 250 (game/calculate-score [1 1 5]))))
  (testing "4k"
    (is (= 400 (game/calculate-score [2 2 2 2])))
    (is (= 600 (game/calculate-score [3 3 3 3])))
    (is (= 800 (game/calculate-score [4 4 4 4])))
    (is (= 1000 (game/calculate-score [5 5 5 5])))
    (is (= 1200 (game/calculate-score [6 6 6 6])))
    (is (= 2000 (game/calculate-score [1 1 1 1]))))
  (testing "3k+1s"
    (is (= 500 (game/calculate-score [1 4 4 4]))))
  (testing "2s+2s"
    (is (= 300 (game/calculate-score [1 1 5 5]))))
  (testing "5k"
    (is (= 800 (game/calculate-score [2 2 2 2 2])))
    (is (= 1200 (game/calculate-score [3 3 3 3 3])))
    (is (= 1600 (game/calculate-score [4 4 4 4 4])))
    (is (= 2000 (game/calculate-score [5 5 5 5 5])))
    (is (= 2400 (game/calculate-score [6 6 6 6 6])))
    (is (= 4000 (game/calculate-score [1 1 1 1 1]))))
  (testing "4k+1s"
    (is (= 500 (game/calculate-score [1 2 2 2 2]))))
  (testing "3k+2s"
    (is (= 1100 (game/calculate-score [1 1 1 5 5]))))
  (testing "3k+1s+1s"
    (is (= 350 (game/calculate-score [1 2 2 2 5]))))
  (testing "6k"
    (is (= 1600 (game/calculate-score [2 2 2 2 2 2])))
    (is (= 2400 (game/calculate-score [3 3 3 3 3 3])))
    (is (= 3200 (game/calculate-score [4 4 4 4 4 4])))
    (is (= 4000 (game/calculate-score [5 5 5 5 5 5])))
    (is (= 4800 (game/calculate-score [6 6 6 6 6 6])))
    (is (= 8000 (game/calculate-score [1 1 1 1 1 1]))))
  (testing "Str"
    (is (= 1500 (game/calculate-score [1 2 3 4 5 6]))))
  (testing "3p"
    (is (= 1500 (game/calculate-score [2 2 4 4 6 6])))
    (is (= 1500 (game/calculate-score [1 1 4 4 5 5]))))
  (testing "5k+1s"
    (is (= 900 (game/calculate-score [1 2 2 2 2 2]))))
  (testing "4k+2s"
    (is (= 2100 (game/calculate-score [1 1 1 1 5 5]))))
  (testing "3k+3k"
    (is (= 800 (game/calculate-score [2 2 2 6 6 6]))))
  (testing "3k+2s+1s"
    (is (= 550 (game/calculate-score [1 1 3 3 3 5])))))

(deftest test-hold-dice
  (testing "Holding 1s"
    (let [player {:rolled [2 3 4 4 5 5]
                  :held []
                  :held-score 0
                  :available-dice 6
                  :total-score 1000}
          updated-game (game/hold-dice {:current-player player} 4)]
      (is (= [2 3 4 4 5] (get-in updated-game [:current-player :rolled])))
      (is (= [5] (get-in updated-game [:current-player :held])))
      (is (= 50 (get-in updated-game [:current-player :held-score])))
      (is (= 5 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Holding 3k"
    (let [player {:rolled [2 3 4 4 4 5]
                  :held []
                  :held-score 0
                  :available-dice 6
                  :total-score 1000}
          updated-game (game/hold-dice {:current-player player} 2)]
      (is (= [2 3 5] (get-in updated-game [:current-player :rolled])))
      (is (= [4 4 4] (get-in updated-game [:current-player :held])))
      (is (= 400 (get-in updated-game [:current-player :held-score])))
      (is (= 3 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Holding 3p"
    (let [player {:rolled [2 2 4 4 5 5]
                  :held []
                  :held-score 0
                  :available-dice 6
                  :total-score 1000}
          updated-game (game/hold-dice {:current-player player} 2)]
      (is (= [] (get-in updated-game [:current-player :rolled])))
      (is (= [2 2 4 4 5 5] (get-in updated-game [:current-player :held])))
      (is (= 1500 (get-in updated-game [:current-player :held-score])))
      (is (= 6 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Holding unscorable dice that would add to scorable hold"
    (let [player {:rolled [4 4 5]
                  :held [4 4 4]
                  :held-score 400
                  :available-dice 3
                  :total-score 1000}
          updated-game (game/hold-dice {:current-player player} 0)]
      (is (= [4 5] (get-in updated-game [:current-player :rolled])))
      (is (= [4 4 4 4] (get-in updated-game [:current-player :held])))
      (is (= 800 (get-in updated-game [:current-player :held-score])))
      (is (= 2 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score]))))))

(deftest test-unhold-dice
  (testing "Unhold 1s"
    (let [player {:rolled [2 3 4 4 5]
                  :held [5]
                  :held-score 50
                  :available-dice 5
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 0)]
      (is (= [2 3 4 4 5 5] (get-in updated-game [:current-player :rolled])))
      (is (= [] (get-in updated-game [:current-player :held])))
      (is (= 0 (get-in updated-game [:current-player :held-score])))
      (is (= 6 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 1s of 1s+1s"
    (let [player {:rolled [3 4 4 5]
                  :held [1 5]
                  :held-score 150
                  :available-dice 4
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 1)]
      (is (= [3 4 4 5 5] (get-in updated-game [:current-player :rolled])))
      (is (= [1] (get-in updated-game [:current-player :held])))
      (is (= 100 (get-in updated-game [:current-player :held-score])))
      (is (= 5 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 3k"
    (let [player {:rolled [2 3 5]
                  :held [4 4 4]
                  :held-score 400
                  :available-dice 3
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 2)]
      (is (= [2 3 4 4 4 5] (get-in updated-game [:current-player :rolled])))
      (is (= [] (get-in updated-game [:current-player :held])))
      (is (= 0 (get-in updated-game [:current-player :held-score])))
      (is (= 6 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 3k when selecting 1s (unholds best possible basic value)"
    (let [player {:rolled [2 3 5]
                  :held [1 1 1]
                  :held-score 1000
                  :available-dice 3
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 2)]
      (is (= [1 1 1 2 3 5] (get-in updated-game [:current-player :rolled])))
      (is (= [] (get-in updated-game [:current-player :held])))
      (is (= 0 (get-in updated-game [:current-player :held-score])))
      (is (= 6 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 3p"
    (let [player {:rolled []
                  :held [2 2 4 4 5 5]
                  :held-score 1500
                  :available-dice 6
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 2)]
      (is (= [2 2 4 4 5 5] (get-in updated-game [:current-player :rolled])))
      (is (= [] (get-in updated-game [:current-player :held])))
      (is (= 0 (get-in updated-game [:current-player :held-score])))
      (is (= 6 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 4k"
    (let [player {:rolled [2 2]
                  :held [1 1 1 1]
                  :held-score 2000
                  :available-dice 2
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 2)]
      (is (= [1 1 1 2 2] (get-in updated-game [:current-player :rolled])))
      (is (= [1] (get-in updated-game [:current-player :held])))
      (is (= 100 (get-in updated-game [:current-player :held-score])))
      (is (= 5 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 4k with no other scorables"
    (let [player {:rolled [4 4]
                  :held [2 2 2 2]
                  :held-score 400
                  :available-dice 2
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 2)]
      (is (= [2 4 4] (get-in updated-game [:current-player :rolled])))
      (is (= [2 2 2] (get-in updated-game [:current-player :held])))
      (is (= 200 (get-in updated-game [:current-player :held-score])))
      (is (= 3 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 3k+3k"
    (let [player {:rolled []
                  :held [1 1 1 3 3 3]
                  :held-score 1300
                  :available-dice 6
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 4)]
      (is (= [3 3 3] (get-in updated-game [:current-player :rolled])))
      (is (= [1 1 1] (get-in updated-game [:current-player :held])))
      (is (= 1000 (get-in updated-game [:current-player :held-score])))
      (is (= 3 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score])))))
  (testing "Unhold 3k+3k when selecting 1s"
    (let [player {:rolled []
                  :held [1 1 1 3 3 3]
                  :held-score 1300
                  :available-dice 6
                  :total-score 1000}
          updated-game (game/unhold-dice {:current-player player} 1)]
      (is (= [1 1 1] (get-in updated-game [:current-player :rolled])))
      (is (= [3 3 3] (get-in updated-game [:current-player :held])))
      (is (= 300 (get-in updated-game [:current-player :held-score])))
      (is (= 3 (get-in updated-game [:current-player :available-dice])))
      (is (= 1000 (get-in updated-game [:current-player :total-score]))))))

(deftest test-roll-again
  (with-redefs [game/scorable (constantly true)]
    (testing "first roll again"
      (let [player {:rolled [2 3 3 4]
                    :held [1 5]
                    :held-score 150
                    :roll-holds []
                    :available-dice 4
                    :total-score 1000}
            updated-player (:current-player (game/roll-dice {:current-player player}))]
        (is (= 4 (count (:rolled updated-player))))
        (is (= [] (:held updated-player)))
        (is (= 150 (:held-score updated-player)))
        (is (= [[1 5]] (:roll-holds updated-player)))))
    (testing "second roll again"
      (let [player {:rolled [3 4 6]
                    :held [1]
                    :held-score 250
                    :roll-holds [[1 5]]
                    :available-dice 3
                    :total-score 1000}
            updated-player (:current-player (game/roll-dice {:current-player player}))]
        (is (= 3 (count (:rolled updated-player))))
        (is (= [] (:held updated-player)))
        (is (= 250 (:held-score updated-player)))
        (is (= [[1 5] [1]] (:roll-holds updated-player)))))
    (testing "hold, roll, hold"
      (let [player {:rolled [1 1 3 4 5 6]
                    :held []
                    :held-score 1500
                    :roll-holds [[1 2 3 4 5 6]]
                    :available-dice 6
                    :total-score 1000}
            updated-player (:current-player (game/hold-dice {:current-player player} 0))]
        (is (= [1] (:held updated-player)))
        (is (= 1600 (:held-score updated-player)))
        (is (= [[1 2 3 4 5 6]] (:roll-holds updated-player)))))
    (testing "hold, roll, hold, hold"
      (let [player {:rolled [1 3 4 5 6]
                    :held [1]
                    :held-score 1600
                    :roll-holds [[1 2 3 4 5 6]]
                    :available-dice 5
                    :total-score 1000}
            updated-player (:current-player (game/hold-dice {:current-player player} 0))]
        (is (= [1 1] (:held updated-player)))
        (is (= 1700 (:held-score updated-player)))
        (is (= [[1 2 3 4 5 6]] (:roll-holds updated-player)))))))

(deftest test-farkle-player
  (let [player {:rolled [4 6]
                :held [1]
                :held-score 350
                :roll-holds [[1 5] [1]]
                :available-dice 2
                :total-score 1000}
        updated-player (game/farkle-player player)]
    (is (= [4 6] (:rolled updated-player)))
    (is (zero? (:held-score updated-player)))
    (is (zero? (:available-dice updated-player)))
    (is (not (:scorable updated-player)))
    (is (= 1000 (:total-score updated-player)))))

(deftest test-roll-farkle
  (with-redefs [game/roll (constantly [2 6])]
    (let [player {:rolled [4 6]
                  :held [1]
                  :held-score 350
                  :roll-holds [[1 5] [1]]
                  :available-dice 2
                  :total-score 1000}
          updated-player (:current-player (game/roll-dice {:current-player player}))]
      (is (= 2 (count (:rolled updated-player))))
      (is (zero? (:held-score updated-player)))
      (is (zero? (:available-dice updated-player)))
      (is (= 1000 (:total-score updated-player))))))

(deftest test-score-player
  (let [player {:held-score 1500
                :total-score 3500}
        updated-player (game/score-player player)]
    (is (= 5000 (:total-score updated-player)))))

(defn is-turn-initialized?
  [{:keys [rolled held held-score roll-holds available-dice]}]
  (and
    (is (= [] rolled) "rolled should be empty")
    (is (= [] held) "held should be empty")
    (is (= [] roll-holds) "roll-holds should be empty")
    (is (zero? held-score) "held-score should be 0")
    (is (= 6 available-dice) "available-dice should be 6")))

(deftest test-initialize-turn
  (let [player {:name "player1"
                :rolled [4 6]
                :held [1]
                :held-score 350
                :roll-holds [[1 5] [1]]
                :available-dice 2
                :total-score 1000}
         updated-player (game/initialize-turn player)]
    (is-turn-initialized? updated-player)))

(deftest test-idx-by-name
  (let [players [{:name "player1"}
                 {:name "player2"}
                 {:name "player3"}]]
    (is (= 1 (game/idx-by-name players "player2")))))

(deftest test-end-turn
  (let [player1 {:name "player1"
                 :rolled []
                 :held []
                 :held-score 0
                 :roll-holds []
                 :available-dice 6
                 :total-score 2000}
        player2 {:name "player2"
                 :rolled [4 6]
                 :held [1]
                 :held-score 350
                 :roll-holds [[1 5] [1]]
                 :available-dice 2
                 :total-score 1000}
        game {:current-player player2
              :players [player1 player2]}
        updated-game (game/end-turn game)]
    (is (= player1 (:current-player updated-game)))
    (is (= "player2" (get-in updated-game [:players 1 :name])))
    (is-turn-initialized? (get-in updated-game [:players 1]))
    (is (= 1350 (get-in updated-game [:players 1 :total-score])))
    (is (:score-disabled? updated-game))
    (is (not (:roll-disabled? updated-game)))))
