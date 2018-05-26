(ns bitfarkle.game
  (:require [clojure.set :as set])
  (:import (Math)))

(def basic-score-map
  {[5] 50
   [1] 100
   [2 2 2] 200
   [3 3 3] 300
   [4 4 4] 400
   [5 5 5] 500
   [6 6 6] 600
   [1 1 1] 1000
   [1 2 3 4 5 6] 1500})

(def basic-scorables-ranked
  [[1 2 3 4 5 6]
   [1 1 1]
   [6 6 6]
   [5 5 5]
   [4 4 4]
   [3 3 3]
   [2 2 2]
   [1]
   [5]])

(defn initialize-player
  "Returns a player in the initialized state."
  [player]
  {:total-score 0
   :held-score 0
   :available-dice 6
   :held []
   :roll-holds []
   :name (:name player)})

(defn initialize-game
  "Given a game state, initializes a new game."
  [{:keys [players]}]
  (let [initialized-players (vec (map initialize-player players))]
    {:current-player (get initialized-players 0)
     :game-over? false
     :players initialized-players
     :roll-disabled? false
     :score-disabled? true}))

(defn three-pairs?
  "Returns true if the dice consist of three pairs; false otherwise."
  [dice]
  (let [potential-pairs (partition-all 2 (sort dice))]
    (and
      (= 3 (count potential-pairs))
      (every? #(= true %) (map (fn [[l r]] (= l r)) potential-pairs)))))

(defn double-scoring
  "Returns the score for four, five, or six of a kind using the doubling rules."
  [dice]
  (let [[basic-scorer v] (partition-all 3 dice)
        basic-score (get basic-score-map basic-scorer)
        multiplier (Math/pow 2 (count v))]
    (int (* basic-score multiplier))))

(defn best-basic-scorable-from-idx
  "Given a set of dice, finds the best basic scorable."
  [dice idx]
  (let [single (conj #{} (vector (get dice idx)))
        trips (set (filter #(< 2 (count %))
                           (for [i (range (- idx 2) (+ idx 1))]
                             (into [] (take 3 (drop i dice))))))
        best-basic-scorable (first (filter #(contains? (set/union single trips) %) basic-scorables-ranked))]
    (cond
      (= [1 2 3 4 5 6] dice) dice

      (three-pairs? dice) dice

      best-basic-scorable best-basic-scorable

      :else [])))

(defn score-1 [dice]
  (get basic-score-map dice 0))

(defn score-multiple-singles [dice]
  (apply + (map (comp score-1 vector) dice)))

(defn score-3 [dice]
  (when (= 3 (count dice))
    (if (apply = dice)
      (get basic-score-map dice)
      (score-multiple-singles dice))))

(defn groups-of-count [vals n]
  (filter #(= n (count %)) vals))

(defn calculate-score
  "Calculates every combination of scorables, returning the highest score value."
  [dice]
  (let [dice-groups (group-by identity dice)
        vals (vals dice-groups)
        six-k (groups-of-count vals 6)
        six-k-score (if (seq six-k)
                      (double-scoring (first six-k))
                      0)
        three-pairs-score (if (three-pairs? dice)
                            1500
                            0)
        straight-score (get basic-score-map dice 0)
        five-k (groups-of-count vals 5)
        five-k-score (if (seq five-k)
                       (double-scoring (first five-k))
                       0)
        four-k (groups-of-count vals 4)
        four-k-score (if (seq four-k)
                       (double-scoring (first four-k))
                       0)
        three-k (groups-of-count vals 3)
        three-k-score (if (seq three-k)
                        (apply + (map score-3 three-k))
                        0)
        pairs (groups-of-count vals 2)
        pairs-score (if (seq pairs)
                      (apply + (map score-multiple-singles pairs))
                      0)
        singles (groups-of-count vals 1)
        singles-score (if (seq singles)
                        (apply + (map score-1 singles))
                        0)]
    (max six-k-score
         three-pairs-score
         straight-score
         (+ five-k-score singles-score)
         (+ four-k-score pairs-score singles-score)
         (+ three-k-score pairs-score singles-score))))

(defn has-at-least-trips?
  "Returns true if the dice at least have three of a kind; false otherwise."
  [dice]
  (if (< 2 (count dice))
    (< 2 (apply max (vals (frequencies dice))))
    false))

(defn scorable
  "Returns truthy if a set of dice contains a scorable combination; false otherwise."
  [dice]
  (let [dice (sort dice)]
    (or (some #(or (= 1 %) (= 5 %)) dice)
        (has-at-least-trips? dice)
        (three-pairs? dice))))

(defn roll
  "Given a number of dice to roll, returns a vector of randomly rolled dice."
  [num-to-roll]
  (sort
    (into []
          (for [_ (range 1 (inc num-to-roll))]
            (inc (rand-int 6))))))

(defn farkle-player
  "Sets a player's attributes appropriately when farkled."
  [player]
  (assoc player :scorable false
                :held-score 0
                :available-dice 0))

(defn roll-dice
  "Given a game, moves the current player's held dice to a history, clears the held dice,
  then rolls the available number of dice and updates the rolled value."
  [game]
  (let [{:keys [available-dice held roll-holds] :as player} (:current-player game)
        rolled (roll available-dice)
        unfarkled (scorable rolled)
        updated-player (if unfarkled
                         (assoc player :held []
                                       :roll-holds (conj roll-holds held)
                                       :rolled rolled
                                       :scorable unfarkled)
                         (farkle-player player))]
    (assoc game :current-player updated-player
                :roll-disabled? true)))

(defn remove-at-idx [v idx]
  (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))

(defn remove-dice
  "Removes a collection of to-remove from a collection of dice.

  Note that while this can lead to partial removal, the game code shouldn't allow the
  to-remove collection to be anything other than a subset of dice."
  [dice to-remove]
  (reduce (fn [remaining d]
          (let [idx (.indexOf remaining d)]
            (if (<= 0 idx)
              (remove-at-idx remaining idx)
              remaining)))
          dice
          to-remove))

(defn add-dice
  "Adds a collection of to-add dice to a collection of dice."
  [dice to-add]
  (vec (sort (concat dice to-add))))

(defn hold-dice
  "Given a game, holds the specified dice for the current player."
  [{:keys [current-player] :as game} dice-num]
  (let [{:keys [rolled held held-score roll-holds]} current-player
        basic (seq (best-basic-scorable-from-idx rolled dice-num))
        single (vector (get rolled dice-num))
        single-added (add-dice held single)
        roll-holds-score (if (< 0 (count roll-holds))
                                held-score
                                0)
        scorable (if (and (nil? basic) (scorable single-added))
                   single
                   basic)]
    (if scorable
      (let [held (add-dice held scorable)
            score (+ (calculate-score held) roll-holds-score)
            rolled (remove-dice rolled scorable)
            updated-player (-> current-player
                               (assoc :held held
                                      :held-score score
                                      :rolled rolled
                                      :available-dice (if (zero? (count rolled))
                                                        6
                                                        (count rolled))))]
        (assoc game :current-player updated-player
                    :roll-disabled? false
                    :score-disabled? false))
      game)))

(defn unhold-dice
  "Given a game, removes specified dice from hold for the current player."
  [{:keys [current-player] :as game} dice-num]
  (let [{:keys [held rolled]} current-player
        basic (vec (seq (best-basic-scorable-from-idx held dice-num)))
        single (vector (get held dice-num))
        basic-removed (remove-dice held basic)
        removable (vec (if (or (zero? (count basic-removed))
                               (scorable basic-removed))
                         basic
                         single))]
    (if removable
      (let [updated-held (remove-dice held removable)
            score (calculate-score updated-held)
            updated-rolled (add-dice rolled removable)
            updated-player (-> current-player
                               (assoc :held updated-held
                                      :held-score score
                                      :rolled updated-rolled
                                      :available-dice (count updated-rolled)))]
        (assoc game :current-player updated-player
                    :roll-disabled? (zero? (count updated-held))
                    :score-disabled? (zero? (count updated-held))))
      game)))

(defn score-player
  "Given a player, adds the player's held amount to their score."
  [{:keys [held-score] :as player}]
  (update player :total-score + held-score))

(defn initialize-turn
  "Given a player, sets the player attributes appropriately for beginning a turn."
  [player]
  (-> player
      (assoc :rolled []
             :held []
             :held-score 0
             :roll-holds []
             :available-dice 6)))

(defn idx-by-name
  "Returns the first index of the item in the vector that matches the name attribute."
  [players name]
  (first (keep-indexed (fn [idx player]
                         (when (= name (:name player))
                           idx)) players)))

(defn end-turn
  "Given a game, updates the players collection with the current player information, sets
   the current player data to a turn-initial state, and moves the next player to current player."
  [{:keys [current-player players] :as game}]
  (let [ending-player (-> current-player
                          (score-player)
                          (initialize-turn))
        ending-player-idx (idx-by-name players (:name ending-player))
        updated-players (assoc players ending-player-idx ending-player)
        next-idx (if (= (inc ending-player-idx) (count players))
                   0
                   (inc ending-player-idx))
        next-player (get updated-players next-idx)]
    (assoc game :current-player next-player
                :players updated-players
                :roll-disabled? false
                :score-disabled? true)))
