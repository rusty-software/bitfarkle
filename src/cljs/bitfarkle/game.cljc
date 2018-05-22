(ns bitfarkle.game
  (:import (Math)))

(def basic-score-map
  {[5] 50
   [1] 100
   [1 1 1] 1000
   [2 2 2] 200
   [3 3 3] 300
   [4 4 4] 400
   [5 5 5] 500
   [6 6 6] 600
   [1 2 3 4 5 6] 1500})

(defn single-dice-scoring
  "Returns a score for a single dice."
  [score dice]
  (let [val-map {5 50
                 1 100}
        val (get val-map dice)]
    (+ val score)))

(defn three-pairs?
  "Returns true if the dice consist of three pairs; false otherwise."
  [dice]
  (let [potential-pairs (partition-all 2 (sort dice))]
    (and
      (= 3 (count potential-pairs))
      (every? #(= true %) (map (fn [[l r]] (= l r)) potential-pairs)))))

(defn all-the-same?
  "Returns true all the dice are the same; false otherwise."
  [dice]
  (let [val (first dice)]
    (every? #(= val %) dice)))

(defn double-scoring
  "Returns the score for four, five, or six of a kind using the doubling rules."
  [dice]
  (let [[basic-scorer v] (partition-all 3 dice)
        basic-score (get basic-score-map basic-scorer)
        multiplier (Math/pow 2 (count v))]
    (int (* basic-score multiplier))))

(defn calculate-score
  "Calculates the score given a collection of dice."
  [dice]
  (cond
    (and (all-the-same? dice)
         (> (count dice) 3))
    (double-scoring dice)

    (three-pairs? dice) 1500

    :else
    (let [score (get basic-score-map dice)]
      (if score
        score
        (reduce single-dice-scoring 0 dice)))))

(defn has-at-least-trips?
  "Returns true if the dice at least have three of a kind; false otherwise."
  [dice]
  (< 2 (apply max (vals (frequencies dice)))))

(defn scorable
  "Returns truthy if a set of dice contains a scorable combination; false otherwise."
  [dice]
  (let [dice (sort dice)]
    (or
      (some #(or (= 1 %) (= 5 %)) dice)
      (has-at-least-trips? dice)
      (three-pairs? dice))))

(defn roll
  "Given a number of dice to roll, returns a vector of randomly rolled dice."
  [num-to-roll]
  (into []
        (for [_ (range 1 (inc num-to-roll))]
          (inc (rand-int 6)))))

(defn score-player
  "Given a player, adds the player's held amount to their score."
  [{:keys [held-score] :as player}]
  (-> player
      (update :total-score + held-score)
      (assoc :held-score 0)))

(defn hold-dice
  "Given a player, adds the held dice to the held score, resets the held dice, and adjusts the available dice."
  [{:keys [to-hold available-dice] :as player}]
  (if (not (scorable to-hold))
    (assoc player :error "Dice must be scorable in order to hold!")
    (let [score (calculate-score to-hold)
          dice-left (if (= available-dice (count to-hold))
                      6
                      (- available-dice (count to-hold)))]
      (-> player
          (update :held-score + score)
          (assoc :to-hold []
                 :available-dice dice-left)))))

(defn generate-game-code
  "Generates a relatively random 4-character game code."
  []
  (apply str (repeatedly 4 #(rand-nth (map char (range 65 91))))))
