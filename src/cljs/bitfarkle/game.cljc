(ns bitfarkle.game)

(def basic-score-map
  {[5] 50
   [1] 100
   [2 2 2] 200
   [3 3 3] 300
   [4 4 4] 400
   [5 5 5] 500
   [6 6 6] 600})

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
  "Returns the score for "
  [dice]
  0)

(defn score
  "Calculates the score given a collection of dice."
  [dice]
  (cond
    (= [1 2 3 4 5 6] dice) 1500

    #_#_(four-or-more-of-a-kind? dice) (double-scoring dice)

    (three-pairs? dice) 1500

    (= [1 1 1] dice) 1000

    :else
    (let [score (get basic-score-map dice)]
      (if score
        score
        (reduce single-dice-scoring 0 dice)))))
