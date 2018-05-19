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

(defn score
  "Calculates the score given a collection of dice."
  [dice]
  (let [score (get basic-score-map dice)]
    (println "basic-score" score)
    (if score
      score
      (reduce single-dice-scoring 0 dice))))
