(ns bitfarkle.views
  (:require [re-frame.core :as rf]
            [bitfarkle.subs]
            [clojure.string :as str]
            [cljs.pprint :as pprint]
            [bitfarkle.config :as config]))

(defn listen [query]
  @(rf/subscribe [query]))

(defn start-button []
  [:button
   {:class "btn btn-primary"
    :style {:margin-right "10px"}
    :on-click #(rf/dispatch [:start-game])}
   "Start Game"])

(defn header []
  [:div
   {:class "text-center"}
   [:h2 "BitFarkle!"]
   [:p
    "Read the "
    [:a
     {:href "https://en.wikipedia.org/wiki/Farkle"
      :target "_blank"}
     "wiki"]
    " for information on how to play."]
   (when-let [code (listen :game-code)]
     [:h4
      {:class "alert alert-success"}
      (str "Game code: " code)])
   (let [game-state (listen :game-state)]
     (when (or (= :over game-state)
               (= :not-started game-state)))
     [start-button])
   (if (listen :logged-in?)
     [:button
      {:class "btn btn-warning"
       :on-click #(rf/dispatch [:sign-out])}
      "Sign Out"]
     [:div
      [:button
       {:class "btn btn-primary"
        :on-click #(rf/dispatch [:sign-in])}
       "Sign in"]
      [:br]
      [:span
       {:class "small"}
       "If you click Sign In and nothing happens, check your pop-up blocker!"]])
   [:hr]])

(defn not-signed-in []
  [:div
   {:class "text-center"}
   [:h3 "Welcome to BitFarkle!"]
   [:p "Sign in with a Google account by clicking above to join the game."]])

(defn no-game []
  [:div
   {:class "text-center"}
   [:div
    {:class "row"}
    [:div
     {:class "col"}
     [:form
      {:class "form-inline"
       :style {:display "inline-block"}
       :on-submit (fn [e]
                    (.preventDefault e)
                    (rf/dispatch [:join-game (str/upper-case (.-value (.getElementById js/document "gameCodeInput")))]))}
      [:div
       {:class "form-group"}
       [:input
        {:class "text-uppercase form-control mx-1"
         :id "gameCodeInput"
         :type "text"
         :name "game"
         :placeholder "Game Code"
         :required true}]
       [:button
        {:class "btn btn-info"
         :type "submit"}
        "Join Game"]]]]
    [:div
     {:class "col"}
     [:button
      {:class "btn btn-danger"
       :on-click #(rf/dispatch [:create-game])} "Create new game"]]]])

(defn pending-game []
  (let [players (listen :players)]
    [:div
     {:class "text-centered"}
     [:div
      {:class "alert alert-primary"}
      "Players"]
     (doall
      (for [[idx player] (map-indexed vector players)]
        [:div
         {:key idx}
         (:name player)]))]))

(defn active-game []
  (let [my-turn? (listen :my-turn?)
        players (listen :players)
        current-player (listen :current-player)
        {:keys [rolled held roll-holds total-held-score scorable]} current-player
        roll-disabled? (listen :roll-disabled?)
        score-disabled? (listen :score-disabled?)
        final-round? (listen :final-round?)
        displaying-boot? (listen :displaying-boot?)]
    [:div
     [:div
      {:class "border"}
      (when final-round?
        [:div
         {:class "row"}
         [:div
          {:class "col text-center"}
          [:h3
           {:class "alert alert-info"}
           "FINAL ROUND!"]]])
      [:div
       {:class "row"}
       (when (and (not (nil? rolled))
                  (not scorable))
         [:div
          {:class "col text-center"}
          [:h3
           {:class "alert alert-danger"}
           "FARKLED!!"]])]
      [:div
       {:class "row"
        :id "play-area"}
       [:div
        {:class "col"}
        [:img {:src (:photo-url current-player) :width "50px"}]]
       [:div
        {:class "col"}
        [:strong
         {:style {:color (if my-turn? "red" "black")}}
         (let [dn (:display-name current-player)]
           (str
             (if (str/blank? dn)
               (:name current-player)
               dn)
             "'s turn"))]]
       [:div
        {:class "col"}
        [:strong "Held Total: "]
        total-held-score]]
      [:div
       {:class "row"}
       [:hr]]
      (when my-turn?
        [:div
         {:class "row"}
         [:div
          {:class "col-1"}]
         [:div
          {:class "col-2"}
          [:button
           {:class "btn btn-success btn-block"
            :on-click #(rf/dispatch [:roll-dice])
            :disabled roll-disabled?}
           "Roll"]]
         [:div
          {:class "col-2"}
          [:button
           {:class "btn btn-dark btn-block"
            :on-click #(rf/dispatch [:end-turn])
            :disabled score-disabled?}
           (if (:farkled? current-player) "End Turn" "Score")]]])
      [:div
       {:class "row"
        :style {:height "5px"}}]
      [:div
       {:class "row"
        :style {:height "100px"}}
       [:div
        {:class "col-1"}
        "Rolled:"]
       (doall
         (for [[idx d] (map-indexed vector rolled)]
           [:div
            {:key idx
             :class (str "col-1 dice dice-" d)
             :on-click #(rf/dispatch [:hold-dice idx])}]))]
      [:div
       {:class "row"
        :style {:height "100px"}}
       [:div
        {:class "col-1"}
        "Held:"]
       (doall
         (for [[idx d] (map-indexed vector held)]
           [:div
            {:key idx
             :class (str "col-1 dice dice-" d)
             :on-click #(rf/dispatch [:unhold-dice idx])}]))]
      [:div
       {:class "row"
        :style {:height "75px"}}
       [:div
        {:class "col-1"}
        "Bucket:"]
       (doall
         (for [[h-idx holding] (map-indexed vector roll-holds)]
           (conj
             (for [[d-idx d] (map-indexed vector holding)]
               [:div
                {:key (str h-idx "-" d-idx)
                 :class (str "tinydice dice-" d)}])
             [:div
              {:key (str "separator-" h-idx)
               :class "bg-dark"
               :style {:width "3px"
                       :height "50px"}}])))]]
     [:div
      [:hr]]
     [:div
      {:class "alert alert-primary"}
      "Players"]
     (doall
       (for [i (range (count players))
             :let [{:keys [farkles] :as player} (get players i)]]
         [:div
          {:key (:name player)
           :class "row"}
          [:div
           {:class "col"}
           [:div
            {:class "row"}
            (when displaying-boot?
            [:div
             [:button
              {:class "btn btn-danger"
               :on-click #(rf/dispatch [:boot-player i])}
              "Boot!"]])
            (if (< 0 farkles)
              (for [_ (range farkles)]
                [:div
                 {:class "tinyf"
                  :style {:height "15px" :width "15px"}}]))
            [:div
             {:class "col"}
             (:name player)]]]
          [:div
           {:class "col"}
           (str "Total Score: " (:total-score player))]]))]))

(defn game-over []
  [:div
   {:class "border"}
   [:div
    {:class "row text-center"}
    [:div
     {:class "col"}
     [:h4
      {:class "alert alert-secondary"}
      "Game Over!"]]]
   (let [players (listen :players)]
     (doall
       (for [player players]
         [:div
          {:key (:name player)
           :class "row text-center"}
          [:div
           {:class "col"}
           (if (:winner? player)
             [:h5
              {:class "alert alert-success"}
              (str (:name player) ", Total Score: " (:total-score player))]
             [:span
              (str (:name player) ", Total Score: " (:total-score player))])]])))])

(defn game []
  (condp = (listen :game-state)
    :over [game-over]
    :playing [active-game]
    :not-started [pending-game]))

(defn main-panel []
  [:div
   {:class "container"}
   [header]
   [:div
    (if (= :not-signed-in (listen :game-state))
      [not-signed-in]
      (let [view (listen :view)]
        (if (= :no-game view)
          [no-game]
          [game])))]
   (when config/debug?
     [:div
      [:hr]
      [:pre
       (with-out-str (pprint/pprint @(rf/subscribe [:db])))]])
   ]
  )
