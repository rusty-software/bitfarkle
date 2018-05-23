(ns bitfarkle.views
  (:require [re-frame.core :as rf]
            [bitfarkle.subs :as subs]
            [clojure.string :as str]
            [cljs.pprint :as pprint]
            [bitfarkle.config :as config]))

(defn listen [query]
  @(rf/subscribe [query]))

(defn header []
  [:div
   {:class "text-center"}
   [:h2 "BitFarkle!"]
   (when-let [code (listen :game-code)]
     [:h4
      {:class "bg-success"}
      (str "Game code: " code)])
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
   [:p "Sign in with a Google account by clicking above to join the game."]
   [:p "Read the "
    [:a {:href "https://en.wikipedia.org/wiki/Farkle"} "wiki"] " for information on how to play."]])

(defn no-game []
  [:div
   [:form
    {:on-submit (fn [e]
                  (.preventDefault e)
                  (println (.. e -target -elements -game -value))
                  (rf/dispatch [:join-game (str/upper-case (.. e -target -elements -game -value))]))}
    [:div
     {:class "form-group row"}
     [:div
      {:class "col-sm-2"}
      [:input
       {:class "text-uppercase form-control"
        :id "gameCodeInput"
        :type "text"
        :name "game"
        :placeholder "Enter Game Code"
        :required true}]]
     [:button
      {:class "btn btn-info"
       :type "submit"}
      "Join Game"]]]
   [:br]
   [:button
    {:class "btn btn-danger"
     :on-click #(rf/dispatch [:create-game])} "Create new game"]])

(defn pending-game []
  (let [players (listen :players)]
    [:div
     {:class "text-center"}
     [:button
      {:class "btn btn-primary"
       :on-click #(rf/dispatch [:start-game])}
      "Start Game"]
     [:table
      {:class "table table-bordered"}
      [:thead
       [:tr
        [:th
         {:class "text-center"}
         "Players"]]]
      [:tbody
       (doall
         (for [[idx player] (map-indexed vector players)]
           [:tr
            {:key idx}
            [:td
             (:name player)]]))]]]))

(defn active-game []
  (let [players (listen :players)
        current-player (listen :current-player)
        rolled-dice (listen :rolled-dice)]
    [:div
     [:div
      {:class "row"
       :id "play-area"}
      [:div
       {:class "col-md-2"}
       [:strong "Current Player:"]]
      [:div
       {:class "col-md-10"}
       (:name current-player)]]
     [:div
      {:class "row"}
      [:div
       {:class "col-md-1"}
       [:button
        {:class "btn btn-success btn-block"
         :on-click #(rf/dispatch [:roll-player-dice])}
        "Roll"]]
      (doall
        (for [d rolled-dice]
          [:div
           ;; TODO: key
           {:class (str "col-md-2 dice dice-" d)}]))
      ]
     [:div
      {:class "row"}
      [:div
       {:class "col-md-1"}
       [:button
        {:class "btn btn-danger btn-block"}
        "Score"]]]
     [:div
      {:class "row"}
      [:hr]]
     (doall
         (for [player players]
           [:div
            {:key (:name player)
             :class "row"}
            [:div
             {:class "col-md-2"}
             (:name player)]
            [:div
             {:class "col-md-1"}
             "Score:"]
            [:div
             {:class "col-md-9"}
             (:total-score player)]
            ]))
     ]))

(defn game-over []
  [:span "game over"])

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
