(ns bitfarkle.views
  (:require [re-frame.core :as rf]
            [bitfarkle.subs :as subs]
            ))

(defn listen [query]
  @(rf/subscribe [query]))

(defn header []
  [:div
   [:h2 "BitFarkle!"]
   (when-let [code (listen :game-code)]
     [:span {:style {:padding-right 10}} "Game code: " code])
   (if (listen :logged-in?)
     [:span [:button {:class "button"
                      :on-click #(rf/dispatch [:sign-out])}
       "Sign Out"]]
     [:span [:button {:on-click #(rf/dispatch [:sign-in])
                      :class "button"}
             "Sign in"]
      [:br]
      [:span {:style {:margin-left "1em"}}
       "If you click Sign In and nothing happens, check your pop-up blocker!"]])
   [:hr]])

(defn not-signed-in []
  [:div
   [:h3 "Welcome to BitFarkle!"]
   [:p "Sign in with a Google account by clicking above to join the game."]
   [:p "Read the "
    [:a {:href "https://en.wikipedia.org/wiki/Farkle"} "wiki"] " for information on how to play."]])

(defn no-game []
  [:span "no game"])

(defn game []
  [:span "game"])

(defn main-panel []
  [:center
   [header]
   [:div
    (if (= :not-signed-in (listen :game-state))
      [not-signed-in]
      (let [view (listen :view)]
        (if (= :no-game view)
          [no-game]
          [game])))]]
  )
