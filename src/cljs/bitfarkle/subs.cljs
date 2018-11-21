(ns bitfarkle.subs
  (:require [re-frame.core :as rf])
  (:require-macros [reagent.ratom :refer [reaction]]))

(rf/reg-sub
 :db
 (fn [db]
   db))

(rf/reg-sub-raw
 :game
 (fn [_ _]
   (reaction
    (let [game-code @(rf/subscribe [:game-code])]
      (if game-code
        @(rf/subscribe [:firebase/on-value {:path [game-code]}])
        nil)))))

(rf/reg-sub
  :user
  (fn [db _] (:user db)))

(rf/reg-sub
 :view
 (fn [db _]
   (:view db)))

(rf/reg-sub
 :logged-in?
 (fn [_ _]
   (rf/subscribe [:user]))
 (fn [user _]
   (boolean user)))

(rf/reg-sub
 :game-code
 (fn [db]
   (:game-code db)))

(rf/reg-sub
 :game-state
 (fn [_ _]
   [(rf/subscribe [:game])
    (rf/subscribe [:user])])
 (fn [[game user] _]
   (if (nil? user)
     :not-signed-in
     (condp = (:game-over? game)
       true :over
       false :playing
       nil :not-started))))

(rf/reg-sub
 :players
 (fn [_ _]
    (rf/subscribe [:game]))
 (fn [game _]
   (:players game)))

(rf/reg-sub
 :current-player
 (fn [_ _]
   (rf/subscribe [:game]))
 (fn [game _]
   (:current-player game)))

(rf/reg-sub
  :roll-disabled?
  (fn [_ _]
    (rf/subscribe [:game]))
  (fn [game _]
    (:roll-disabled? game)))

(rf/reg-sub
  :score-disabled?
  (fn [_ _]
    (rf/subscribe [:game]))
  (fn [game _]
    (:score-disabled? game)))

(rf/reg-sub
  :my-turn?
  (fn [_ _]
   [(rf/subscribe [:game])
    (rf/subscribe [:user])])
  (fn [[game user] _]
    (= (:email user) (get-in game [:current-player :name]))))

(rf/reg-sub
  :final-round?
  (fn [_ _]
    (rf/subscribe [:game]))
  (fn [game _]
    (:final-round? game)))

(rf/reg-sub
 :displaying-boot?
 (fn [_ _]
   (rf/subscribe [:game]))
 (fn [game _]
   (:displaying-boot? game)))
