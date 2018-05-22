(ns bitfarkle.events
  (:require [re-frame.core :as rf]
            [bitfarkle.config]
            [bitfarkle.db :as db]
            [com.degel.re-frame-firebase]
            ))

(rf/reg-event-db
 :initialize-db
 (fn [_ _] db/default-db))

(rf/reg-event-fx
 :sign-out
 (fn [_ _] {:firebase/sign-out nil}))

(rf/reg-event-fx
 :sign-in
 (fn [_ _] {:firebase/google-sign-in {:sign-in-method :redirect
                                      #_(if config/debug?
                                        :popup
                                        :redirect)}}))

(rf/reg-event-db
 :set-user
 (fn [db [_ user]]
   (assoc db :user user)))

(rf/reg-event-db
 :firebase-error
 (fn [db [_ error]]
   (assoc db :firebase-error (pr-str error))))

(rf/reg-event-fx
 :create-game
 (fn [{:keys [db]} [_]]
   (let [code (apply str (repeatedly 4 #(rand-nth (map char (range 65 91)))))]
     {:db (assoc db
               :game-code code
               :view :pregame)
      :firebase/write {:path [(keyword code)]
                       :value {:players [{:name (get-in db [:user :email])}]}
                       :on-success #(js/console.log "wrote success")
                       :on-failure [:firebase-error]}})))

(rf/reg-event-fx
 :join-game
 (fn [{:keys [db]} [_ code]]
   {:db (assoc db
               :game-code code
               :view :pregame)
    :firebase/swap! {:path [(keyword code) :players]
                     :function (fn [players]
                                 (let [user-id (get-in db [:user :email])]
                                   (if (some (comp (partial = user-id) :name) players)
                                     players
                                     (conj (vec players) {:name user-id}))))
                     :on-success #(println "join game success")
                     :on-failure [:firebase-error]}}))
