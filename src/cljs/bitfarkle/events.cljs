(ns bitfarkle.events
  (:require [re-frame.core :as rf]
            [bitfarkle.db :as db]
            [bitfarkle.game :as game]
            [com.degel.re-frame-firebase]
            [bitfarkle.config :as config]
            [com.degel.re-frame-firebase.core :as core]))

#_(defn email-sign-in []
  (-> (js/firebase.auth)
      (sign-in auth-provider)
      (.then (partial maybe-link-with-credential link-with-credential))
      (.catch (core/default-error-handler))))



(rf/reg-event-db
 :initialize-db
 (fn [_ _] db/default-db))

(rf/reg-event-fx
 :sign-out
 (fn [_ _] {:firebase/sign-out nil}))

(rf/reg-event-fx
 :sign-in-google
 (fn [_ _] {:firebase/google-sign-in {:sign-in-method (if config/debug?
                                                        :popup
                                                        :redirect)}}))

(rf/reg-event-fx
 :sign-in-email
 (fn [_ _] {:firebase/google-sign-in {:sign-in-method (if config/debug?
                                                        :popup
                                                        :redirect)}}))

(rf/reg-event-fx
 :create-user
 (fn [_ _] {:firebase/google-sign-in {:sign-in-method (if config/debug?
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

(rf/reg-event-fx
  :hold-dice
  (fn [{:keys [db]} [_ dice-num]]
    {:firebase/swap! {:path [(keyword (:game-code db))]
                      :function #(game/hold-dice % dice-num)
                      :on-success #(println "hold-dice success")
                      :on-failure [:firebase-error]}}))

(rf/reg-event-fx
  :unhold-dice
  (fn [{:keys [db]} [_ dice-num]]
    {:firebase/swap! {:path [(keyword (:game-code db))]
                      :function #(game/unhold-dice % dice-num)
                      :on-success #(println "unhold-dice success")
                      :on-failure [:firebase-error]}}))

(defn game-event! [event f & args]
  (let [enabled? (atom true)]
    (rf/reg-event-fx
     event
     (fn [{:keys [db]} _]
       (js/setTimeout (fn [_] (compare-and-set! enabled? false true)) 500)
       (if (compare-and-set! enabled? true false)
         {:firebase/swap! {:path [(keyword (:game-code db))]
                           :function #(apply f % args)
                           :on-success #(println (str event " success"))
                           :on-failure [:firebase-error]}}
         {})))))

(game-event! :start-game game/initialize-game)
(game-event! :roll-dice game/roll-dice)
(game-event! :end-turn game/end-turn)

