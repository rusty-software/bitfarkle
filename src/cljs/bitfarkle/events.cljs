(ns bitfarkle.events
  (:require [re-frame.core :as rf]
            [bitfarkle.config :as config]
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
