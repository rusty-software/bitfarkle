(ns bitfarkle.events
  (:require [re-frame.core :as rf]
            [bitfarkle.config :as config]
            [bitfarkle.db :as db]
            ))

(rf/reg-event-db
 :initialize-db
 (fn [_ _] db/default-db))

(rf/reg-event-fx
 :sign-out
 (fn [_ _] {:firebase/sign-out nil}))

(rf/reg-event-fx
 :sign-in
 (fn [_ _] {:firebase/google-sign-in {:sign-in-method
                                      (if config/debug?
                                        :popup
                                        :redirect)}}))
