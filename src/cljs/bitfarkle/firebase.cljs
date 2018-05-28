(ns bitfarkle.firebase
  (:require
    [com.degel.re-frame-firebase :as firebase]
    [bitfarkle.events]
    [bitfarkle.subs]
    [iron.re-utils :as re-utils]
    [re-frame.core :as rf]))

(def fb-ref #'com.degel.re-frame-firebase.core/fb-ref)

(defn init
  "Connects to the firebase app.

  None of these values seem to pose a security risk, given the nature of the data being stored for this app."
  []
  (firebase/init :firebase-app-info {:apiKey "AIzaSyDIfnQE1UH_ztVNT5s0ZLBSCXo-uoHIKmQ"
                                     :authDomain "bitfarkle.firebaseapp.com"
                                     :databaseURL "https://bitfarkle.firebaseio.com"
                                     :storageBucket "bitfarkle.appspot.com"
                                     :projectId "bitfarkle"
                                     :messagingSenderId "457614881377"}
                 :get-user-sub [:user]
                 :set-user-event [:set-user]
                 :default-error-handler [:firebase-error]))

(defn success-failure-wrapper [on-success on-failure]
  (let [on-success (and on-success (re-utils/event->fn on-success))
        on-failure (and on-failure (re-utils/event->fn on-failure))]
    (fn [err]
      (cond (nil? err)
            (when on-success (on-success))

            on-failure (on-failure err)

            :else      ;; [TODO] This should use default error handler
            (js/console.error "Firebase error:" err)))))

(defn- js->clj-tree [x]
  (-> x
      js->clj
      clojure.walk/keywordize-keys))

(defn firebase-transaction-effect [{:keys [path function on-success on-failure]}]
  (.transaction (fb-ref path)
                (fn [data] (-> data
                               js->clj-tree
                               function
                               clj->js))
                (success-failure-wrapper on-success on-failure)))

(rf/reg-fx :firebase/swap! firebase-transaction-effect)