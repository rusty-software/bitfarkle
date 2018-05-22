(ns bitfarkle.firebase
  (:require
    [com.degel.re-frame-firebase :as firebase]
    [bitfarkle.events]
    [bitfarkle.subs]))

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
