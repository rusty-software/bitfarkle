(ns bitfarkle.firebase
  (:require
    [cljsjs/firebase]))

(defn init
  "Connects to the firebase app.

  DO NOT commit the apiKey or messagingSenderId to git unless you want everyone
  to be able to mess with your stuff all the time..."
  []
  (js/firebase.initializeApp
    #js {:apiKey ""
         :authDomain "bitfarkle.firebaseapp.com"
         :databaseURL "https://bitfarkle.firebaseio.com"
         :projectId "bitfarkle"
         :storageBucket "bitfarkle.appspot.com"
         :messagingSenderId ""}))
