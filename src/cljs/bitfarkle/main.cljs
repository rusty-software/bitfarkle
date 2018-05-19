(ns bitfarkle.main
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [bitfarkle.events :as events]
            [bitfarkle.views :as views]
            [bitfarkle.config :as config]
            [bitfarkle.firebase :as firebase]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (firebase/init)
  (mount-root))
