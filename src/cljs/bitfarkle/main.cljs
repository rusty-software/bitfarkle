(ns bitfarkle.main
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [bitfarkle.events]
            [bitfarkle.views :as views]
            [bitfarkle.config :as config]
            [bitfarkle.firebase :as firebase]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (rf/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app"))
  (let [keys-down (atom #{})]
    (. js/document (addEventListener "keydown" (fn [e]
                                                 (swap! keys-down conj (.-key e)))))
    (. js/document (addEventListener "keyup" (fn [e]
                                               (when (and (contains? @keys-down "Control")
                                                          (contains? @keys-down "Shift")
                                                          (contains? @keys-down "B"))
                                                 (rf/dispatch [:toggle-boot-buttons]))
                                               (swap! keys-down (fn [kd]
                                                                  (set (remove #{(.-key e)} kd)))))))))

(defn ^:export init []
  (rf/dispatch-sync [:initialize-db])
  (dev-setup)
  (firebase/init)
  (mount-root))
