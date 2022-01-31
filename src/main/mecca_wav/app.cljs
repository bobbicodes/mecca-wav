(ns mecca-wav.app
  (:require [reagent.dom :as rdom]))

(defn square-root
  [x]
  (.sqrt js/Math x))

(defn app []
  [:div#app
   [:h1 "mecca-wav"]
   [:h2 "PCM data browser"]])

(defn render []
  (rdom/render [app]
            (.getElementById js/document "root")))

(defn ^:dev/after-load start []
  (render)
  (js/console.log "start"))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
