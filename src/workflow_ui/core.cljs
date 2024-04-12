(ns workflow-ui.core
  (:require
    [uix.core :refer [defui $]]
    [uix.dom]
    [workflow-ui.view :as view]))

(defui button [{:keys [on-click children]}]
  ($ :button.btn {:on-click on-click}
     children))

(defui app []
  ($ view/workflow))

(defonce root
         (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))