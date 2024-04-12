(ns workflow-ui.view
  (:require
    [applied-science.js-interop :as j]
    [uix.core :as uix :refer [defui $]]
    [uix.dom]
    ["reactflow" :default ReactFlow :refer [addEdge
                                            Background
                                            Controls
                                            Handle
                                            ReactFlowProvider
                                            useEdgesState
                                            useNodesState
                                            useReactFlow]]
    ["@uiw/react-codemirror" :default CodeMirror]
    ["@codemirror/lang-javascript" :refer [javascript]]
    ["@codemirror/lang-sql" :refer [sql]]))

(def id (atom 0))

(def graph-state (atom {:graph {}
                        :execution-result {}}))

(defn execute-all []
  (letfn [(execute-node [node-id]
            (or (get (:execution-result @graph-state) node-id)
                (let [{:keys [parents function-body node-type]} (get-in @graph-state [:graph node-id])
                      parents-result (mapv execute-node parents)
                      func (case node-type
                             "js" (apply js/Function (conj parents function-body))
                             nil)
                      result (when func (apply func parents-result))]
                  (swap! graph-state assoc-in [:execution-result node-id] result)
                  result)))]
    (doseq [[node-id _] (:graph @graph-state)]
      (execute-node node-id))
    @graph-state))

(defn make-node
  [{:keys [position]}]
  (let [node-id (str "node" (swap! id inc))]
    #js {:id node-id
         :data #js {:label node-id}
         :dragHandle ".header"
         :position position
         :type "editor-node"}))

(defui code-editor [{:keys [language value on-change]}]
  (let [language (case language
                   "js" (javascript)
                   "sql" (sql))]
    ($ CodeMirror {:class "editor"
                   :value value
                   :extensions (array language)
                   :on-change on-change})))

(defui node-type-selector [{:keys [value on-change]}]
  ($ :<> {}
     ($ :select#node-type-selector.nodrag {:value value :on-change on-change}
        ($ :option {:value "js"} "JS")
        ($ :option {:value "sql"} "SQL")
        ($ :option {:value "url"} "URL"))))

(def editor-node
  (uix/memo
    (fn [props]
      (let [node-id (j/get props :id)
            [state set-state!] (uix/use-state {:function-body (get-in @graph-state [:graph node-id :function-body])
                                               :execution-result (get-in @graph-state [:execution-result node-id])
                                               :node-type "js"})
            on-change (uix/use-callback
                        (fn [function-body]
                          (swap! graph-state assoc-in [:graph node-id :function-body] function-body)
                          (set-state! (assoc state :function-body function-body)))
                        [state node-id])]
        ($ :<> {}
           ($ Handle {:type "source"
                      :position "right"})
           (when-not (= node-id "node1")
             ($ Handle {:type "target"
                        :position "left"}))
           ($ :div.custom-node {}
              ($ :div.header {}
                 ($ :span (j/get-in props [:data :label]))
                 ($ node-type-selector {:value (:node-type state)
                                        :on-change (fn [event]
                                                     (let [v (-> event .-target .-value)]
                                                       (swap! graph-state assoc-in [:graph node-id :node-type] v)
                                                       (set-state! #(assoc % :node-type v))))}))
              ($ code-editor {:language (:node-type state)
                              :value (:function-body state)
                              :on-change on-change})
              ($ :div.footer {} "Execution Result: " (:execution-result state))))))))

(defui flow []
  (let [react-flow-wrapper (uix/use-ref nil)
        connecting-node-id (uix/use-ref nil)
        [nodes set-nodes! on-nodes-change] (useNodesState (array))
        [edges set-edges! on-edges-change] (useEdgesState (array))
        screenToFlowPosition (j/get (useReactFlow) :screenToFlowPosition)
        on-connect (uix/use-callback
                     (fn [params]
                       (swap! connecting-node-id j/assoc! :current nil)
                       (swap! graph-state update-in [:graph (j/get params :target) :parents] conj (j/get params :source))
                       (set-edges! (fn [eds]
                                     (addEdge params eds))))
                     [set-edges!])
        on-connect-start (uix/use-callback
                           (fn [_ node]
                             (swap! connecting-node-id j/assoc! :current (j/get node :nodeId)))
                           [])
        on-connect-end (uix/use-callback
                         (fn [event]
                           (let [connecting-node-id (j/get @connecting-node-id :current)
                                 new-node (make-node {:position (screenToFlowPosition #js {:x (.-clientX event)
                                                                                           :y (.-clientY event)})})
                                 new-node-id (j/get new-node :id)]
                             (when (and connecting-node-id
                                        (.contains (-> event .-target .-classList) "react-flow__pane"))
                               (swap! graph-state assoc-in [:graph new-node-id] {:id            new-node-id
                                                                                 :parents       [connecting-node-id]
                                                                                 :function-body ""
                                                                                 :node-type     "js"})
                               (set-nodes! #(.concat % new-node))
                               (set-edges! #(.concat % #js {:id     @id
                                                            :source connecting-node-id
                                                            :target new-node-id})))))
                         [screenToFlowPosition set-nodes! set-edges!])]
    ($ :<> {}
       ($ :button {:on-click (fn []
                               (let [new-node (make-node {:position {:x 500
                                                                     :y 500}})
                                     new-node-id (j/get new-node :id)]
                                 (swap! graph-state assoc-in [:graph new-node-id] {:id            new-node-id
                                                                                   :parents       []
                                                                                   :function-body ""
                                                                                   :node-type     "js"})
                                 (set-nodes! #(.concat % new-node))))}
          "Add Node")
       ($ :button {:on-click execute-all} "Execute All")
       ($ :div.flow-wrapper {:ref react-flow-wrapper}
          ($ ReactFlow {:edges            edges
                        :nodes            nodes
                        :node-types       #js {:editor-node editor-node}
                        :on-connect       on-connect

                        :on-connect-start on-connect-start
                        :on-connect-end   on-connect-end
                        :on-nodes-change  on-nodes-change
                        :on-edges-change  on-edges-change}
             ($ Background)
             ($ Controls))))))

(defui workflow []
  ($ ReactFlowProvider
     ($ flow)))