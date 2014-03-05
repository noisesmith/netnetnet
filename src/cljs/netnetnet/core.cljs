(ns netnetnet.core
  (:require 
   [cljs.core.async :refer [chan <! >! put!]]
   [cljs.reader :as reader]
   [om.core :as om :include-macros true]
   [netnetnet.connect :as connect]
   [netnetnet.util :as util]
   [netnetnet.shapes :as shapes])
  (:require-macros 
   [cljs.core.async.macros :refer [go]]))

(def send (chan))
(def receive (chan))

(def ws-url "ws://localhost:19991/async")
(def ws (new js/WebSocket ws-url))

(def app-state
  (atom {:tick 0
         :opts {:canvas {:width 2000
                         :height 1000}
                :model {:width 100
                        :padding 50}
                :field {:padding 3
                        :height 20}}
         :models {:blog0
                  {:id 1
                   :fields [{:type "collection"
                             :name "posts"
                             :model-id 1
                             :target-id 2}]}
                  :blog-post0
                  {:id 2
                   :fields [{:type "part"
                             :name "blog"
                             :model-id 2
                             :target-id 1}]}
                  :blog1
                  {:id 3
                   :fields [{:type "string"
                             :name "foo"}
                            {:type "collection"
                             :name "posts"
                             :model-id 3
                             :target-id 4}]}
                  :blog-post1
                  {:id 4
                   :fields [{:type "part"
                             :name "blog"
                             :model-id 4
                             :target-id 3}]}
                  :blog2
                  {:id 5
                   :fields [{:type "string"
                             :name "foo"}
                            {:type "number"
                             :name "bar"}
                            {:type "collection"
                             :name "posts"
                             :model-id 5
                             :target-id 6}]}
                  :blog-post2
                  {:id 6
                   :fields [{:type "part"
                             :name "blog"
                             :model-id 6
                             :target-id 5}]}
                  :blog3
                  {:id 7
                   :fields [{:type "string"
                             :name "foo"}
                            {:type "number"
                             :name "bar"}
                            {:type "text"
                             :name "baz"}
                            {:type "collection"
                             :name "posts"
                             :model-id 7
                             :target-id 8}]}
                  :blog-post3
                  {:id 8
                   :fields [{:type "part"
                             :name "blog"
                             :model-id 8
                             :target-id 7}]}}}))

(defn models-display
  [state owner]
  (reify
    om/IWillMount
    (will-mount [_])
    om/IRender
    (render [arg]
      (shapes/models state arg))))

(defn debug
  [state owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (js/setInterval
       (fn [] (om/transact! state :ticks inc))
       10))
    om/IRender
    (render [arg]
      (util/log "ticking ticking ticking")
      (shapes/debug-impl (:ticks state)))))

(defn init
  [data]
  #_ (om/root
      debug
      app-state
      {:target (. js/document (getElementById "debug"))})
  (om/root
   models-display
   app-state
   {:target (. js/document (getElementById "models"))}))

(defn dispatch-message
  []
  (go
   (while true
     (let [msg (<! receive)
           raw (.-data msg)
           data (reader/read-string raw)]
       (case (:op data)
         :init (init data)
         (util/log (str "op not supported! " data)))))))

(defn make-sender
  []
  (go
   (while true
     (let [[id event data] (<! send)]
       (condp = id
         :click (util/log "click!"))))))

(defn make-receiver []
  (set! 
   (.-onmessage ws)
   (fn [msg]
     (put! receive msg)))
  (set!
   (.-onopen ws)
   (fn [msg] 
     (.send ws {:op :init})))
  (dispatch-message))

(defn init!
  []
  (make-sender)
  (make-receiver))

(def on-load
  (set! (.-onload js/window) init!))

(connect/connect)
