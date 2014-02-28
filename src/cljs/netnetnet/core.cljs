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
  (atom {:opts {:canvas {:width 1024
                         :height 1000}
                :model {:width 100
                        :padding 50}
                :field {:padding 3
                        :height 20}}
         :models {:blog
                  {:id 1
                   :fields [{:type "collection"
                             :name "posts"
                             :model-id 1
                             :target-id 2}]}
                  :blog-post
                  {:id 2
                   :fields [{:type "part"
                             :name "blog"
                             :model-id 2
                             :target-id 1}]}}}))

(defn models-display
  [state owner]
  (reify
    om/IWillMount
    (will-mount [_])
    om/IRender
    (render [arg]
      (shapes/models state arg))))

(defn init
  [data]
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

