(ns netnetnet.core
  (:require 
   [clojure.string :as string]
   [cljs.core.async :refer [chan <! >! put!]]
   [cljs.reader :as reader]
   [singult.core :as sing]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [netnetnet.connect :as connect])
  (:require-macros 
   [cljs.core.async.macros :refer [go]]))

(def send (chan))
(def receive (chan))

(def ws-url "ws://localhost:19991/async")
(def ws (new js/WebSocket ws-url))

(defn log
  [e]
  (.log js/console e))

(def app-state 
  (atom {:count 0}))

(defn boxy
  [state owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (js/setInterval
       (fn []
         (om/transact! state :count inc))
       50))
    om/IRender
    (render [_]
      (dom/div
       nil
       (dom/svg
        #js {:width 1024
             :height 1000}
        (dom/rect #js {:x (* 300 (+ 1 (Math/sin (/ (:count state) 100))))
                       :y (* 103 (+ 1 (Math/cos (/ (:count state) 69))))
                       :width (+ 100 (* 10 (mod (/ (:count state) 200) 50)))
                       :height (+ 20 (* 30 (mod (- 100 (/  (:count state) 133)) 31)))
                       :style {}
                       :fill "rgb(0,0,255)"
                       :strokeWidth "3"
                       :stroke "rgb(255,255,0)"}))))))

(defn init
  [data]
  (log "init")
  (om/root
   boxy
   app-state
   {:target (. js/document (getElementById "boxy"))}))

(defn dispatch-message
  []
  (go
   (while true
     (let [msg (<! receive)
           raw (.-data msg)
           data (reader/read-string raw)]
       (condp = (:op data)
         :init (init data)
         (log (str "op not supported! " data)))))))

(defn make-sender
  []
  (log "HELLO")
  (go
   (while true
     (let [[id event data] (<! send)]
       (condp = id
         :click (log "click!"))))))

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

