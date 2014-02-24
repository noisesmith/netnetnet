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
  (atom {:count 0
         :list ["okay" "what" "the" "penguin"]}))

(defn widget
  [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil (:text data)))))

(defn counter
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
      (dom/div nil (:count state)))))

(defn penguin
  [state owner]
  (apply 
   dom/ul nil
   (map (fn [x] (dom/li nil x)) (shuffle (:list state)))))

(defn init
  [data]
  (log "init")
  (om/root
   penguin
   app-state
   {:target (. js/document (getElementById "site"))})
  (om/root
   counter
   app-state
   {:target (. js/document (getElementById "counter"))}))

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

