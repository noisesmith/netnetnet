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
  (-> {}
      (assoc-in
       [:models :blog]
       {:id 1
        :fields [{:type "collection"
                  :name "posts"
                  :target-id 2}]})
      (assoc-in
       [:models :blog-post]
       {:id 2
        :fields [{:type "part"
                  :name "blog"
                  :target-id 1}]})
      atom))

(defn field->rect
  [x y width height]
  (fn field->rect-placed
    [j field]
    (let [rx (+ x 3)
          tx (+ rx 3)
          ry (+ y 10 (* j 30))
          ty (+ ry 15)
          rw (- width 6)
          tw (- rw 6)
          rh 20
          th (- rh 6)]
      [[(dom/rect
          #js {:x rx
               :y ry
               :width rw
               :height rh
               :style {}
               :fill "#999988"
               :strokeWidth 2
               :stroke "#888899"})
         (dom/text
          #js {:x tx
               :y ty
               :width tw
               :height th
               :fill "#000000"}
          (str (:name field)
               (when-let [val  (:value field)]
                 (str " " val))))]
       ;; returning of stub for arrow goes here!
       (when false nil)])))

(defn connect-arrows
  "this will turn tail and head halves of arrows into a single path"
  [arrows])

(defn model->rect
  [i [namek model]]
  (let [x (+ 100 (* i 150))
        y 100
        width 100
        height 400
        fields (conj (seq (:fields model))
                     {:name "id" :value (:id model)}
                     {:name (str namek)})
        get-shapes (field->rect x y width height)
        [shapes arrows _] (reduce (fn [[shapes arrows count] field]
                                   (let [[s a] (get-shapes count field)]
                                     [(concat shapes s)
                                      (concat arrows a)
                                      (inc count)]))
                                 [[] 0]
                                 fields)
        arrows (connect-arrows arrows)]
    (conj
     (concat shapes arrows)
     (dom/rect
      #js {:x x
           :y y
           :width width
           :height height
           :style {}
           :fill "#778277"
           :strokeWidth "5"
           :stroke "#827782"}))))

(defn models-display
  [state owner]
  (reify
    om/IWillMount
    (will-mount [_])
    om/IRender
    (render [arg]
      (log (str (:models state)))
      (dom/div
       nil
       (apply dom/svg
              #js {:width 1024
                   :height 1000}
              (apply concat
                     (map-indexed
                      model->rect
                      (:models state))))))))

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
         (log (str "op not supported! " data)))))))

(defn make-sender
  []
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

