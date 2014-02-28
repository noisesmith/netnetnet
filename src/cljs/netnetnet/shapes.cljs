(ns netnetnet.shapes
  (:require [clojure.string :as string]
            [om.dom :as dom :include-macros true]
            [netnetnet.util :as util]))

(defn field->rect
  [x y {{model-width :width
         model-padding :padding} :model
        {field-padding :padding
         field-height :height} :field}]
  (let [rx (+ x field-padding)
        tx (+ rx field-padding)
        ;; text is placed from the bottom line
        rw (- model-width (* 2 field-padding))
        tw (- rw (* 2 field-padding))
        rh field-height
        th (- rh (* 2 field-padding))]
    (fn field->rect-placed
      [j field]
      (let [ry (+ y field-padding (* j (+ field-height field-padding)))
            ty (+ ry (- field-height field-padding))]
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
         (when (contains? #{"link" "part" "collection"} (:type field))
           [{:type (:type field)
             :target (:target-id field)
             :model (:model-id field)
             :x rx
             :y (+ ry (/ field-height 2))}])]))))

(defn model->rect
  [i [namek model] {{width :width padding :padding} :model :as opts}]
  (let [x (+ width (* i (+ width padding)))
        y padding
        height 400
        fields (conj (seq (:fields model))
                     {:name "id" :value (:id model)}
                     {:name (str namek)})
        get-shapes (field->rect x y opts)
        [shapes arrows _] (reduce (fn [[shapes arrows count] field]
                                   (let [[s a] (get-shapes count field)]
                                     [(concat shapes s)
                                      (concat arrows a)
                                      (inc count)]))
                                 [() () 0]
                                 fields)]
    {:arrows arrows
     :shapes (conj
              shapes
              (dom/rect
               #js {:x x
                    :y y
                    :width width
                    :height height
                    :style {}
                    :fill "#778277"
                    :strokeWidth "5"
                    :stroke "#827782"}))}))

(defn make-path
  [{x :x y :y} {x' :x y' :y} {{width :width} :model
                              {padding :padding} :field}]
  (string/join " " ["M" (+ x width) y "L" (- x' padding padding) y' "z"]))

(defn line
  [width]
  (fn [pair]))

(defn slope
  [{x :x y :y} {x' :x y' :y}]
  (/ (- y' y) (- x' x)))

(defn antislope
  [x y]
  (/ -1 (slope x y)))

(defn arrow
  [opts]
  (fn [[a b]]
    (let [[from to] (sort-by :x [a b])]
      (dom/path #js {:d (make-path from to opts)
                     :strokeWidth 3
                     :stroke "#000000"}))))

(defn connect-arrows
  "this will turn tail and head halves of arrows into a single path"
  [arrows opts]
  (util/log (str "connecting arrows" arrows " : " (count arrows)))
  (let [{{width :width} :model} opts
        {parts "part"
         collections "collection"
         joins "join"} (group-by :type arrows)
        join-up (group-by (juxt :target :model) joins)
        pair-joins (loop [joined [] joinable join-up]
                     (if (empty? joinable)
                       joined
                       (let [[[id model] join] (first joinable)
                             [_ [joinee]] (get joinable [model id])]
                         (recur (conj joined [join joinee])
                                (dissoc joinable [model id] [id model])))))
        part-of (group-by (juxt :target :model) parts)
        collected (map (fn [coll]
                         [coll
                          (first (get part-of ((juxt :model :target) coll)))])
                         collections)
        paths (concat (map (line width) pair-joins)
                      (map (arrow opts) collected))]
    paths))

(defn models
  [state args]
  (let [opts (:opts state)
        make-shapes (fn [[shapes-acc arrows-acc count] model]
                      (let [{:keys [shapes arrows]}
                            (model->rect count model opts)]
                        [(concat shapes-acc shapes)
                         (concat arrows-acc arrows)
                         (inc count)]))
        [shapes arrows] (reduce make-shapes [() () 0] (:models state))
        shapes (concat shapes (connect-arrows arrows opts))]
    (dom/div
     nil
     (apply dom/svg
            #js {:width (-> opts :canvas :width)
                 :height (-> opts :canvas :height)}
            shapes))))
