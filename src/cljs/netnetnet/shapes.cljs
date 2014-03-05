(ns netnetnet.shapes
  (:require [clojure.string :as string]
            [om.dom :as dom :include-macros true]
            [netnetnet.util :as util]))

(defn field->rect
  [x y {{model-width :width
         model-padding :padding} :model
        {field-padding :padding
         field-height :height} :field}]
  (let [body-x (+ x field-padding)
        label-x (+ body-x field-padding)
        body-width (- model-width (* 2 field-padding))
        label-width (- body-width (* 2 field-padding))
        body-height field-height
        label-height (- body-height (* 2 field-padding))]
    (fn field->rect-placed
      [j field]
      (let [body-y (+ y field-padding (* j (+ field-height field-padding)))
            ;; text is placed from the bottom line
            label-y (+ body-y (- field-height field-padding))]
        [[(dom/rect
           #js {:x body-x
                :y body-y
                :width body-width
                :height body-height
                :style {}
                :fill "#999988"
                :strokeWidth 2
                :stroke "#888899"})
          (dom/text
           #js {:x label-x
                :y label-y
                :width label-width
                :height label-height
                :fill "#000000"}
           (str (:name field)
                (when-let [val  (:value field)]
                  (str " " val))))]
         (when (contains? #{"link" "part" "collection"} (:type field))
           [{:type (:type field)
             :target (:target-id field)
             :model (:model-id field)
             :x body-x
             :y (+ body-y (/ field-height 2))}])]))))

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
                                   (let [[this-shape this-arrow]
                                         (get-shapes count field)]
                                     [(concat shapes this-shape)
                                      (concat arrows this-arrow)
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
  (string/join " " ["M" (+ x width) y "L" x' y' "z"]))

(defn line
  [width]
  (fn [pair]))

(defn slope
  [{x :x y :y} {x' :x y' :y}]
  (/ (- y' y) (- x' x)))

(defn antislope
  [x y]
  (/ -1 (slope x y)))

(defn make-arrowhead
  [{{padding :padding} :field} invert]
  (let [x 0
        x' (* 4 padding)]
    (if invert
      (string/join " " ["M" x padding
                        "L" x (- padding)
                        "L" x' 0
                        "L" x padding
                        "z"])
      (string/join " " ["M" x (- padding)
                        "L" x padding
                        "L" (- x') 0
                        "L" x (- padding)
                        "z"]))))

(defn reverse-rotate
  [from to]
  (* (/ 180 Math/PI) (Math/atan (slope from to))))

(defn arrow
  [opts]
  (fn [[from to]]
    (util/log (str "arrow from: " (select-keys a [:x :y]) " to " (select-keys b [:x :y])))
    (let [padding (-> opts :field :padding)
          x-dest (- (:x to) padding padding)
          y-dest (:y to)]
      [(dom/path #js {:d (make-path from {:x x-dest :y y-dest} opts)
                       :strokeWidth 3
                       :stroke "#ffffff"})
       (dom/g #js {:transform (str "translate(" x-dest "," y-dest ")")}
              (dom/path #js {:d (make-arrowhead opts (<= (:x from) (:x to)))
                             :transform (str "rotate(" (reverse-rotate from to) " 0 0)")
                             :fill "#ffffff"}))])))

(defn debug-impl
  []
  (dom/div
   nil
   (apply dom/svg
          #js {:width 500
               :height 500}
          (mapcat (fn [i]
                    ((arrow {:field {:padding 3}
                             :model {:width 0}})
                     [{:x 250
                       :y 250}
                      {:x (+ 250 (* 100 (Math/sin (/ i 10))))
                       :y (+ 250 (* 100 (Math/cos (/ i 10))))}]))
                  (range (inc (int (* Math/PI 2 10))))))))

(defn connect-arrows
  "this will turn tail and head halves of arrows into a single path"
  [arrows opts]
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
                      (mapcat (arrow opts) collected))]
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
