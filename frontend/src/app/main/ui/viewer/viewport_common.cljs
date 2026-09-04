;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer.viewport-common
  "Shared object preparation for viewer viewports (SVG and WASM)."
  (:require
   [app.common.data :as d]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape.animation.sample :as ctss]
   [app.common.uuid :as uuid]))

(defn apply-animations
  "Overlay sampled keyframe values onto prepared (vbox-space) objects
  for playback at time `t-ms`. `vector` is the negated viewport offset
  that `prepare-objects` moved shapes by; positional tracks (x/y) are
  shifted by it so they land in the same space. Only shapes inside
  `frame-id` with an `:animation` are touched. Returns objects
  unchanged when nothing animates."
  [objects frame-id t-ms vector]
  (let [ids (cfh/get-children-ids-with-self objects frame-id)]
    (reduce
     (fn [objects id]
       (let [shape (get objects id)]
         (if-let [animation (:animation shape)]
           (let [sampled (ctss/sample animation t-ms)]
             (if (empty? sampled)
               objects
               (reduce-kv (fn [shape path value]
                            (case (first path)
                              :x (assoc shape :x (+ value (:x vector)))
                              :y (assoc shape :y (+ value (:y vector)))
                              (assoc shape (first path) value)))
                          shape sampled)))
           objects)))
     objects
     ids)))

(defn prepare-objects
  [frame size delta objects]
  (let [frame-id  (:id frame)
        vector  (-> (gpt/point (:x size) (:y size))
                    (gpt/add delta)
                    (gpt/negate))
        update-fn #(d/update-when %1 %2 gsh/transform-shape (ctm/move-modifiers vector))]
    (->> (cfh/get-children-ids objects frame-id)
         (into [frame-id])
         (reduce update-fn objects))))

(defn get-fixed-ids
  [objects]
  (let [fixed-ids (filter cfh/fixed-scroll? (vals objects))

        fixed-children-ids
        (into #{} (mapcat #(cfh/get-children-ids objects (:id %)) fixed-ids))

        parent-children-ids
        (->> fixed-ids
             (mapcat #(cons (:id %) (cfh/get-parent-ids objects (:id %))))
             (remove #(= % uuid/zero)))

        fixed-ids
        (concat fixed-children-ids parent-children-ids)]
    fixed-ids))

(defn frame-fixed-mask-ids
  "Fixed-layer shape ids inside `frame-id` (same rules as `get-fixed-ids`)."
  [objects frame-id]
  (when frame-id
    (let [subtree (into #{} (cfh/get-children-ids-with-self objects frame-id))]
      (into #{}
            (filter #(contains? subtree %)
                    (get-fixed-ids objects))))))

(defn prepare-page-objects
  "Transform all page objects into vbox-space (for overlay positioning)."
  [objects size delta]
  (let [vector (-> (gpt/point (:x size) (:y size))
                   (gpt/add delta)
                   (gpt/negate))
        update-fn #(d/update-when %1 %2 gsh/transform-shape (ctm/move-modifiers vector))
        ids (->> (keys objects) (remove #(= % uuid/zero)))]
    (reduce update-fn objects ids)))

(defn viewer-scale
  [size]
  (if (and (:base-width size) (pos? (:base-width size)))
    (/ (:width size) (:base-width size))
    1))
