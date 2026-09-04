;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer.hooks.animation-wasm
  "Per-frame WASM playback for shape keyframe animations.

  For each animated shape under the viewed frame: select it with the
  wasm `use-shape` context, push the sampled scalar values through
  the incremental `set-shape-*` bridge (selrect for geometry, direct
  setters for rotation/opacity/corners), then re-render the frame
  synchronously. Serialized on the global wasm render queue so it
  never interleaves with snapshot renders."
  (:require
   [app.common.files.helpers :as cfh]
   [app.common.geom.rect :as grc]
   [app.common.types.shape.animation :as ctsan]
   [app.common.types.shape.animation.sample :as ctss]
   [app.main.render-viewer-wasm :as rwv]
   [app.render-wasm.api :as wasm.api]))

(defn- apply-sampled!
  "Push sampled values into the currently selected wasm shape.
  Geometry (x/y/w/h) is folded into a rebuilt selrect (x1/y1/x2/y2
  as the wasm bridge expects); rotation, opacity and radii have
  direct setters."
  [shape sampled]
  (let [geom-track? (some (partial contains? sampled)
                          [[:x] [:y] [:width] [:height]])]

    (when geom-track?
      (let [x'     (get sampled [:x] (:x shape))
            y'     (get sampled [:y] (:y shape))
            width' (get sampled [:width] (:width shape))
            height' (get sampled [:height] (:height shape))
            rect   (grc/make-rect x' y' width' height')]
        (wasm.api/set-shape-selrect rect)))

    (when (contains? sampled [:rotation])
      (wasm.api/set-shape-rotation (get sampled [:rotation])))

    (when (contains? sampled [:opacity])
      (wasm.api/set-shape-opacity (get sampled [:opacity])))

    (when (some (partial contains? sampled) [[:r1] [:r2] [:r3] [:r4]])
      (wasm.api/set-shape-corners [(get sampled [:r1] (:r1 shape))
                                   (get sampled [:r2] (:r2 shape))
                                   (get sampled [:r3] (:r3 shape))
                                   (get sampled [:r4] (:r4 shape))]))

    ;; fills cross-fade: replace the animated index with the sampled
    ;; fill and push the whole fills seq through the bridge
    (let [fill-paths (filter #(= :fills (first %)) (keys sampled))]
      (when (seq fill-paths)
        (let [fills (vec (or (:fills shape) []))]
          (doseq [path fill-paths
                  :let [index (second path)]
                  :when (< index (count fills))]
            (wasm.api/set-shape-fills
             (:id shape)
             (assoc fills index (get sampled path))
             false)))))))

(defn render-animation-frame!
  "Advance wasm playback: sample every animated shape under `frame-id`
  at `t-ms` and re-render the frame. Enqueued on the wasm render
  queue; a no-op when the wasm module or the frame is missing."
  [objects frame-id t-ms]
  (when (and (wasm.api/initialized?) (get objects frame-id))
    (rwv/enqueue-wasm-render!
     (fn []
       (let [ids (cfh/get-children-ids-with-self objects frame-id)]
         (doseq [id ids]
           (let [shape (get objects id)]
             (when-let [animation (:animation shape)]
               (when-not (ctsan/animation-empty? animation)
                 (let [sampled (ctss/sample animation t-ms)]
                   (when (seq sampled)
                     (wasm.api/use-shape id)
                     (apply-sampled! shape sampled)))))))
          (wasm.api/render-sync-shape frame-id))))))
