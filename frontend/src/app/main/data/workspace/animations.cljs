;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.animations
  "Potok events for the per-shape keyframe animation attribute.
  All mutations go through `dwsh/update-shapes` so the normal change
  pipeline (undo/redo, validation, persistence) applies."
  (:require
   [app.common.data.macros :as dm]
   [app.common.types.shape.animation :as ctsan]
   [app.main.data.workspace.shapes :as dwsh]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn- update-animation
  [shape update-fn]
  (let [animation (dm/get-prop shape :animation)
        animation (update-fn (or animation {:tracks []}))]
    (if (ctsan/animation-empty? animation)
      (dissoc shape :animation)
      (assoc shape :animation animation))))

(defn add-animation-keyframe
  "Add (or replace) a keyframe on `shape-id` for the property path at
  the current property value."
  [{:keys [page-id shape-id property value t] :as params}]
  (ptk/reify ::add-animation-keyframe
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (dwsh/update-shapes [shape-id]
                           (fn [shape]
                             (update-animation
                              shape
                              #(ctsan/add-keyframe %
                                                   property
                                                   (ctsan/make-keyframe
                                                    {:t t
                                                     :value value
                                                     :easing ctsan/default-easing}))))
                           {:page-id page-id})))))

(defn update-animation-keyframe
  "Update the keyframe at `t` on `property` with `update-fn`."
  [{:keys [page-id shape-id property t update-fn] :as params}]
  (ptk/reify ::update-animation-keyframe
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (dwsh/update-shapes [shape-id]
                           (fn [shape]
                             (update-animation
                              shape
                              #(ctsan/update-keyframe % property t update-fn)))
                           {:page-id page-id})))))

(defn remove-animation-keyframe
  "Remove the keyframe at time `t` on `property`."
  [{:keys [page-id shape-id property t] :as params}]
  (ptk/reify ::remove-animation-keyframe
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (dwsh/update-shapes [shape-id]
                           (fn [shape]
                             (update-animation
                              shape
                              #(ctsan/remove-keyframe % property t)))
                           {:page-id page-id})))))

(defn move-animation-keyframe
  "Move the keyframe at `from-t` to `to-t` on `property`."
  [{:keys [page-id shape-id property from-t to-t] :as params}]
  (ptk/reify ::move-animation-keyframe
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (dwsh/update-shapes [shape-id]
                           (fn [shape]
                             (update-animation
                              shape
                              #(ctsan/move-keyframe % property from-t to-t)))
                           {:page-id page-id})))))

(defn add-animation
  "Set (or replace) the whole animation attribute on a shape."
  [{:keys [page-id shape-id animation] :as params}]
  (ptk/reify ::add-animation
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (dwsh/update-shapes [shape-id]
                           (fn [shape]
                             (if (ctsan/animation-empty? animation)
                               (dissoc shape :animation)
                               (assoc shape :animation animation)))
                           {:page-id page-id})))))

(defn update-animation-playback
  "Set playback options (loop/alternate) on the shape animation."
  [{:keys [page-id shape-id playback] :as params}]
  (ptk/reify ::update-animation-playback
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (dwsh/update-shapes [shape-id]
                           (fn [shape]
                             (update-animation
                              shape
                              #(assoc % :playback playback)))
                           {:page-id page-id})))))

(defn remove-animation
  "Drop the whole animation attribute from the shape."
  [{:keys [page-id shape-id] :as params}]
  (ptk/reify ::remove-animation
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (dwsh/update-shapes [shape-id]
                           (fn [shape] (dissoc shape :animation))
                           {:page-id page-id})))))
