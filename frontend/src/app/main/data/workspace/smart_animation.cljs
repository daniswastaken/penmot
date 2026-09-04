;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.data.workspace.smart-animation
  "Events for the smart animate authoring flow: capture a shape
  snapshot, edit the shape, then generate the animation that
  interpolates from the captured state to the current one.

  Snapshots are workspace-local (not persisted, not undoable)."
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.logic.animation :as cla]
   [app.common.types.shape.animation :as ctsan]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.animations :as dwa]
   [potok.v2.core :as ptk]))

(defn capture-smart-animation-snapshot
  "Remember the current state of the selected shapes as the animation
  start state."
  []
  (ptk/reify ::capture-smart-animation-snapshot
    ptk/UpdateEvent
    (update [_ state]
      (let [page-id  (:current-page-id state)
            objects  (dsh/lookup-page-objects state)
            selected (dsh/lookup-selected-raw state)
            snapshot (select-keys objects selected)]
        (assoc-in state [:workspace-local :smart-animation-snapshot]
                  {:page-id page-id
                   :snapshot snapshot})))))

(defn clear-smart-animation-snapshot
  "Drop the captured snapshot."
  []
  (ptk/reify ::clear-smart-animation-snapshot
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local dissoc :smart-animation-snapshot))))

(defn generate-smart-animation
  "Diff the captured snapshot against the current state of the same
  shapes and write the generated animation (per shape) through the
  normal changes pipeline. No-op when no snapshot is captured or
  nothing changed."
  [{:keys [duration-ms] :or {duration-ms cla/default-smart-duration} :as params}]
  (ptk/reify ::generate-smart-animation
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-id  (:current-page-id state)
            objects  (dsh/lookup-page-objects state)
            snap     (dm/get-in state [:workspace-local :smart-animation-snapshot])
            before   (:snapshot snap)
            selected (dsh/lookup-selected-raw state)]

        (when (and (d/not-empty? before)
                   (= page-id (:page-id snap)))
          (let [after (select-keys objects selected)]
            (into []
                  (keep (fn [shape]
                          (let [before-shape (get before (:id shape))]
                            (when (some? before-shape)
                              (let [animation (cla/smart-animate before-shape shape duration-ms)]
                                (when-not (ctsan/animation-empty? animation)
                                  (dwa/add-animation
                                   {:page-id page-id
                                    :shape-id (:id shape)
                                    :animation animation}))))))
                  (vals after)))))))))
