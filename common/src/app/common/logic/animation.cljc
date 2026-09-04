;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL
;;
;; Smart animate: given two snapshots of the same shape (before/after
;; states), generate an `:animation` that interpolates between them.
;;
;; Design notes:
;; - Same shape id, different attr values -> tracks for changed
;;   scalar properties.
;; - Structurally-equal fills/strokes are not handled in this phase:
;;   Phase 0 restricted keyframe values to scalars (number/boolean/
;;   string). Non-scalar changes fall back to a step track when a
;;   scalar projection exists, otherwise the property is skipped.
;; - Children of frames/bools/groups: matched by id; each pair is
;;   diffed recursively and merged into one animation map keyed by
;;   shape id (see `smart-animate-tree`).

(ns app.common.logic.animation
  (:require
   [app.common.files.helpers :as cfh]
   [app.common.types.shape.animation :as ctsan]))

;; Properties eligible for smart animate, with the value selector
;; used to compare and to store as keyframe values. All scalars.
(def ^:private smart-props
  [[:opacity :opacity]
   [:x :x]
   [:y :y]
   [:width :width]
   [:height :height]
   [:rotation :rotation]
   [:r1 :r1]
   [:r2 :r2]
   [:r3 :r3]
   [:r4 :r4]])

(def default-smart-duration 300)

(defn- scalar-value
  "Value of a smart-animatable property on a shape, or nil when the
   shape has none. Numbers only: booleans/strings are legal keyframe
   values but are compared with = so they produce step tracks; keep
   the selector numeric for now."
  [shape prop]
  (let [v (get shape prop)]
    (when (number? v)
      v)))

(defn- plain-vec
  "Shape attribute as a plain vector (fills may be a wrapped record)."
  [shape attr]
  (when-let [v (get shape attr)]
    (vec v)))

(defn- plain-fill
  "Normalize a fill entry into the plain fill-map form used as a
  keyframe value."
  [fill]
  (cond-> (select-keys fill [:fill-color :fill-opacity :fill-color-gradient])
    ;; round numbers: keyframe values are compared with = and the
    ;; schema treats them as safe-numbers
    (contains? fill :fill-opacity)
    (update :fill-opacity identity)))

(defn- structured-tracks
  "Tracks for the structured paint values (fills/strokes/shadow/blur).
  Index-aligned: only entries that exist on BOTH sides and changed
  produce tracks; nothing is invented."
  [before after duration-ms]
  (let [entries
        (concat
         ;; fills / strokes: per-index
         (mapcat (fn [[idx [f0 f1]]]
                   (when (and (some? f0) (some? f1) (not= f0 f1))
                     [[[:fills idx] f0 f1]]))
                 (map-indexed vector (map vector (plain-vec before :fills)
                                          (plain-vec after :fills))))
         (mapcat (fn [[idx [s0 s1]]]
                   (when (and (some? s0) (some? s1) (not= s0 s1))
                     [[[:strokes idx] s0 s1]]))
                 (map-indexed vector (map vector (plain-vec before :strokes)
                                          (plain-vec after :strokes))))
         ;; shadow: first entry only (the schema caps shapes at one)
         (let [s0 (first (plain-vec before :shadow))
               s1 (first (plain-vec after :shadow))]
           (when (and (some? s0) (some? s1) (not= s0 s1))
             [[[:shadow 0] s0 s1]]))
         ;; blur / background-blur: whole maps
         (for [attr [:blur :background-blur]]
           (let [b0 (get before attr)
                 b1 (get after attr)]
             (when (and (some? b0) (some? b1) (not= b0 b1))
               [[attr] b0 b1]))))]

    (into {}
          (keep (fn [[path v0 v1]]
                  (let [v0' (if (= :fills (first path)) (plain-fill v0) v0)
                        v1' (if (= :fills (first path)) (plain-fill v1) v1)]
                    (when (and (some? v0') (some? v1'))
                      [path
                       [(ctsan/make-keyframe {:t 0 :value v0'})
                        (ctsan/make-keyframe {:t duration-ms :value v1'
                                              :easing :ease-in-out})]]))))
          entries)))

(defn smart-animate
  "Generate an animation map for a single shape moving from `before`
   to `after` over `duration-ms`. Changed scalar properties and
   index-aligned structured paint values (fills, strokes, shadow,
   blur) produce tracks. Returns an animation map (possibly with empty
   :tracks when nothing changed)."
  ([before after]
   (smart-animate before after default-smart-duration))
  ([before after duration-ms]
   (let [scalar-tracks
         (into {}
               (keep (fn [[prop]]
                       (let [v0 (scalar-value before prop)
                             v1 (scalar-value after prop)]
                         (when (and (some? v0) (some? v1) (not= v0 v1))
                           [(vector prop)
                            [(ctsan/make-keyframe {:t 0 :value v0})
                             (ctsan/make-keyframe {:t duration-ms :value v1
                                                   :easing :ease-in-out})]]))))
               smart-props)]
     (ctsan/make-animation (merge scalar-tracks
                                  (structured-tracks before after duration-ms))))))

(defn smart-animate?
  "True when the two snapshots would produce at least one track."
  [before after]
  (pos? (count (:tracks (smart-animate before after)))))

(defn smart-animate-tree
  "Smart animate across a shape tree: the root shape and every
   descendant matched by id between the two objects maps. Returns a
   map of shape-id -> animation (only shapes with at least one
   track)."
  ([objects-before objects-after root-id]
   (smart-animate-tree objects-before objects-after root-id default-smart-duration))
  ([objects-before objects-after root-id duration-ms]
   (let [ids (cfh/get-children-ids-with-self objects-before root-id)]
     (into {}
           (keep (fn [id]
                   (let [before (get objects-before id)
                         after  (get objects-after id)]
                     (when (and (some? before) (some? after))
                       (let [anim (smart-animate before after duration-ms)]
                         (when (pos? (count (:tracks anim)))
                           [id anim])))))
                 ids)))))
