;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL
;;
;; Sampling engine: evaluate an animation at a time instant and get
;; the animated property values.
;;
;; `sample` is pure and total: any time t and any (schema-valid)
;; animation produce a map of property-path -> value. Properties whose
;; track has only keyframes at times > t are not present yet; the
;; timeline starts at the first keyframe of each track.

(ns app.common.types.shape.animation.sample
  (:require
   [app.common.types.shape.animation :as ctsan]
   [app.common.types.shape.animation.easing :as ctse]))

(declare sample-keyframes)

(defn- segment-sample
  "Sample the segment [kf-prev, kf-next] at time t. The segment's
   easing lives on the ending keyframe. A `:hold` ending keyframe
   keeps the previous value until the instant itself (a step)."
  [t kf-prev kf-next]
  (let [t0 (:t kf-prev)
        t1 (:t kf-next)
        v0 (:value kf-prev)
        v1 (:value kf-next)]
    (cond
      ;; hold: previous value until t1, then step (caller already
      ;; handles t >= t1 by returning the keyframe value directly)
      (true? (:hold kf-next)) v0
      ;; zero-length segment: target value
      (<= t1 t0) v1
      ;; non-numeric values cannot be interpolated: step, keeping the
      ;; previous value until the keyframe instant
      (not (and (number? v0) (number? v1))) v0
      :else
      (let [p    (double (/ (- t t0) (- t1 t0)))
            ease (ctse/ease-fn (:easing kf-next ctsan/default-easing)
                               (:easing-params kf-next))
            p'   (ease p)]
        (+ v0 (* p' (- v1 v0)))))))

(defn- sample-keyframes
  "Value of one track's keyframe vector at time t."
  [t kfs]
  (let [first-kf (first kfs)]
    (cond
      ;; before the first keyframe: the track is not active yet
      (< t (:t first-kf)) nil
      ;; at or after the last keyframe: hold the last value
      (>= t (:t (peek kfs))) (:value (peek kfs))
      :else
      (loop [prev first-kf
             rest (next kfs)]
        (let [next-kf (first rest)]
          (cond
            (nil? next-kf) (:value prev)
            (= t (:t next-kf)) (:value next-kf)
            (< t (:t next-kf)) (segment-sample t prev next-kf)
            :else (recur next-kf (next rest))))))))

(defn sample
  "Evaluate `animation` at time `t` (ms). Returns a map of
   property-path -> value for every track whose first keyframe instant
   has been reached. Pure; safe on any schema-valid animation."
  [animation t]
  (into {}
        (keep (fn [{:keys [property keyframes]}]
                (when-let [value (sample-keyframes t keyframes)]
                  [property value])))
        (:tracks animation)))

(defn sample-at-percent
  "Sample at a fraction of the animation duration."
  [animation fraction]
  (sample animation (long (* (ctsan/duration animation) (double fraction)))))
