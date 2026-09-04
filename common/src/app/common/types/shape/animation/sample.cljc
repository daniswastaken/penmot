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
   [app.common.types.color :as ctc]
   [app.common.types.shape.animation :as ctsan]
   [app.common.types.shape.animation.easing :as ctse]))

(declare sample-keyframes)

(defn- lerp
  [v0 v1 p]
  (+ v0 (* p (- v1 v0))))

(defn- mix-fill
  "Cross-fade two solid-color fill maps: lerp RGB and opacity."
  [v0 v1 p]
  (let [[r0 g0 b0] (ctc/hex->rgb (:fill-color v0 "#000000"))
        [r1 g1 b1] (ctc/hex->rgb (:fill-color v1 "#000000"))
        o0 (or (:fill-opacity v0) 1)
        o1 (or (:fill-opacity v1) 1)]
    (cond-> {}
      true
      (assoc :fill-color (ctc/rgb->hex [(lerp r0 r1 p)
                                        (lerp g0 g1 p)
                                        (lerp b0 b1 p)]))

      (or (contains? v0 :fill-opacity)
          (contains? v1 :fill-opacity))
      (assoc :fill-opacity (lerp o0 o1 p)))))

(defn- mix-gradient
  "Interpolate two gradient fills: lerp the geometry (start/end/width)
   and per-stop color/opacity. Requires the same gradient type and
   stop count; anything else steps (caller keeps the previous
   value)."
  [v0 v1 p]
  (let [g0 (:fill-color-gradient v0)
        g1 (:fill-color-gradient v1)]
    (if (or (not= (:type g0) (:type g1))
            (not= (count (:stops g0)) (count (:stops g1))))
      v0
      (let [stops (mapv (fn [s0 s1]
                          (let [[r0 g0' b0] (ctc/hex->rgb (:color s0 "#000000"))
                                [r1 g1' b1] (ctc/hex->rgb (:color s1 "#000000"))]
                            (cond-> {:color (ctc/rgb->hex [(lerp r0 r1 p)
                                                           (lerp g0' g1' p)
                                                           (lerp b0 b1 p)])}
                              (or (contains? s0 :opacity)
                                  (contains? s1 :opacity))
                              (assoc :opacity (lerp (or (:opacity s0) 1)
                                                    (or (:opacity s1) 1)
                                                    p)))))
                        (:stops g0)
                        (:stops g1))]
        {:fill-color-gradient
         (-> g0
             (assoc :start-x (lerp (:start-x g0) (:start-x g1) p))
             (assoc :start-y (lerp (:start-y g0) (:start-y g1) p))
             (assoc :end-x   (lerp (:end-x g0) (:end-x g1) p))
             (assoc :end-y   (lerp (:end-y g0) (:end-y g1) p))
             (assoc :width  (lerp (:width g0) (:width g1) p))
             (assoc :stops stops))}))))

(defn- mix-stroke
  "Interpolate two solid strokes: lerp color, opacity and width.
   Non-numeric/step fields take the ending value at p>0 (they cannot
   be lerped)."
  [v0 v1 p]
  (cond-> {}
    true
    (merge (dissoc v1 :stroke-color :stroke-opacity :stroke-width))

    (and (:stroke-color v0) (:stroke-color v1))
    (assoc :stroke-color
           (let [[r0 g0 b0] (ctc/hex->rgb (:stroke-color v0 "#000000"))
                 [r1 g1 b1] (ctc/hex->rgb (:stroke-color v1 "#000000"))]
             (ctc/rgb->hex [(lerp r0 r1 p)
                            (lerp g0 g1 p)
                            (lerp b0 b1 p)])))

    (and (contains? v0 :stroke-opacity)
         (contains? v1 :stroke-opacity))
    (assoc :stroke-opacity (lerp (:stroke-opacity v0)
                                 (:stroke-opacity v1)
                                 p))

    (and (contains? v0 :stroke-width)
         (contains? v1 :stroke-width))
    (assoc :stroke-width (lerp (:stroke-width v0)
                               (:stroke-width v1)
                               p))))

(defn- mixable?
  [v0 v1]
  (cond
    (and (number? v0) (number? v1)) true
    (and (map? v0) (map? v1)
         (:fill-color v0) (:fill-color v1)) true
    (and (map? v0) (map? v1)
         (:fill-color-gradient v0) (:fill-color-gradient v1)) true
    (and (map? v0) (map? v1)
         (:stroke-color v0) (:stroke-color v1)) true
    :else false))

(defn- mix
  [v0 v1 p]
  (cond
    (and (number? v0) (number? v1)) (lerp v0 v1 p)
    (and (map? v0) (:fill-color-gradient v0)) (mix-gradient v0 v1 p)
    (and (map? v0) (:stroke-color v0)) (mix-stroke v0 v1 p)
    (and (map? v0) (map? v1)) (mix-fill v0 v1 p)
    :else v0))

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
      ;; unmixable values cannot be interpolated: step, keeping the
      ;; previous value until the keyframe instant
      (not (mixable? v0 v1)) v0
      :else
      (let [p    (double (/ (- t t0) (- t1 t0)))
            ease (ctse/ease-fn (:easing kf-next ctsan/default-easing)
                               (:easing-params kf-next))
            p'   (double (ease p))]
        (cond
          ;; exact endpoints: never round-trip through the mixer
          (<= p' 0.0) v0
          (>= p' 1.0) v1
          :else (mix v0 v1 p'))))))

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
