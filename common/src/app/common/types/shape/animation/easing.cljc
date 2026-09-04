;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL
;;
;; Easing functions for keyframe interpolation. Pure and total:
;; `ease` maps a normalized progress `p` in [0,1] to eased progress,
;; roughly in [0,1] (springs may overshoot slightly).
;;
;; The easing of a segment always belongs to the keyframe the segment
;; ENDS on: interpolating from keyframe A to keyframe B uses B's
;; `:easing`.

(ns app.common.types.shape.animation.easing
  (:require
   [app.common.math :as mth]
   [app.common.types.shape.animation :as ctsan]))

(def ^:private e-base
  #?(:cljs js/Math.E
     :clj Math/E))

(defn bezier-ease
  "Cubic bezier easing with explicit control points (CSS
   cubic-bezier(x1,y1,x2,y2) semantics). Bisection on the
   x-projection: exact to tolerance, dependency-free."
  [{:keys [x1 y1 x2 y2] :or {x1 0.42 y1 0.0 x2 0.58 y2 1.0}}]
  (let [;; x(t) = 3(1-t)^2 t x1 + 3(1-t) t^2 x2 + t^3
        fx (fn [t]
             (+ (* 3 (- 1 t) (- 1 t) t x1)
                (* 3 (- 1 t) t t x2)
                (* t t t)))
        ;; y(t) = same form with y1/y2
        fy (fn [t]
             (+ (* 3 (- 1 t) (- 1 t) t y1)
                (* 3 (- 1 t) t t y2)
                (* t t t)))]
    (fn [p]
      (cond
        (<= p 0) 0
        (>= p 1) 1
        :else
        (loop [lo 0.0
               hi 1.0
               iterations 0]
          (if (>= iterations 32)
            (fy (/ (+ lo hi) 2))
            (let [mid (/ (+ lo hi) 2)
                  x   (fx mid)]
              (cond
                (mth/close? x p 0.00001) (fy mid)
                (< x p)                 (recur mid hi (inc iterations))
                :else                   (recur lo mid (inc iterations))))))))))

(defn spring-ease
  "Damped harmonic oscillator from x(0)=0 toward target 1 with
   x'(0)=0, sampled at normalized time p.

   Params: `:stiffness` k, `:damping` c, `:mass` m. Underdamped
   springs overshoot (intended, capped for float sanity). At p=1 the
   spring snaps to exactly 1 so keyframes always land on their
   value."
  [{:keys [stiffness damping mass] :or {stiffness 170 damping 26 mass 1.0}}]
  (let [m      (max mass 0.001)
        k      (max stiffness 0.001)
        c      (max damping 0.0)
        omega0 (mth/sqrt (/ k m))                 ;; natural frequency
        zeta   (/ c (* 2 (mth/sqrt (* k m))))      ;; damping ratio
        eps    0.001]
    (fn [p]
      (cond
        (<= p 0) 0
        (>= p 1) 1
        ;; overdamped / critically damped: monotone approach, no
        ;; overshoot. e^(-zeta w0 t) decay envelope toward 1.
        (>= zeta 1)
        (let [v (- 1 (mth/pow e-base (* -1 zeta omega0 p)))]
          (if (mth/close? v 1 eps) 1 (max 0 v)))

        ;; underdamped: x(t) = 1 - e^(-zeta w0 t) (cos(wd t) + zeta/sqrt(1-zeta^2) sin(wd t))
        :else
        (let [s     (mth/sqrt (- 1 (* zeta zeta)))
              wd    (* omega0 s)
              decay (mth/pow e-base (* -1 zeta omega0 p))
              phase (/ zeta s)
              x     (- 1 (* decay (+ (mth/cos (* wd p))
                                     (* phase (mth/sin (* wd p))))))]
          (cond
            (mth/close? x 1 eps)      1
            :else                     (mth/clamp x 0 1.15)))))))

(defn ease-fn
  "Resolve an easing keyword + params into a unary function of
   normalized progress. Unknown easings fall back to linear —
   sampling must never throw."
  ([easing]
   (ease-fn easing nil))
  ([easing params]
   (let [params (or params {})]
     (case easing
       :linear       identity
       :ease         (bezier-ease {:x1 0.25 :y1 0.1 :x2 0.25 :y2 1.0})
       :ease-in      (bezier-ease {:x1 0.42 :y1 0.0 :x2 1.0 :y2 1.0})
       :ease-out     (bezier-ease {:x1 0.0 :y1 0.0 :x2 0.58 :y2 1.0})
       :ease-in-out  (bezier-ease {:x1 0.42 :y1 0.0 :x2 0.58 :y2 1.0})
       :bezier       (bezier-ease (merge ctsan/bezier-defaults params))
       :spring       (spring-ease (merge ctsan/spring-defaults params))
       :hold         (fn [p] (if (>= p 1) 1 0))
       ;; unknown: never throw during sampling
       identity))))

(def presets
  "Easing presets usable as keyframe `:easing` values with their
   default params. Exposed for UI listings."
  {:linear       {}
   :ease         {:x1 0.25 :y1 0.1 :x2 0.25 :y2 1.0}
   :ease-in      {:x1 0.42 :y1 0.0 :x2 1.0 :y2 1.0}
   :ease-out     {:x1 0.0 :y1 0.0 :x2 0.58 :y2 1.0}
   :ease-in-out  {:x1 0.42 :y1 0.0 :x2 0.58 :y2 1.0}
   :bezier       ctsan/bezier-defaults
   :spring       ctsan/spring-defaults
   :hold         {}})

(defn valid-easing?
  [easing]
  (contains? presets easing))
