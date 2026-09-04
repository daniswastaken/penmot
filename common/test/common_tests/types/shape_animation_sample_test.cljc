;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.types.shape-animation-sample-test
  (:require
   [app.common.math :as mth]
   [app.common.types.shape.animation :as ctsan]
   [app.common.types.shape.animation.easing :as ctse]
   [app.common.types.shape.animation.sample :as ctss]
   [clojure.test :as t]))

(defn- kf
  [t value & {:as opts}]
  (ctsan/make-keyframe (merge {:t t :value value} opts)))

(t/deftest easing-endpoints
  (doseq [easing [:linear :ease :ease-in :ease-out :ease-in-out
                  :bezier :spring :hold]]
    (let [f (ctse/ease-fn easing)]
      (t/testing (str "easing " easing " endpoints")
        (t/is (zero? (f 0)) (str easing " at 0"))
        (t/is (= 1 (f 1)) (str easing " at 1"))))))

(t/deftest easing-monotone-curves
  (doseq [easing [:linear :ease :ease-in :ease-out :ease-in-out]]
    (let [f (ctse/ease-fn easing)
          ys (map f (range 0 1.001 0.05))
          increasing (map (fn [a b] (<= a b)) ys (rest ys))]
      (t/testing (str "easing " easing " is monotone non-decreasing")
        (t/is (every? true? increasing))))))

(t/deftest easing-linear-is-identity
  (t/is (= 0.37 ((ctse/ease-fn :linear) 0.37))))

(t/deftest easing-hold-stays-at-zero
  (t/is (zero? ((ctse/ease-fn :hold) 0.999))))

(t/deftest easing-bezier-midpoint
  (let [f (ctse/ease-fn :bezier)]
    ;; symmetric ease-in-out curve: y(0.5) = 0.5 by symmetry
    (t/is (mth/close? (f 0.5) 0.5 0.001))))

(t/deftest easing-spring-overshoots-then-returns
  (let [f (ctse/ease-fn :spring {:stiffness 400 :damping 10 :mass 1})
        ys (map f (range 0 1.001 0.01))
        overshoots? (some #(> % 1.001) ys)]
    (t/testing "underdamped spring overshoots somewhere"
      (t/is (true? overshoots?)))
    (t/testing "spring settles to 1 at the end"
      (t/is (= 1 (f 1))))))

(t/deftest easing-spring-overdamped-never-overshoots
  (let [f (ctse/ease-fn :spring {:stiffness 170 :damping 400 :mass 1})
        ys (map f (range 0 1.001 0.01))]
    (t/is (every? #(<= % 1.0) ys))))

(t/deftest easing-unknown-falls-back-to-linear
  (t/is (= 0.42 ((ctse/ease-fn :nope) 0.42))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest sample-basic-linear-interpolation
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 100 0)]})]
    (t/testing "sample mid-segment"
      (t/is (= 0.5 (get (ctss/sample anim 50) [:opacity]))))
    (t/testing "sample at endpoints"
      (t/is (mth/close? 1 (get (ctss/sample anim 0) [:opacity])))
      (t/is (mth/close? 0 (get (ctss/sample anim 100) [:opacity]))))
    (t/testing "after the end holds last value"
      (t/is (mth/close? 0 (get (ctss/sample anim 500) [:opacity]))))))

(t/deftest sample-eased-segment
  (let [anim (ctsan/make-animation
              {[:x] [(kf 0 0) (kf 100 100 :easing :ease-in-out)]})]
    (t/testing "midpoint of symmetric ease-in-out is the middle value"
      (t/is (mth/close? (get (ctss/sample anim 50) [:x]) 50 0.001))
      )

    (t/testing "ease-in-out is slower than linear at the start"
      (t/is (< (get (ctss/sample anim 10) [:x]) 10)))))

(t/deftest sample-hold-is-a-step
  (let [anim (ctsan/make-animation
              {[:opacity] [(kf 0 1) (kf 100 0 :hold true)]})]
    (t/testing "value holds until the hold keyframe instant"
      (t/is (= 1 (get (ctss/sample anim 99) [:opacity])))
      (t/is (mth/close? 0 (get (ctss/sample anim 100) [:opacity]))))))

(t/deftest sample-track-starts-later
  (let [anim (ctsan/make-animation
              {[:opacity] [(kf 100 1) (kf 200 0)]})]
    (t/testing "property absent before its first keyframe"
      (t/is (nil? (get (ctss/sample anim 50) [:opacity])))
      (t/is (nil? (get (ctss/sample anim -10) [:opacity]))))

    (t/testing "property present at its first keyframe"
      (t/is (mth/close? 1 (get (ctss/sample anim 100) [:opacity]))))))

(t/deftest sample-multiple-tracks
  (let [anim (ctsan/make-animation
              {[:opacity] [(kf 0 1) (kf 300 0)]
               [:x] [(kf 0 0) (kf 100 50)]
               [:rotation] [(kf 50 0) (kf 150 180 :easing :linear)]})]
    (let [s (ctss/sample anim 100)]
      (t/testing "each track sampled independently"
        (t/is (mth/close? 0.666 (get s [:opacity]) 0.01))
        (t/is (= 50 (get s [:x])))
        (t/is (mth/close? 90 (get s [:rotation])))))

    (let [s (ctss/sample anim 10)]
      (t/testing "late track not started yet"
        (t/is (nil? (get s [:rotation])))))))

(t/deftest sample-single-keyframe-track
  (let [anim (ctsan/make-animation {[:opacity] [(kf 100 0.5)]})]
    (t/testing "before: absent; from instant on: constant"
      (t/is (nil? (get (ctss/sample anim 99) [:opacity])))
      (t/is (mth/close? 0.5 (get (ctss/sample anim 100) [:opacity])))
      (t/is (= 0.5 (get (ctss/sample anim 999) [:opacity]))))))

(t/deftest sample-string-values-step
  (let [anim (ctsan/make-animation
              {[:grow-type] [(kf 0 "auto-height") (kf 100 "fixed")]})]
    (t/testing "non-numeric values step at keyframe instants"
      (t/is (= "auto-height" (get (ctss/sample anim 50) [:grow-type])))
      (t/is (= "fixed" (get (ctss/sample anim 100) [:grow-type]))))))

(t/deftest sample-at-percent
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 200 0)]})]
    (t/testing "fraction of duration"
      (t/is (= 0.5 (get (ctss/sample-at-percent anim 0.5) [:opacity]))))))

(t/deftest sample-loop-ping-pong
  ;; playback mapping is a runtime concern; sampling stays linear.
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 200 0)]}
                                   :loop :loop)]
    (t/testing "sample is unaffected by playback metadata"
      (t/is (mth/close? 0.75 (get (ctss/sample anim 50) [:opacity]))))))

(t/deftest sample-fill-crossfade
  (let [red   {:fill-color "#ff0000" :fill-opacity 1}
        blue  {:fill-color "#0000ff" :fill-opacity 0.5}
        anim  (ctsan/make-animation
               {[:fills 0] [(kf 0 red) (kf 100 blue)]})]
    (t/testing "fill cross-fades color and opacity at midpoint"
      (let [v (get (ctss/sample anim 50) [:fills 0])]
        (t/is (map? v))
        ;; 255,0,0 -> 0,0,255 at p=0.5 gives 127,0,127 after int rounding
        (t/is (= "#7f007f" (:fill-color v)))
        (t/is (mth/close? 0.75 (:fill-opacity v) 0.001))))

    (t/testing "endpoints are exact"
      (t/is (= red (get (ctss/sample anim 0) [:fills 0])))
      (t/is (= blue (get (ctss/sample anim 100) [:fills 0]))))

    (t/testing "before the first keyframe the track is inactive"
      (t/is (nil? (get (ctss/sample anim -1) [:fills 0]))))))

(t/deftest sample-gradient-interpolation
  (let [lin-a {:fill-color-gradient
               {:type :linear :start-x 0 :start-y 0 :end-x 100 :end-y 0 :width 100
                :stops [{:color "#ff0000" :opacity 1} {:color "#000000" :opacity 0}]}}
        lin-b {:fill-color-gradient
               {:type :linear :start-x 50 :start-y 20 :end-x 150 :end-y 20 :width 200
                :stops [{:color "#0000ff" :opacity 0.5} {:color "#ffffff" :opacity 1}]}}
        anim (ctsan/make-animation
              {[:fills 0] [(kf 0 lin-a) (kf 100 lin-b)]})]
    (t/testing "geometry and stops interpolate at midpoint"
      (let [g (get-in (ctss/sample anim 50) [[:fills 0] :fill-color-gradient])]
        (t/is (= :linear (:type g)))
        (t/is (mth/close? 25 (:start-x g) 0.001))
        (t/is (mth/close? 10 (:start-y g) 0.001))
        (t/is (mth/close? 125 (:end-x g) 0.001))
        (t/is (mth/close? 150 (:width g) 0.001))
        (t/is (= "#7f007f" (get-in g [:stops 0 :color])))
        (t/is (mth/close? 0.75 (get-in g [:stops 0 :opacity]) 0.001))))

    (t/testing "endpoints are exact"
      (t/is (= lin-a (get (ctss/sample anim 0) [:fills 0])))
      (t/is (= lin-b (get (ctss/sample anim 100) [:fills 0]))))

    (t/testing "mismatched gradient types step instead of mixing"
      (let [rad-b (assoc-in lin-b [:fill-color-gradient :type] :radial)
            anim2 (ctsan/make-animation
                   {[:fills 0] [(kf 0 lin-a) (kf 100 rad-b)]})]
        (t/is (= lin-a (get (ctss/sample anim2 50) [:fills 0])))))

    (t/testing "mismatched stop counts step instead of mixing"
      (let [short-b (update-in lin-b [:fill-color-gradient :stops] (comp vec butlast))
            anim3 (ctsan/make-animation
                   {[:fills 0] [(kf 0 lin-a) (kf 100 short-b)]})]
        (t/is (= lin-a (get (ctss/sample anim3 50) [:fills 0])))))))

(t/deftest sample-stroke-interpolation
  (let [s0 {:stroke-color "#ff0000" :stroke-opacity 1 :stroke-width 2
            :stroke-style :solid :stroke-alignment :inner}
        s1 {:stroke-color "#0000ff" :stroke-opacity 0.5 :stroke-width 6
            :stroke-style :solid :stroke-alignment :inner}
        anim (ctsan/make-animation
              {[:strokes 0] [(kf 0 s0) (kf 100 s1)]})]
    (t/testing "color, opacity and width lerp at midpoint"
      (let [v (get (ctss/sample anim 50) [:strokes 0])]
        (t/is (= "#7f007f" (:stroke-color v)))
        (t/is (mth/close? 0.75 (:stroke-opacity v) 0.001))
        (t/is (mth/close? 4 (:stroke-width v) 0.001))
        (t/is (= :solid (:stroke-style v)))
        (t/is (= :inner (:stroke-alignment v)))))

    (t/testing "endpoints are exact"
      (t/is (= s0 (get (ctss/sample anim 0) [:strokes 0])))
      (t/is (= s1 (get (ctss/sample anim 100) [:strokes 0]))))

    (t/testing "missing opacity on either side keeps the other's value"
      (let [anim2 (ctsan/make-animation
                   {[:strokes 0] [(kf 0 (dissoc s0 :stroke-opacity))
                                  (kf 100 s1)]})
            v (get (ctss/sample anim2 50) [:strokes 0])]
        (t/is (nil? (:stroke-opacity v)))))))
