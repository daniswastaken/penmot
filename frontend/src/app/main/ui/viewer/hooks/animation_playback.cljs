;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer.hooks.animation-playback
  "rAF-driven playback clock for per-shape keyframe animations.

  Owns the elapsed time in component-local state: one rAF loop per
  mounted viewport, no store round-trips per frame. Playback loops
  over the longest animation duration found in the frame while it is
  displayed."
  (:require
   [app.common.types.shape.animation :as ctsan]
   [rumext.v2 :as mf]))

(defn use-animation-playback
  "React hook. Returns [elapsed-ms playing? has-animations?]. When any
  shape under `objects` has an animation, runs a rAF loop advancing
  elapsed-ms; elapsed wraps at the longest animation duration (loop
  playback). `objects` is the page objects map."
  [objects]
  (let [elapsed* (mf/use-state 0)
        started-at* (mf/use-state nil)
        playing?* (mf/use-state false)

        playing? (deref playing?*)

        durations
        (into []
              (keep (fn [shape]
                      (when-let [animation (:animation shape)]
                        (let [duration (ctsan/duration animation)]
                          (when (pos? duration)
                            duration)))))
              (vals objects))

        has-animations? (pos? (count durations))

        play-duration (when has-animations? (reduce max 0 durations))

        play (mf/use-fn
              (fn []
                (reset! started-at* (js/performance.now))
                (reset! elapsed* 0)
                (reset! playing?* true)))

        stop (mf/use-fn
              (fn []
                (reset! playing?* false)
                (reset! elapsed* 0)))]

    ;; start automatically when the mounted objects contain animations
    (mf/use-effect
     (mf/deps has-animations?)
     (fn []
       (when has-animations?
         (play))
       stop))

    ;; the rAF loop: advance elapsed from the performance clock,
    ;; wrapping at the animation duration (loop)
    (mf/use-effect
     (mf/deps playing? play-duration)
     (fn []
       (when (and playing? play-duration)
         (let [started-at (deref started-at*)
               tick (fn tick []
                      (let [raw  (- (js/performance.now) started-at)
                            elapsed (long (mod raw play-duration))]
                        (reset! elapsed* elapsed)
                        (.requestAnimationFrame js/window tick)))
               raf-id (.requestAnimationFrame js/window tick)]
           #(.cancelAnimationFrame js/window raf-id)))))

    [(deref elapsed*) playing? has-animations?]))
