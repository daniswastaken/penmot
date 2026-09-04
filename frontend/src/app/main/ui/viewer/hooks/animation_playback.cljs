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
  displayed; a `restart` counter in viewer state lets interaction
  triggers (:play-animation) replay from t=0."
  (:require
   [app.common.types.shape.animation :as ctsan]
   [app.main.store :as st]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private restarts-ref
  (l/derived (l/key :viewer-animation-restarts) st/state))

(def ^:private mode-priority {:ping-pong 2 :loop 1 :none 0})

(defn- aggregate-playback-mode
  "The most continuous playback mode among the frame's animations:
  ping-pong > loop > none."
  [objects]
  (transduce
   (comp (map :animation)
         (filter some?)
         (map ctsan/playback-loop))
   (completing (fn [acc mode]
                 (if (> (get mode-priority mode -1)
                        (get mode-priority acc -1))
                   mode
                   acc)))
   nil
   (vals objects)))

(defn use-animation-playback
  "React hook. Returns [elapsed-ms playing? has-animations? controls]
  where controls is {:play :pause :resume :stop :duration}. When any
  shape under `objects` has an animation, runs a rAF loop advancing
  elapsed-ms; elapsed wraps at the longest animation duration (loop
  playback). `frame-id` (optional) watches the interaction-triggered
  restart counter for that frame and replays from zero when it
  bumps."
  ([objects]
   (use-animation-playback objects nil))
  ([objects frame-id]
   (let [elapsed* (mf/use-state 0)
         started-at* (mf/use-state nil)
         playing?* (mf/use-state false)

         playing? (deref playing?*)
         elapsed (deref elapsed*)

         restart-count
         (when frame-id
           (get (mf/deref restarts-ref) frame-id 0))

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

         playback-mode (when has-animations? (aggregate-playback-mode objects))

         ;; a :none animation plays once and stops at the end
         one-shot? (= :none playback-mode)

         play (mf/use-fn
               (fn []
                 (reset! started-at* (js/performance.now))
                 (reset! elapsed* 0)
                 (reset! playing?* true)))

         pause (mf/use-fn
                (fn []
                  (reset! playing?* false)))

         resume (mf/use-fn
                 (mf/deps elapsed play-duration)
                 (fn []
                   (when (and (pos? (or play-duration 0))
                              (< (or elapsed 0) play-duration))
                     ;; continue from the frozen elapsed: shift the
                     ;; start timestamp so the clock resumes mid-loop
                     (reset! started-at* (- (js/performance.now) elapsed))
                     (reset! playing?* true))))

         stop (mf/use-fn
               (fn []
                 (reset! playing?* false)
                 (reset! elapsed* 0)))

         seek (mf/use-fn
              (mf/deps play-duration)
              (fn [t]
                (when play-duration
                  (let [t (max 0 (min t play-duration))]
                    (reset! elapsed* t)
                    ;; re-anchor the clock so playback continues from the
                    ;; seek point (raw t maps to elapsed t on the forward
                    ;; half of every mode)
                    (reset! started-at* (- (js/performance.now) t))))))

         controls {:play play
                   :pause pause
                   :resume resume
                   :stop stop
                   :seek seek
                   :duration play-duration}]

     ;; start automatically when the mounted objects contain animations
     (mf/use-effect
      (mf/deps has-animations?)
      (fn []
        (when has-animations?
          (play))
        stop))

     ;; interaction-triggered replay (:play-animation)
     (mf/use-effect
      (mf/deps restart-count)
      (fn []
        (when (and frame-id has-animations? (pos? restart-count))
          (play))))

     ;; the rAF loop: advance elapsed from the performance clock,
     ;; mapping through the aggregate playback mode (wrap / bounce /
     ;; one-shot)
     (mf/use-effect
      (mf/deps playing? play-duration playback-mode)
      (fn []
        (when (and playing? play-duration)
          (let [started-at (deref started-at*)
                mode-animation {:playback {:loop playback-mode}
                                 :tracks [{:property [:duration]
                                           :keyframes [{:t 0 :value 0}
                                                       {:t play-duration :value 0}]}]}
                tick (fn tick []
                       (let [raw     (- (js/performance.now) started-at)
                             elapsed (ctsan/playback-elapsed mode-animation raw)]
                         (reset! elapsed* elapsed)
                         (when-not (and one-shot? (>= raw play-duration))
                           (.requestAnimationFrame js/window tick))))
                raf-id (.requestAnimationFrame js/window tick)]
            #(.cancelAnimationFrame js/window raf-id)))))

     [elapsed playing? has-animations? controls])))
