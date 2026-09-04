;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.hooks.animation-preview
  "Workspace-local animation preview clock + objects overlay.

  The animation panel starts a preview; the SVG workspace viewport
  reads the elapsed time through `use-animation-preview` and applies
  `apply-animations` to the rendered objects. The clock is a plain
  module atom plus one rAF loop — no potok events per frame, no
  persisted state."
  (:require
   [app.common.files.helpers :as cfh]
   [app.common.types.shape.animation.sample :as ctss]
   [rumext.v2 :as mf]))

(defonce ^:private preview-state
  (atom {:playing false
         :frame-id nil
         :elapsed 0
         :duration 0
         :started-at nil
         :raf nil}))

(defn- stop-raf!
  []
  (when-let [raf-id (:raf @preview-state)]
    (.cancelAnimationFrame js/window raf-id))
  nil)

(defn- tick!
  []
  (let [{:keys [duration started-at]} @preview-state]
    (when (and duration (pos? duration))
      (let [raw (- (js/performance.now) started-at)
            elapsed (long (mod raw duration))]
        (swap! preview-state assoc :elapsed elapsed)
        (when (:playing @preview-state)
          (swap! preview-state assoc :raf (.requestAnimationFrame js/window tick!)))))))

(defn preview-frame-id
  "Frame whose subtree is being previewed, or nil."
  []
  (:frame-id @preview-state))

(defn preview-duration
  []
  (:duration @preview-state))

(defn preview-elapsed
  []
  (:elapsed @preview-state))

(defn playing?
  []
  (:playing @preview-state))

(defn start-preview!
  "Play the animation of `frame-id` subtree (duration-ms computed by
  the caller) on the workspace canvas."
  [frame-id duration-ms]
  (stop-raf!)
  (reset! preview-state {:playing true
                         :frame-id frame-id
                         :elapsed 0
                         :duration duration-ms
                         :started-at (js/performance.now)
                         :raf (.requestAnimationFrame js/window tick!)}))

(defn stop-preview!
  []
  (stop-raf!)
  (reset! preview-state {:playing false
                         :frame-id nil
                         :elapsed 0
                         :duration 0
                         :started-at nil
                         :raf nil}))

(defn- apply-sampled-path
  "Apply one sampled track value to a shape ([attr] scalar or whole-map
  values; [attr index] replaces the element; x/y are workspace-space
  already — no viewport shift here)."
  [shape path value]
  (if (= 2 (count path))
    (let [[attr index] path
          elements (vec (or (get shape attr) []))]
      (if (< index (count elements))
        (assoc shape attr
               (into (subvec elements 0 index)
                     (conj (when (> (count elements) (inc index))
                             (subvec elements (inc index)))
                           value)))
        shape))
    (assoc shape (first path) value)))

(defn apply-animations
  "Overlay sampled keyframe values onto workspace (design-space)
  objects for the preview clock's current elapsed time. Returns the
  objects unchanged when no preview is running."
  [objects]
  (let [{:keys [playing frame-id elapsed]} @preview-state]
    (if-not (and playing frame-id (get objects frame-id))
      objects
      (let [ids (cfh/get-children-ids-with-self objects frame-id)]
        (reduce
         (fn [objects id]
           (let [shape (get objects id)]
             (if-let [animation (:animation shape)]
               (let [sampled (ctss/sample animation elapsed)]
                 (if (empty? sampled)
                   objects
                   (assoc objects id
                          (reduce-kv apply-sampled-path shape sampled))))
               objects)))
         objects
         ids)))))

(defn use-animation-preview
  "React hook for the workspace viewport: subscribes to the preview
  clock (re-renders on every state change, i.e. every tick)."
  []
  (let [version* (mf/use-state 0)]
    (mf/use-effect
     (mf/deps [])
     (fn []
       (let [watch (add-watch preview-state ::preview-preview
                              (fn [_ _ _ _] (swap! version* inc)))]
         #(remove-watch preview-state watch))))
    (deref version*)))
