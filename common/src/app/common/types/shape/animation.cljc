;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL
;;
;; Per-property keyframe animation data model for shapes.
;;
;; An `animation` is an optional shape attribute that describes how the
;; shape's properties change over time:
;;
;;   {:tracks {<property-path> [keyframe ...]}
;;    :playback {:loop :none|:loop|:ping-pong
;;               :alternate false}}
;;
;; A `keyframe` is:
;;
;;   {:t <safe-int ms>
;;    :value <any>                  ;; a value valid for the property path
;;    :easing <keyword>             ;; easing INTO this keyframe (from previous)
;;    :easing-params {...}          ;; optional: bezier points / spring params
;;    :hold <boolean>}              ;; optional: hold value until next keyframe
;;
;; A `property-path` is a vector of keywords/indices addressing a value
;; inside the shape, e.g. [:opacity] or [:fills 0 :color].

(ns app.common.types.shape.animation
  (:require
   [app.common.schema :as sm]
   [app.common.schema.generators :as sg]
   [app.common.types.fills :as ctf]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCHEMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def easing-types
  "Easing used to interpolate into a keyframe. `:hold` keeps the previous
   value until the keyframe instant (a step function)."
  #{:linear
    :ease
    :ease-in
    :ease-out
    :ease-in-out
    :bezier
    :spring
    :hold})

(def playback-types
  #{:none :loop :ping-pong})

(def schema:bezier-params
  [:map {:title "BezierEasingParams"}
   [:x1 ::sm/safe-number]
   [:y1 ::sm/safe-number]
   [:x2 ::sm/safe-number]
   [:y2 ::sm/safe-number]])

(def schema:spring-params
  [:map {:title "SpringEasingParams"}
   [:stiffness ::sm/safe-number]
   [:damping ::sm/safe-number]
   [:mass {:optional true} ::sm/safe-number]])

(def spring-defaults
  {:stiffness 170
   :damping 26
   :mass 1.0})

(def bezier-defaults
  {:x1 0.42
   :y1 0.0
   :x2 0.58
   :y2 1.0})

(def schema:scalar-value
  "Values a keyframe can hold. Phase 0 scope: the scalar properties
   (opacity, x/y, width/height, rotation, radius, color hex strings,
   grow-type keywords). Structured values (fills, points) get dedicated
   value schemas with their own codecs in later phases.

   `:string` is tried before numbers on decode: the JSON transformer
   coerces numeric-looking strings into numbers otherwise, which breaks
   round-trips when a value genuinely is a string."
  [:or {:title "ScalarValue"}
   :string
   ::sm/safe-number
   :boolean])

(def schema:fill-value
  "A solid-color fill as a keyframe value (fills cross-fade track).
   Reuses the fill schema — JSON codecs come with it — restricted to
   solid colors because gradient/image interpolation needs its own
   value schemas."
  [:and {:title "FillValue"}
   ctf/schema:fill-attrs
   [:fn (fn [v]
          (and (map? v)
               (contains? v :fill-color)
               (not (or (contains? v :fill-image)
                        (contains? v :fill-color-gradient)))))]])

(def schema:gradient-fill-value
  "A gradient fill as a keyframe value (gradient interpolation
   track). Same shape as a solid fill value but carrying
   :fill-color-gradient."
  [:and {:title "GradientFillValue"}
   ctf/schema:fill-attrs
   [:fn (fn [v]
          (and (map? v)
               (contains? v :fill-color-gradient)))]])

(def schema:easing-params
  [:or {:title "EasingParams"}
   schema:bezier-params
   schema:spring-params])

(def schema:keyframe-value
  "Everything a keyframe can hold: scalars, a solid-color fill, or a
   gradient fill. The fill branches come after `:string` so numeric
   strings stay strings."
  [:or {:title "KeyframeValue"}
   :string
   ::sm/safe-number
   :boolean
   schema:fill-value
   schema:gradient-fill-value])

(def schema:keyframe
  [:map {:title "Keyframe"}
   [:t ::sm/safe-int]
   [:value schema:keyframe-value]
   [:easing {:optional true} [::sm/one-of easing-types]]
   [:easing-params {:optional true} schema:easing-params]
   [:hold {:optional true} :boolean]])

(defn- valid-track?
  [kfs]
  (and (vector? kfs)
       (pos? (count kfs))
       (let [ts (mapv :t kfs)]
         (and (every? #(not (neg? %)) ts)
              (or (= 1 (count ts))
                  (apply < ts))))))

(defn- gen-track
  "Generator for tracks: random keyframes, clamped to non-negative
   times, deduplicated by instant, sorted. Always satisfies
   `valid-track?`, so the `:fn` constraint never needs to filter."
  []
  (sg/fmap (fn [kfs]
             (->> kfs
                  (map #(assoc % :t (max 0 (long (:t %)))))
                  (group-by :t)
                  (mapv (fn [[_ group]] (first group)))
                  (sort-by :t)
                  (vec)))
           (sg/vector (sg/generator schema:keyframe) 1 10)))

(def schema:track
  "A single property track: a non-empty vector of keyframes, sorted by
   strictly increasing `:t`."
  [:and {:title "Track"
         :gen/gen (gen-track)}
   [:vector {:gen/min 1 :gen/max 10} schema:keyframe]
   [:fn {:error/message "keyframes must be sorted by :t, strictly increasing"
         :gen/fmap (fn [kfs]
                     (->> kfs
                          (map #(assoc % :t (max 0 (long (:t %)))))
                          (group-by :t)
                          (mapv (fn [[_ group]] (first group)))
                          (sort-by :t)
                          (vec)))}
    valid-track?]])

(def schema:property-path
  "Path addressing a value inside a shape: keywords and/or integer
   indices, e.g. [:opacity], [:fills 0 :color]. JSON stores every element
   as a string, so decoding parses integers back from numeric strings
   and everything else becomes a keyword."
  [:vector {:gen/min 1 :gen/max 4
            :decode/json #(mapv (fn [el]
                                  (if (string? el)
                                    (if-let [n (parse-long el)]
                                      n
                                      (keyword el))
                                    el))
                                %)
            :encode/json #(mapv (fn [el]
                                 (if (keyword? el)
                                   (name el)
                                   el))
                                %)}
   [:or :keyword ::sm/safe-int]])

(def schema:playback
  [:map {:title "AnimationPlayback"}
   [:loop {:optional true} [::sm/one-of playback-types]]
   [:alternate {:optional true} :boolean]])

(def schema:track-entry
  "One track: the property path it animates plus its keyframes. Kept as
   a vector of entries (not a map keyed by path) so it survives JSON
   round-trips — JSON object keys must be strings, and property paths
   are vectors."
  [:map {:title "TrackEntry"}
   [:property schema:property-path]
   [:keyframes schema:track]])

(def schema:animation
  [:map {:title "Animation"}
   [:tracks {:gen/min 1 :gen/max 6}
    [:vector schema:track-entry]]
   [:playback {:optional true} schema:playback]])

(sm/register! ::animation schema:animation)
(sm/register! ::keyframe schema:keyframe)
(sm/register! ::track schema:track)

(def check-animation!
  (sm/check-fn schema:animation))

(def check-keyframe!
  (sm/check-fn schema:keyframe))

(def check-track!
  (sm/check-fn schema:track))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-easing :linear)

(defn make-keyframe
  "Build a keyframe with defaults applied. `:easing` describes the
   interpolation INTO this keyframe from the previous one."
  [{:keys [t value easing easing-params hold]}]
  (assert (sm/check-safe-int t) "keyframe `:t` must be a safe int (ms)")
  (assert (not (neg? t)) "keyframe `:t` must be >= 0")
  (assert (some? value) "keyframe `:value` is required")
  (cond-> {:t t :value value}
    (some? easing) (assoc :easing easing)
    (some? easing-params) (assoc :easing-params easing-params)
    (some? hold) (assoc :hold hold)))

(defn- find-entry-index
  "Index of the track entry for `path`, or nil."
  [animation path]
  (some (fn [[i entry]]
          (when (= path (:property entry))
            i))
        (map-indexed vector (:tracks animation))))

(defn- replace-entry
  [animation index entry]
  (assoc animation :tracks
         (into (subvec (vec (:tracks animation)) 0 index)
               (conj (subvec (vec (:tracks animation)) (inc index))
                     entry))))

(defn- remove-entry
  [animation index]
  (let [tracks (vec (:tracks animation))]
    (assoc animation :tracks
           (into (subvec tracks 0 index)
                 (subvec tracks (inc index))))))

(defn- upsert-track
  "Set the keyframes of the track for `path`, adding a new track entry
   when it does not exist."
  [animation path kfs]
  (if-let [index (find-entry-index animation path)]
    (replace-entry animation index {:property path :keyframes kfs})
    (update animation :tracks conj {:property path :keyframes kfs})))

(defn make-animation
  "Build an animation from a map of property-path -> keyframe seq.
   Tracks are sorted by `:t` on construction."
  [tracks & {:keys [loop alternate]}]
  (let [entries (into []
                      (map (fn [[path kfs]]
                             {:property path
                              :keyframes (vec (sort-by :t kfs))}))
                      tracks)]
    (cond-> {:tracks entries}
      loop (assoc-in [:playback :loop] loop)
      alternate (assoc-in [:playback :alternate] alternate))))

(defn duration
  "Total duration of the animation in ms: max keyframe `:t` across
   tracks, or 0 when there are no tracks."
  [animation]
  (max 0 (transduce (comp (map :keyframes)
                          (mapcat identity)
                          (map :t))
                    max
                    -1
                    (:tracks animation))))

(defn track
  "Get the sorted keyframes for a property path."
  [animation path]
  (when-let [index (find-entry-index animation path)]
    (-> (:tracks animation)
        (nth index)
        :keyframes)))

(defn add-keyframe
  "Add or replace a keyframe on a track. Replacement happens when a
   keyframe with the same `:t` already exists (a property can only hold
   one value at an instant)."
  [animation path keyframe]
  (assert (check-keyframe! keyframe))
  (let [existing (track animation path)
        kfs (if (and existing (some #(= (:t %) (:t keyframe)) existing))
              (mapv #(if (= (:t %) (:t keyframe)) keyframe %) existing)
              (conj (or existing []) keyframe))]
    (upsert-track animation path (vec (sort-by :t kfs)))))

(defn remove-keyframe
  "Remove the keyframe at time `t` on `path`. Removes the whole track
   when the last keyframe goes away (absence over nil, per data model
   conventions)."
  [animation path t]
  (let [kfs (seq (remove #(= (:t %) t) (track animation path)))]
    (if kfs
      (upsert-track animation path (vec (sort-by :t kfs)))
      (if-let [index (find-entry-index animation path)]
        (remove-entry animation index)
        animation))))

(defn move-keyframe
  "Move the keyframe at `from-t` to `to-t` on `path`. No-op when there
   is no keyframe at `from-t`. When a keyframe already sits at `to-t`
   it is replaced."
  [animation path from-t to-t]
  (let [kfs (track animation path)
        kf (some #(when (= (:t %) from-t) %) kfs)]
    (if (nil? kf)
      animation
      (-> animation
          (remove-keyframe path from-t)
          (add-keyframe path (assoc kf :t to-t))))))

(defn update-keyframe
  "Update the keyframe at time `t` on `path` with `update-fn`. No-op
   when there is no keyframe at `t`."
  [animation path t update-fn]
  (let [kfs (track animation path)]
    (if (nil? (some #(= (:t %) t) kfs))
      animation
      (reduce (fn [anim kf]
                (if (= (:t kf) t)
                  (add-keyframe anim path (update-fn kf))
                  anim))
              animation
              kfs))))

(defn animation-empty?
  "True when the animation has no tracks left (or is nil)."
  [animation]
  (or (nil? animation)
      (not (seq (:tracks animation)))))

(defn property-paths
  "All property paths that have tracks."
  [animation]
  (into [] (map :property) (:tracks animation)))

(defn normalize
  "Sort every track by time, drop empty tracks. Idempotent."
  [animation]
  (let [entries (into []
                      (comp (map (fn [{:keys [property keyframes]}]
                                   {:property property
                                    :keyframes (vec (sort-by :t keyframes))}))
                            (filter (fn [entry] (pos? (count (:keyframes entry))))))
                      (:tracks animation))]
    (if (seq entries)
      (assoc animation :tracks entries)
      {:tracks []})))

(defn first-keyframe
  "First keyframe of a track, or nil."
  [animation path]
  (first (track animation path)))

(defn last-keyframe
  "Last keyframe of a track, or nil."
  [animation path]
  (peek (track animation path)))

(defn keyframe-times
  "Sorted set of all keyframe instants across all tracks (the union
   timeline ruler marks)."
  [animation]
  (into (sorted-set)
        (comp (map :keyframes) (mapcat identity) (map :t))
        (:tracks animation)))

(defn clamp-time
  "Clamp a time sample into [0, duration]."
  [animation t]
  (max 0 (min t (duration animation))))

(defn playback-loop
  [animation]
  (get-in animation [:playback :loop] :none))

(defn alternate?
  [animation]
  (boolean (get-in animation [:playback :alternate])))

(defn playback-elapsed
  "Map a raw (possibly unbounded) elapsed time onto the animation's
   playback mode:

   - :none      holds at duration after one pass
   - :loop      wraps into [0, duration)
   - :ping-pong cycles duration*2: forward, then reversed, then repeats

   Returns a number in [0, duration]."
  [animation raw-elapsed]
  (let [duration (duration animation)]
    (if (or (not (pos? duration)) (neg? raw-elapsed))
      0
      (case (playback-loop animation)
        :none
        (min raw-elapsed duration)

        :loop
        (long (mod raw-elapsed duration))

        :ping-pong
        (let [cycle (long (mod raw-elapsed (* 2 duration)))]
          (if (> cycle duration)
            (- (* 2 duration) cycle)
            cycle))

        ;; unknown mode: clamp (never throw during playback)
        (min raw-elapsed duration)))))
