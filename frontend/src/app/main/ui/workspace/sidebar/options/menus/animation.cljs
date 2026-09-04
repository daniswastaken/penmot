;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.workspace.sidebar.options.menus.animation
  "Prototype-tab section: per-shape keyframe animation editing.

  Phase 3 scope: add/remove keyframes on the animatable scalar
  properties, at a chosen instant, using the current shape value at
  that property. A full bottom-docked timeline with scrubbing and
  drag interactions lands in a later phase; this section is the data
  entry surface and works end-to-end through the changes pipeline."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.types.shape.animation :as ctsan]
    [app.main.data.workspace.animations :as dwa]
    [app.main.data.workspace.smart-animation :as dwsa]
    [app.main.refs :as refs]
    [app.main.store :as st]
    [app.main.ui.components.select :refer [select]]
    [app.main.ui.components.title-bar :refer [title-bar*]]
    [app.main.ui.ds.buttons.button :refer [button*]]
    [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
    [app.main.ui.ds.product.empty-state :refer [empty-state*]]
    [app.util.dom :as dom]
    [app.util.i18n :as i18n :refer [tr]]
    [rumext.v2 :as mf]))

(def ^:private animatable-props
  [:opacity :x :y :width :height :rotation :r1 :r2 :r3 :r4])

(def ^:private easing-options
  [:linear :ease :ease-in :ease-out :ease-in-out :spring :bezier :hold])

(defn- prop-label
  [prop]
  (tr (str "workspace.options.animation.prop." (name prop))))

(defn- easing-label
  [easing]
  (tr (str "workspace.options.animation.easing." (name easing))))

(defn- track-options
  "Addable tracks for a shape: the scalar props plus, when the shape
  has them, the structured paint entries (first element of
  fills/strokes/shadow, and the blur maps). Values encode the
  property path: plain scalars as the attr name, indexed entries as
  `<attr>-0`, whole maps as the attr name."
  [shape]
  (let [scalar-opts
        (mapv #(do {:value (name %) :label (prop-label %)})
              animatable-props)

        indexed-opts
        (into []
              (keep (fn [[attr entries]]
                      (when (seq entries)
                        {:value (dm/str (name attr) "-0")
                         :label (prop-label attr)})))
              [[:fills (:fills shape)]
               [:strokes (:strokes shape)]
               [:shadow (:shadow shape)]])

        map-opts
        (into []
              (keep (fn [[attr v]]
                      (when (some? v)
                        {:value (name attr) :label (prop-label attr)})))
              [[:blur (:blur shape)]
               [:background-blur (:background-blur shape)]])]

    (concat scalar-opts indexed-opts map-opts)))

(defn- parse-track-option
  "Decode an add-track select value into [property-path value]."
  [shape value]
  (let [[_ attr-part index-part] (re-matches #"(.+)-(\d+)" value)]
    (if (some? attr-part)
      (let [attr (keyword attr-part)
            index (d/parse-integer index-part)
            elements (vec (or (get shape attr) []))]
        (when (< index (count elements))
          [[attr index] (nth elements index)]))
      (let [attr (keyword value)]
        (when-let [v (dm/get-prop shape attr)]
          [[attr] v])))))

(mf/defc track-row*
  {::mf/private true}
  [{:keys [shape page-id animation property]}]
  (let [property (vec property)
        prop-key (first property)
        kfs      (ctsan/track animation property)

        add-keyframe
        (mf/use-fn
         (mf/deps shape page-id property)
         (fn [t]
           ;; for indexed tracks the current value is the element the
           ;; path addresses; scalars read the attr directly
           (let [value (if (= 2 (count property))
                        (let [[attr index] property
                              elements (vec (or (dm/get-prop shape attr) []))]
                          (when (< index (count elements))
                            (nth elements index)))
                        (dm/get-prop shape prop-key))]
             (st/emit! (dwa/add-animation-keyframe
                        {:page-id page-id
                         :shape-id (dm/get-prop shape :id)
                         :property property
                         :value value
                         :t t})))))

        remove-keyframe
        (mf/use-fn
         (mf/deps shape page-id property)
         (fn [t]
           (st/emit! (dwa/remove-animation-keyframe
                      {:page-id page-id
                       :shape-id (dm/get-prop shape :id)
                       :property property
                       :t t}))))

        set-easing
        (mf/use-fn
         (mf/deps shape page-id property)
         (fn [t easing]
           (st/emit! (dwa/update-animation-keyframe
                      {:page-id page-id
                       :shape-id (dm/get-prop shape :id)
                       :property property
                       :t t
                       ;; switching easing resets params to the new
                       ;; easing's defaults (fresh spring/bezier state)
                       :update-fn #(assoc (dissoc % :easing-params)
                                           :easing easing)}))))

        set-easing-param
        (mf/use-fn
         (mf/deps shape page-id property)
         (fn [t param value]
           (st/emit! (dwa/update-animation-keyframe
                      {:page-id page-id
                       :shape-id (dm/get-prop shape :id)
                       :property property
                       :t t
                       :update-fn #(assoc-in %
                                            [:easing-params param]
                                            (or (d/parse-double value) 0))}))))

        set-keyframe-value
        (mf/use-fn
         (mf/deps shape page-id property)
         (fn [t value]
           (st/emit! (dwa/update-animation-keyframe
                      {:page-id page-id
                       :shape-id (dm/get-prop shape :id)
                       :property property
                       :t t
                       ;; only numeric tracks are editable in place;
                       ;; parse failures keep the previous value
                       :update-fn #(assoc %
                                           :value (or (d/parse-double value)
                                                      (:value %)))}))))

        ;; drag-to-move: horizontal pointer drag over a keyframe chip
        ;; re-times it. The event commits once, on pointer release, so
        ;; the undo stack stays clean.
        drag-state* (mf/use-state nil)

        on-keyframe-drag-start
        (mf/use-fn
         (mf/deps property)
         (fn [event kf]
           ;; only drag when the chip itself is grabbed, so selects
           ;; and param inputs inside it keep working
           (when (= (dom/get-target event) (dom/get-current-target event))
             (dom/capture-pointer event)
             (reset! drag-state* {:t (:t kf)
                                  :start-x (:x (dom/get-client-position event))
                                  :moved false}))))

        on-keyframe-drag-move
        (mf/use-fn
         (fn [event]
           (when-let [drag @drag-state*]
             (let [delta-x (- (:x (dom/get-client-position event))
                              (:start-x drag))]
               ;; 2ms per pixel of horizontal drag
               (reset! drag-state*
                       (assoc drag
                              :current-t (max 0 (long (+ (:t drag) (* 2 delta-x))))
                              :moved true))))))

        on-keyframe-drag-end
        (mf/use-fn
         (mf/deps shape page-id property)
         (fn []
           (some-> @drag-state*
                   (as-> drag
                     (do (reset! drag-state* nil)
                         (when (and (:moved drag) (:current-t drag))
                           (st/emit! (dwa/move-animation-keyframe
                                      {:page-id page-id
                                       :shape-id (dm/get-prop shape :id)
                                       :property property
                                       :from-t (:t drag)
                                       :to-t (:current-t drag)}))))))))]

    [:div {:class (stl/css :track-row)}
     [:span {:class (stl/css :track-name)} (prop-label prop-key)]

     [:div {:class (stl/css :track-keyframes)}
      (for [kf kfs]
        (let [drag @drag-state*
              dragging-this? (and (some? drag) (= (:t drag) (:t kf)))
              display-t (if dragging-this?
                          (or (:current-t drag) (:t kf))
                          (:t kf))]
          [:div {:class (stl/css :keyframe (when dragging-this? :keyframe-dragging))
                 :key (str (:t kf))
                 :on-pointer-down #(on-keyframe-drag-start % kf)
                 :on-pointer-move on-keyframe-drag-move
                 :on-lost-pointer-capture on-keyframe-drag-end}
           [:span {:class (stl/css :keyframe-t)} (str display-t "ms")]
           (if (number? (:value kf))
             [:& numeric-input* {:class (stl/css :keyframe-value)
                                 :default-value (str (:value kf))
                                 :on-change #(set-keyframe-value (:t kf) %)}]
             [:span {:class (stl/css :keyframe-value)} (pr-str (:value kf))])
           [:& select {:default-value (name (or (:easing kf) :linear))
                       :options (mapv #(do {:value (name %)
                                            :label (easing-label %)})
                                      easing-options)
                       :on-change #(set-easing (:t kf) (keyword %))}]

           (when (= (:easing kf) :spring)
             [:div {:class (stl/css :keyframe-params)}
              (for [param [:stiffness :damping :mass]]
                [:& numeric-input*
                 {:key (name param)
                  :placeholder (name param)
                  :default-value (str (get-in kf [:easing-params param]
                                             (param ctsan/spring-defaults)))
                  :on-change #(set-easing-param (:t kf) param %)}])])

           (when (= (:easing kf) :bezier)
             [:div {:class (stl/css :keyframe-params)}
              (for [param [:x1 :y1 :x2 :y2]]
                [:& numeric-input*
                 {:key (name param)
                  :placeholder (name param)
                  :default-value (str (get-in kf [:easing-params param]
                                             (param ctsan/bezier-defaults)))
                  :on-change #(set-easing-param (:t kf) param %)}])])

           [:> icon-button* {:variant "ghost"
                             :aria-label (tr "workspace.options.animation.remove-keyframe")
                             :on-click #(remove-keyframe (:t kf))
                             :icon i/close}]]))

      [:div {:class (stl/css :keyframe-add)}
       [:& numeric-input* {:placeholder (tr "workspace.options.animation.time")
                           :default-value ""
                           :on-change (fn [value]
                                        (let [t (d/parse-integer value)]
                                          (when (and (int? t) (not (neg? t)))
                                            (add-keyframe t))))}]]]]))

(mf/defc animation-section*
  {::mf/private true}
  [{:keys [shape page-id]}]
  (let [animation (dm/get-prop shape :animation)
        show-content* (mf/use-state true)
        show-content? (deref show-content*)
        toggle-content (mf/use-fn #(swap! show-content* not))

        add-track
        (mf/use-fn
         (mf/deps shape page-id)
         (fn [value]
           (when-let [[property prop-value] (parse-track-option shape value)]
             (st/emit! (dwa/add-animation-keyframe
                        {:page-id page-id
                         :shape-id (dm/get-prop shape :id)
                         :property property
                         :value prop-value
                         :t 0})))))

        remove-animation
        (mf/use-fn
         (mf/deps shape page-id)
         #(st/emit! (dwa/remove-animation
                     {:page-id page-id
                      :shape-id (dm/get-prop shape :id)})))

        set-playback-mode
        (mf/use-fn
         (mf/deps shape page-id)
         (fn [value]
           (st/emit! (dwa/update-animation-playback
                      {:page-id page-id
                       :shape-id (dm/get-prop shape :id)
                       :playback {:loop (keyword value)}}))))

        workspace-local (mf/deref refs/workspace-local)
        snapshot (get-in workspace-local [:smart-animation-snapshot :snapshot])

        capture-snapshot
        (mf/use-fn
         #(st/emit! (dwsa/capture-smart-animation-snapshot)))

        clear-snapshot
        (mf/use-fn
         #(st/emit! (dwsa/clear-smart-animation-snapshot)))

        generate-animation
        (mf/use-fn
         #(st/emit! (dwsa/generate-smart-animation {})))]

    [:div {:class (stl/css :section)}
     [:div {:class (stl/css :title)}
      [:> title-bar* {:collapsable (some? animation)
                      :collapsed (not show-content?)
                      :on-collapsed toggle-content
                      :title (tr "workspace.options.animation.title")
                      :class (stl/css :title-bar)}
       (when animation
         [:> icon-button* {:variant "ghost"
                           :aria-label (tr "workspace.options.animation.remove-animation")
                           :on-click remove-animation
                           :icon i/close}])]]

     (when show-content?
       [:div {:class (stl/css :content)}
        [:div {:class (stl/css :smart-animation)}
         [:> button* {:variant "secondary"
                      :on-click (if (nil? snapshot)
                                  capture-snapshot
                                  clear-snapshot)}
          (tr (if (nil? snapshot)
                "workspace.options.animation.smart.capture"
                "workspace.options.animation.smart.clear"))]
         (when (some? snapshot)
           [:> button* {:variant "primary"
                        :on-click generate-animation}
            (tr "workspace.options.animation.smart.generate")])]

        (if animation
          [:*
           (for [property (ctsan/property-paths animation)]
             [:> track-row* {:key (pr-str property)
                             :shape shape
                             :page-id page-id
                             :animation animation
                             :property property}])
           [:div {:class (stl/css :add-track)}
            [:& select {:default-value ""
                        :options (into [{:value ""
                                         :label (tr "workspace.options.animation.add-track")}]
                                       (track-options shape))
                        :on-change add-track}]]
            [:div {:class (stl/css :track-duration)}
             (tr "workspace.options.animation.duration" (str (ctsan/duration animation)))]
            [:div {:class (stl/css :playback-mode)}
             [:& select {:default-value (name (ctsan/playback-loop animation))
                         :options [{:value "none" :label (tr "workspace.options.animation.playback.none")}
                                   {:value "loop" :label (tr "workspace.options.animation.playback.loop")}
                                   {:value "ping-pong" :label (tr "workspace.options.animation.playback.ping-pong")}]
                         :on-change set-playback-mode}]]]

           [:div {:class (stl/css :empty)}
            [:> empty-state* {:icon i/curve
                             :text (tr "workspace.options.animation.empty")}]])])]))
