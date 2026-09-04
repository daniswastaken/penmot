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
   [app.main.store :as st]
   [app.main.ui.components.select :refer [select]]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.product.empty-state :refer [empty-state*]]
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
           (let [value (dm/get-prop shape prop-key)]
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
                                            (or (d/parse-double value) 0))}))))]

    [:div {:class (stl/css :track-row)}
     [:span {:class (stl/css :track-name)} (prop-label prop-key)]

     [:div {:class (stl/css :track-keyframes)}
      (for [kf kfs]
        [:div {:class (stl/css :keyframe)
               :key (str (:t kf))}
         [:span {:class (stl/css :keyframe-t)} (str (:t kf) "ms")]
         [:span {:class (stl/css :keyframe-value)} (pr-str (:value kf))]
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
                           :icon i/close}]])

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
           (let [prop (keyword value)
                 prop-value (dm/get-prop shape prop)]
             (when (some? prop-value)
               (st/emit! (dwa/add-animation-keyframe
                          {:page-id page-id
                           :shape-id (dm/get-prop shape :id)
                           :property [prop]
                           :value prop-value
                           :t 0}))))))

        remove-animation
        (mf/use-fn
         (mf/deps shape page-id)
         #(st/emit! (dwa/remove-animation
                     {:page-id page-id
                      :shape-id (dm/get-prop shape :id)})))]

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
                                       (mapv #(do {:value (name %)
                                                   :label (prop-label %)})
                                             animatable-props))
                        :on-change add-track}]]
           [:div {:class (stl/css :track-duration)}
            (tr "workspace.options.animation.duration" (str (ctsan/duration animation)))]]

          [:div {:class (stl/css :empty)}
           [:> empty-state* {:icon i/curve
                            :text (tr "workspace.options.animation.empty")}]])])]))
