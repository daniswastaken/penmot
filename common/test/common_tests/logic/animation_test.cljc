;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.logic.animation-test
  (:require
   [app.common.logic.animation :as cla]
   [app.common.math :as mth]
   [app.common.types.shape.animation :as ctsan]
   [app.common.types.shape.animation.sample :as ctss]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(defn- rect
  [id x y & {:keys [width height opacity rotation]}]
  {:id id :type :rect :name "r"
   :x x :y y
   :width (or width 100) :height (or height 100)
   :opacity (or opacity 1)
   :rotation (or rotation 0)})

(t/deftest smart-animate-moved-shape
  (let [before (rect (uuid/next) 0 0)
        after  (assoc before :x 100)
        anim   (cla/smart-animate before after)]
    (t/testing "produces exactly the changed tracks"
      (t/is (= [[:x]] (ctsan/property-paths anim))))

    (t/testing "keyframes interpolate between the states"
      (t/is (= 0 (get (ctss/sample anim 0) [:x])))
      (t/is (mth/close? 100 (get (ctss/sample anim 300) [:x]) 0.001)))))

(t/deftest smart-animate-unchanged-shape
  (let [before (rect (uuid/next) 0 0)
        after  (assoc before :name "other name")]
    (t/testing "non-animatable change yields no tracks"
      (t/is (false? (cla/smart-animate? before after)))
      (t/is (ctsan/animation-empty? (cla/smart-animate before after))))))

(t/deftest smart-animate-multiple-props
  (let [id (uuid/next)
        before (rect id 0 0 :opacity 1 :rotation 0)
        after  (rect id 50 25 :opacity 0.5 :rotation 90)
        anim   (cla/smart-animate before after)]
    (t/testing "every changed animatable prop gets a track"
      (t/is (= #{[:x] [:y] [:opacity] [:rotation]}
               (set (ctsan/property-paths anim))))))

  (let [id (uuid/next)
        before (rect id 0 0)
        after  (rect id 0 0)]
    (t/testing "no changes, no tracks"
      (t/is (false? (cla/smart-animate? before after))))))

(t/deftest smart-animate-missing-prop-skipped
  (let [id (uuid/next)
        before (rect id 0 0)
        after  (-> before
                   (assoc :rotation 45)
                   (dissoc :opacity))]
    (t/testing "nil on either side -> property skipped, others kept"
      (let [anim (cla/smart-animate before after)]
        (t/is (= #{[:rotation]} (set (ctsan/property-paths anim)))))))

  (let [id (uuid/next)
        before (-> (rect id 0 0) (dissoc :opacity))
        after  (assoc before :rotation 45)]
    (t/testing "missing on the before side too"
      (let [anim (cla/smart-animate before after)]
        (t/is (= #{[:rotation]} (set (ctsan/property-paths anim))))))))

(t/deftest smart-animate-radius
  (let [id (uuid/next)
        before (assoc (rect id 0 0) :r1 0 :r2 0 :r3 0 :r4 0)
        after  (assoc before :r1 16 :r2 16 :r3 16 :r4 16)]
    (t/testing "radius tracks animate"
      (let [anim (cla/smart-animate before after)]
        (t/is (= #{[:r1] [:r2] [:r3] [:r4]}
                 (set (ctsan/property-paths anim))))
        (t/is (mth/close? 8 (get (ctss/sample-at-percent anim 0.5) [:r1]) 0.001))))))

(t/deftest smart-animate-tree
  (let [frame-id (uuid/next)
        child-a  (uuid/next)
        child-b  (uuid/next)
        objects-before {frame-id {:id frame-id :type :frame :x 0 :y 0
                                  :width 200 :height 200 :opacity 1 :rotation 0
                                  :shapes [child-a child-b]}
                        child-a (rect child-a 0 0)
                        child-b (rect child-b 100 0)}
        objects-after  {frame-id (assoc (get objects-before frame-id) :x 50)
                        child-a (rect child-a 10 10)
                        child-b (rect child-b 100 0)}
        anims (cla/smart-animate-tree objects-before objects-after frame-id)]
    (t/testing "only moved shapes get animations"
      (t/is (contains? anims frame-id))
      (t/is (contains? anims child-a))
      (t/is (not (contains? anims child-b))))

    (t/testing "frame animates x"
      (let [anim (get anims frame-id)]
        (t/is (= [[:x]] (ctsan/property-paths anim)))
        (t/is (mth/close? 25 (get (ctss/sample (get anims frame-id) 150) [:x]) 0.001))))

    (t/testing "custom duration is respected"
      (let [anims (cla/smart-animate-tree objects-before objects-after frame-id 500)]
        (t/is (= 500 (ctsan/duration (get anims frame-id))))))))

(t/deftest smart-animate-structured-values
  (let [id (uuid/next)
        base (assoc (rect id 0 0)
                    :fills [{:fill-color "#ff0000" :fill-opacity 1}]
                    :strokes [{:stroke-color "#000000" :stroke-width 1}]
                    :shadow [{:style :drop-shadow :offset-x 0 :offset-y 4 :blur 8 :spread 0
                              :hidden false :color {:color "#000000" :opacity 0.25}}]
                    :blur {:type :layer-blur :value 0 :hidden false})]

    (t/testing "changed fill produces a fills track with plain fill maps"
      (let [after (assoc base :fills [{:fill-color "#0000ff" :fill-opacity 0.5}])
            anim (cla/smart-animate base after)]
        (t/is (= [[:fills 0]] (ctsan/property-paths anim)))
        (t/is (= {:fill-color "#ff0000" :fill-opacity 1}
                 (:value (ctsan/first-keyframe anim [:fills 0]))))))

    (t/testing "changed stroke produces a strokes track"
      (let [after (assoc base :strokes [{:stroke-color "#ffffff" :stroke-width 4}])
            anim (cla/smart-animate base after)]
        (t/is (contains? (set (ctsan/property-paths anim)) [:strokes 0]))))

    (t/testing "changed shadow produces a shadow track"
      (let [after (assoc base :shadow [{:style :drop-shadow :offset-x 10 :offset-y 0 :blur 8 :spread 0
                                         :hidden false :color {:color "#000000" :opacity 0.25}}])
            anim (cla/smart-animate base after)]
        (t/is (contains? (set (ctsan/property-paths anim)) [:shadow 0]))))

    (t/testing "changed blur produces a blur track"
      (let [after (assoc base :blur {:type :layer-blur :value 12 :hidden false})
            anim (cla/smart-animate base after)]
        (t/is (contains? (set (ctsan/property-paths anim)) [:blur]))))

    (t/testing "fill present only after: no track invented"
      (let [after (assoc (dissoc base :fills) :fills [{:fill-color "#00ff00"}])
            before (dissoc base :fills)
            anim (cla/smart-animate before after)]
        (t/is (not (contains? (set (ctsan/property-paths anim)) [:fills 0])))))

    (t/testing "unchanged paint values produce no tracks"
      (let [anim (cla/smart-animate base (assoc base :name "renamed"))]
        (t/is (ctsan/animation-empty? anim))))))
