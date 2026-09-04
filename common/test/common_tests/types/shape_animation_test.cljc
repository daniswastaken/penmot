;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns common-tests.types.shape-animation-test
  (:require
   [app.common.types.shape :as cts]
   [app.common.types.shape.animation :as ctsan]
   [clojure.test :as t]))

(defn- kf
  [t value & {:as opts}]
  (ctsan/make-keyframe (merge {:t t :value value} opts)))

(t/deftest make-keyframe-defaults
  (t/testing "minimal keyframe"
    (let [k (ctsan/make-keyframe {:t 0 :value 1.0})]
      (t/is (= 0 (:t k)))
      (t/is (= 1.0 (:value k)))
      (t/is (nil? (:easing k)))
      (t/is (nil? (:easing-params k)))
      (t/is (nil? (:hold k)))))

  (t/testing "full keyframe"
    (let [k (ctsan/make-keyframe {:t 100 :value 0.5 :easing :bezier
                                  :easing-params ctsan/bezier-defaults :hold true})]
      (t/is (= :bezier (:easing k)))
      (t/is (= ctsan/bezier-defaults (:easing-params k)))
      (t/is (true? (:hold k)))))

  (t/testing "negative time rejected"
    (t/is (thrown? #?(:clj AssertionError :cljs js/Error) (ctsan/make-keyframe {:t -1 :value 1}))))

  (t/testing "missing value rejected"
    (t/is (thrown? #?(:clj AssertionError :cljs js/Error) (ctsan/make-keyframe {:t 0})))))

(t/deftest make-animation-sorts-tracks
  (let [anim (ctsan/make-animation {[:opacity] [(kf 300 0)
                                                (kf 0 1)
                                                (kf 150 0.5)]})]
    (t/testing "track sorted by :t"
      (t/is (= [0 150 300] (mapv :t (ctsan/track anim [:opacity])))))))

(t/deftest duration-across-tracks
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 200 0)]
                                    [:x] [(kf 0 0) (kf 500 100)]})]
    (t/testing "duration is max keyframe t"
      (t/is (= 500 (ctsan/duration anim)))))

  (t/testing "empty animation has zero duration"
    (t/is (= 0 (ctsan/duration {:tracks []})))))

(t/deftest add-keyframe-replaces-same-t
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 100 0)]})]

    (t/testing "append new keyframe sorted"
      (let [anim' (ctsan/add-keyframe anim [:opacity] (kf 50 0.5))]
        (t/is (= [0 50 100] (mapv :t (ctsan/track anim' [:opacity]))))))

    (t/testing "same-t keyframe replaces, not duplicates"
      (let [anim' (ctsan/add-keyframe anim [:opacity] (kf 100 0.7))]
        (t/is (= [0 100] (mapv :t (ctsan/track anim' [:opacity]))))
        (t/is (= 0.7 (:value (ctsan/last-keyframe anim' [:opacity]))))))

    (t/testing "new track created on demand"
      (let [anim' (ctsan/add-keyframe anim [:fills 0 :opacity] (kf 0 1))]
        (t/is (= [0] (mapv :t (ctsan/track anim' [:fills 0 :opacity]))))))))

(t/deftest remove-keyframe-drops-empty-track
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1)]})]
    (t/testing "last keyframe removal removes the track"
      (let [anim' (ctsan/remove-keyframe anim [:opacity] 0)]
        (t/is (nil? (ctsan/track anim' [:opacity])))
        (t/is (ctsan/animation-empty? anim')))))

  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 100 0)]})]
    (t/testing "non-last removal keeps track"
      (let [anim' (ctsan/remove-keyframe anim [:opacity] 0)]
        (t/is (= [100] (mapv :t (ctsan/track anim' [:opacity]))))))

    (t/testing "removing absent time is a no-op"
      (t/is (= anim (ctsan/remove-keyframe anim [:opacity] 42))))))

(t/deftest move-keyframe
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 100 0) (kf 200 0.5)]})]

    (t/testing "move forward keeps sort"
      (let [anim' (ctsan/move-keyframe anim [:opacity] 100 150)]
        (t/is (= [0 150 200] (mapv :t (ctsan/track anim' [:opacity]))))
        (t/is (= 1 (:value (ctsan/first-keyframe anim' [:opacity]))))))

    (t/testing "move onto existing t replaces it"
      (let [anim' (ctsan/move-keyframe anim [:opacity] 100 200)]
        (t/is (= [0 200] (mapv :t (ctsan/track anim' [:opacity]))))
        (t/is (= 0 (:value (ctsan/last-keyframe anim' [:opacity]))))))

    (t/testing "moving absent keyframe is a no-op"
      (t/is (= anim (ctsan/move-keyframe anim [:opacity] 999 1000))))))

(t/deftest update-keyframe
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 100 0)]})]

    (t/testing "update value at t"
      (let [anim' (ctsan/update-keyframe anim [:opacity] 100 #(assoc % :value 0.25))]
        (t/is (= 0.25 (:value (ctsan/last-keyframe anim' [:opacity]))))
        (t/is (= [0 100] (mapv :t (ctsan/track anim' [:opacity]))))))

    (t/testing "update absent time is a no-op"
      (t/is (= anim (ctsan/update-keyframe anim [:opacity] 42 identity))))))

(t/deftest keyframe-times-union
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 300 0)]
                                    [:x] [(kf 0 0) (kf 150 10) (kf 300 20)]})]
    (t/testing "union of instants across tracks, sorted"
      (t/is (= [0 150 300] (vec (ctsan/keyframe-times anim)))))))

(t/deftest playback-accessors
  (t/testing "defaults"
    (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1)]})]
      (t/is (= :none (ctsan/playback-loop anim)))
      (t/is (false? (ctsan/alternate? anim)))))

  (t/testing "explicit loop"
    (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1)]}
                                     :loop :loop
                                     :alternate true)]
      (t/is (= :loop (ctsan/playback-loop anim)))
      (t/is (true? (ctsan/alternate? anim))))))

(t/deftest normalize-idempotent
  (let [anim {:tracks [{:property [:opacity] :keyframes [(kf 100 0) (kf 0 1)]}
                       {:property [:x] :keyframes []}]}
        norm (ctsan/normalize anim)]
    (t/testing "sorts tracks and drops empty ones"
      (t/is (= [0 100] (mapv :t (ctsan/track norm [:opacity]))))
      (t/is (nil? (ctsan/track norm [:x])))
      (t/testing "normalizing again changes nothing"
        (t/is (= norm (ctsan/normalize norm)))))))

(t/deftest clamp-time
  (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 400 0)]})]
    (t/testing "clamps into [0, duration]"
      (t/is (= 0 (ctsan/clamp-time anim -50)))
      (t/is (= 50 (ctsan/clamp-time anim 50)))
      (t/is (= 400 (ctsan/clamp-time anim 999))))))

(t/deftest schema-roundtrip
  (t/testing "a well-formed animation passes the schema check"
    (let [anim (ctsan/make-animation
                {[:opacity] [(kf 0 1) (kf 300 0 :easing :ease-in-out)]
                 [:x] [(kf 0 0 :easing :spring :easing-params ctsan/spring-defaults)
                       (kf 500 100 :easing :bezier :easing-params ctsan/bezier-defaults)]}
                :loop :loop)]
      (t/is (= anim (ctsan/check-animation! anim)))))

  (t/testing "unsorted track fails the schema check"
    (let [anim {:tracks [{:property [:opacity]
                          :keyframes [{:t 100 :value 0}
                                      {:t 0 :value 1}]}]}]
      (t/is (thrown? #?(:clj Exception :cljs js/Error) (ctsan/check-animation! anim)))))

  (t/testing "single-keyframe track is valid"
    (let [anim (ctsan/make-animation {[:opacity] [(kf 0 1)]})]
      (t/is (= anim (ctsan/check-animation! anim))))))

(t/deftest animation-attr-on-shape-schema
  (t/testing "shape with optional :animation attr validates"
    (let [shape (cts/setup-shape
                 {:type :rect
                  :animation (ctsan/make-animation {[:opacity] [(kf 0 1) (kf 200 0)]})})]
      (t/is (true? (cts/valid-shape? shape)))))

  (t/testing "shape without :animation still validates"
    (let [shape (cts/setup-shape {:type :rect})]
      (t/is (true? (cts/valid-shape? shape))))))




