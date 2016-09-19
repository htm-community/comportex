(ns org.nfrac.comportex.demos.sensorimotor-1d
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.test.check.random :as random]))

(def bit-width 300)
(def motor-bit-width 100)
(def motor-n-on-bits 25)
(def world-size 10)
(def items [:a :b :c :d :e :f :g :h :i :j])
(def saccades [-1 0 1 2])

(def higher-level-params-diff
  {:column-dimensions [800]
   :proximal {:max-segments 5
              :new-synapse-count 12
              :learn-threshold 6}})

(def params
  {:column-dimensions [800]
   :depth 5
   :proximal {:perm-inc 0.10
              :perm-dec 0.01}
   :distal {:punish? false}})

(def higher-level-params
  (util/deep-merge params higher-level-params-diff))

(def fields
  (->>
   (for [k [:abcdefghij
            :baggagejade
            :baggagefeed
            :beachjadehigh
            :deafjigjag
            :hidebadface
            :hidefacebad]]
     [k (mapv (comp keyword str) (name k))])
   (into {})))

(defn initial-world
  [field seed]
  (-> {:field field
       :position (quot (count field) 2)
       :next-saccade 1}
      (vary-meta assoc ::rng (random/make-random seed))))

(defn world-transform
  [m]
  (let [n (count (:field m))
        dx (:next-saccade m)
        x (-> (+ (:position m) dx)
              (mod n))
        [rng rng*] (-> (::rng (meta m))
                       (random/split))
        sacc (util/rand-nth rng* saccades)]
    (-> (assoc m
               :position x
               :last-saccade dx
               :next-saccade sacc)
        (vary-meta assoc ::rng rng))))

(defn attach-current-value
  [m]
  (assoc m :value (get (:field m) (:position m))))

(defn input-seq
  "Returns an infinite lazy seq of sensory input values."
  ([]
   (input-seq (initial-world (first (vals fields)) 42)))
  ([world]
   (->> (iterate world-transform world)
        (map attach-current-value))))

(def block-sensor
  [:value
   (enc/category-encoder [bit-width] items)])

(def block-motor-sensor
  [:next-saccade
   (enc/linear-encoder [motor-bit-width] motor-n-on-bits
                       [(first saccades) (last saccades)])])

(defn build
  ([]
   (build params))
  ([params]
   (cx/network {:layer-a (layer/layer-of-cells params)
                  :layer-b (layer/layer-of-cells higher-level-params)}
               {:input block-sensor
                :motor block-motor-sensor}
               {:ff-deps {:layer-a [:input]
                          :layer-b [:layer-a]}
                :lat-deps {:layer-a [:motor]}})))
