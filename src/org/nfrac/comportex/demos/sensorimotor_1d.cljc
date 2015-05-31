(ns org.nfrac.comportex.demos.sensorimotor-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def bit-width 300)
(def motor-bit-width 100)
(def motor-on-bits 25)
(def world-size 10)
(def items [:a :b :c :d :e :f :g :h :i :j])
(def saccades [-1 0 1 2])

(def higher-level-spec-diff
  {:column-dimensions [800]
   :ff-max-segments 5
   :ff-seg-new-synapse-count 12
   :ff-seg-learn-threshold 6
   })

(def spec
  {:column-dimensions [800]
   :depth 5
   :ff-perm-inc 0.10
   :ff-perm-dec 0.01
   :distal-punish? false
   :layer-3 higher-level-spec-diff})

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

(defn make-random-field
  [n]
  (vec (repeatedly n #(util/rand-nth items))))

(defn initial-input
  []
  {:field items
   :position (quot (count items) 2)
   :next-saccade 1})

(defn input-transform
  [m]
  (let [n (count (:field m))
        dx (:next-saccade m)
        x (-> (+ (:position m) dx)
              (mod n))
        sacc (util/rand-nth saccades)]
    (assoc m
      :position x
      :last-saccade dx
      :next-saccade sacc)))

(def block-sensory-input
  (let [e (enc/pre-transform #(get (:field %) (:position %))
                             (enc/category-encoder bit-width items))]
    (core/sensorimotor-input e e)))

(def block-motor-input
  (let [e (enc/pre-transform :next-saccade
                             (enc/linear-encoder motor-bit-width motor-on-bits
                                                 [(first saccades) (last saccades)]))]
    (core/sensorimotor-input nil e)))

(defn world-seq
  "Returns an infinite lazy seq of sensory input values."
  []
  (iterate input-transform (initial-input)))

(defn n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensorimotor-region
                             block-sensory-input block-motor-input
                             n
                             (list* spec (repeat (merge spec higher-level-spec-diff))))))
