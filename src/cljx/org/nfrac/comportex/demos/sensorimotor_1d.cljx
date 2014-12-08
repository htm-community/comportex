(ns org.nfrac.comportex.demos.sensorimotor-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            #+clj [clojure.core.async :as async]
            #+cljs [cljs.core.async :as async]))

(def bit-width 300)
(def motor-bit-width 100)
(def motor-on-bits 25)
(def world-size 10)
(def items [:a :b :c :d :e :f :g :h :i :j])
(def max-saccade 2)
(def saccades (range (- max-saccade) (inc max-saccade)))

(def spec
  {:column-dimensions [1000]
   :depth 8
   :distal-punish? false
   :layer-3 {:ff-potential-radius 0.05
             :ff-init-frac 0.5}})

(defn make-random-field
  [n]
  (vec (repeatedly n #(util/rand-nth items))))

(defn make-simple-field
  [n]
  (vec (take n items)))

(defn initial-input
  []
  {:field (make-simple-field world-size)
   :position 0
   :next-saccade 1})

(defn input-transform
  [m]
  (let [dx (:next-saccade m)
        x (+ (:position m) dx)
        n (count (:field m))
        next-x (util/rand-int (max 0 (- x max-saccade))
                              (min (dec n) (+ x max-saccade)))]
    (assoc m
      :position x
      :last-saccade dx
      :next-saccade (- next-x x))))

(def block-sensory-input
  (let [e (enc/pre-transform #(get (:field %) (:position %))
                             (enc/category-encoder bit-width items))]
    (core/sensorimotor-input e e)))

(def block-motor-input
  (let [e (enc/pre-transform :next-saccade
                             (enc/linear-encoder motor-bit-width motor-on-bits
                                                 [(first saccades) (last saccades)]))]
    (core/sensorimotor-input nil e)))

(defn world
  "Returns a channel of sensory input values."
  []
  (doto (async/chan)
    (async/onto-chan (iterate input-transform (initial-input)))))

(defn n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensorimotor-region
                             block-sensory-input block-motor-input
                             n spec)))
