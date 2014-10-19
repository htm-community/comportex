(ns org.nfrac.comportex.demos.isolated-2d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def input-size [50 50])
(def on-bits 64)
(def numb-domain [10 10])

(def patterns
  {:down-1 (mapv vector (repeat 1) (range 10))
   :down-right (into (mapv vector (repeat 1) (range 5))
                     (mapv vector (range 1 10 2) (repeat 5)))
   :diag-tl-br (mapv vector (range 10) (range 10))
   :rand-10 (vec (repeatedly 10 #(vector (util/rand 0 10)
                                         (util/rand 0 10))))
   })

(def pattern-order (keys patterns))

(def gap-length 1)

(defn initial-input
  []
  (-> (util/remap (fn [xs]
                    {:seq xs :index nil})
                  patterns)
      (assoc ::current-pattern-index 0
             ::gap-countdown nil)))

(defn input-transform
  [input]
  (let [k (nth pattern-order (::current-pattern-index input))
        m (input k)
        at-end? (= (:index m) (dec (count (:seq m))))
        in-gap? (::gap-countdown input)]
    (-> input
        (update-in [k]
                   (fn [m]
                     (cond
                      ;; reached end of sequence
                      at-end?
                      (assoc m :index nil)
                      ;; in gap, wait
                      in-gap?
                      m
                      ;; starting sequence
                      (not (:index m))
                      (assoc m :index 0)
                      ;; in sequence
                      :else
                      (update-in m [:index] inc))))
        (update-in [::current-pattern-index]
                   (fn [i]
                     (if at-end?
                       (util/rand-int 0 (count patterns))
                       i)))
        (update-in [::gap-countdown]
                   (fn [i]
                     (cond
                      ;; a pattern ended, start gap
                      at-end?
                      (dec gap-length)
                      ;; continue gap
                      (and in-gap? (pos? (dec i)))
                      (dec i)
                      ;; end of gap
                      in-gap?
                      nil
                      :else
                      i))))))

(defn current-value
  [m]
  (when (:index m)
    (get (:seq m) (:index m))))

(defn current-values
  [input]
  (map current-value
       (map input pattern-order)))

(def encoder
  (enc/pre-transform current-values
                     (enc/ensplat
                      (enc/linear-2d-encoder input-size on-bits numb-domain))))

(def spec
  {:column-dimensions [20 50]
   :ff-init-frac 0.3
   :ff-potential-radius 0.2
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 3
   :global-inhibition false
   :activation-level 0.02
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 8
   :max-segments 5
   :seg-max-synapse-count 18
   :seg-new-synapse-count 10
   :seg-stimulus-threshold 5
   :seg-learn-threshold 4
   :distal-perm-connected 0.20
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-init 0.16
   :inhibition-base-distance 2
   :inhibition-speed 0.25
   })

(defn ^:export input-gen
  []
  (core/sensory-input (initial-input) input-transform encoder))

(defn ^:export n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region (input-gen) n spec)))
