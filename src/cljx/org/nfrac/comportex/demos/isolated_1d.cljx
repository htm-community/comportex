(ns org.nfrac.comportex.demos.isolated-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def bit-width 400)
(def on-bits 25)
(def numb-max 15)
(def numb-domain [0 numb-max])

(def patterns
  {:run-0-5 [0 1 2 3 4 5]
   :rev-5-1 [5 4 3 2 1]
   :run-6-10 [6 7 8 9 10]
   :jump-6-12 [6 7 8 11 12]
   :twos [0 2 4 6 8 10 12 14]
   :saw-10-15 [10 12 11 13 12 14 13 15]})

(def pattern-order (keys patterns))

(def gap-length 5)

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
                      (enc/linear-encoder bit-width on-bits numb-domain))))

(def spec
  {:column-dimensions [1000]
   :ff-potential-radius-frac 0.1
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
   :seg-new-synapse-count 12
   :seg-stimulus-threshold 9
   :seg-learn-threshold 7
   :distal-perm-connected 0.20
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-init 0.16
   })

(defn ^:export input-gen
  []
  (core/sensory-input (initial-input) input-transform encoder))

(defn ^:export n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region (input-gen) n spec)))
