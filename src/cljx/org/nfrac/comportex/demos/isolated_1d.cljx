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

(defn initial-input
  []
  (-> (util/remap (fn [xs]
                    {:seq xs :index nil})
                  patterns)
      (assoc ::current-pattern-index 0)))

(defn input-transform
  [input]
  (let [i (::current-pattern-index input)
        k (nth pattern-order i)
        m (input k)
        finished? (= (:index m) (dec (count (:seq m))))]
    (-> input
        (update-in [k]
                   (fn [m]
                     (cond
                      ;; reached end of sequence
                      finished?
                      (assoc m :index nil)
                      ;; starting sequence
                      (not (:index m))
                      (assoc m :index 0)
                      ;; in sequence
                      :else
                      (update-in m [:index] inc))))
        (update-in [::current-pattern-index]
                   (fn [i]
                     (if finished?
                       (mod (inc i) (count patterns))
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
  {:ncol 1000
   :potential-radius-frac 0.1
   :activation-level 0.02
   :global-inhibition false
   :stimulus-threshold 3
   :sp-perm-inc 0.05
   :sp-perm-dec 0.01
   :sp-perm-signal-inc 0.05
   :sp-perm-connected 0.20
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 8
   :max-segments 5
   :max-synapse-count 18
   :new-synapse-count 12
   :activation-threshold 9
   :min-threshold 7
   :connected-perm 0.20
   :initial-perm 0.16
   :permanence-inc 0.05
   :permanence-dec 0.01
   })

(defn ^:export input-gen
  []
  (core/input-generator (initial-input) input-transform encoder))

(defn ^:export n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/cla-region (input-gen) n spec)))
