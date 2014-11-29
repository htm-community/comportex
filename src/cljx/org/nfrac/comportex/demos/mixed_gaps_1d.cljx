(ns org.nfrac.comportex.demos.mixed-gaps-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            #+clj [clojure.core.async :as async]
            #+cljs [cljs.core.async :as async]))

(def bit-width 400)
(def on-bits 25)

;; for block encoder
(def numb-max 15)
(def numb-domain [0 numb-max])

(def spec
  {:column-dimensions [1000]
   :ff-init-frac 0.2
   :ff-potential-radius 1.0
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 1
   :global-inhibition? false
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
   :inhibition-base-distance 0
   })

(def patterns
  {:run-0-5 [0 1 2 3 4 5]
   :rev-5-1 [5 4 3 2 1]
   :run-6-10 [6 7 8 9 10]
   :jump-6-12 [6 7 8 11 12]
   :twos [0 2 4 6 8 10 12 14]
   :saw-10-15 [10 12 11 13 12 14 13 15]})

(def pattern-order (keys patterns))

(def gap-range
  (->> (vals patterns) (map count) (reduce +) (long) (* 2)))

(defn initial-input
  []
  (assoc {} (first pattern-order) 0))

(defn input-transform
  [input]
  (reduce (fn [m [id values]]
            (if-let [index (m id)]
              ;; pattern is currently active
              (if (< index (dec (count (patterns id))))
                ;; continue
                (update-in m [id] inc)
                ;; finished
                (dissoc m id))
              ;; pattern is not currently active
              (if (zero? (util/rand-int gap-range))
                ;; start
                (assoc m id 0)
                m)))
          input
          patterns))

(defn current-values
  [input]
  (map (fn [[id index]]
         (get-in patterns [id index]))
       input))

(def block-encoder
  (enc/pre-transform current-values
                     (enc/ensplat
                      (enc/linear-encoder bit-width on-bits numb-domain))))

(defn world
  "Returns a channel of sensory input values."
  []
  (doto (async/chan)
    (async/onto-chan (iterate input-transform (initial-input)))))

(defn n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region
                             (core/sensory-input block-encoder)
                             n spec)))
