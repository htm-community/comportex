(ns org.nfrac.comportex.demos.directional-steps-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def bit-width 300)
(def cat-bit-width 60)
(def numb-bit-width (- bit-width cat-bit-width))
(def numb-max 7)
(def numb-domain [0 numb-max])
(def on-bits 30)

(def spec
  {:column-dimensions [500]
   :ff-potential-radius 0.2
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 3
   :global-inhibition? false
   :activation-level 0.04
   :boost-active-every 10000
   :boost-overlap-every 10000
   ;; sequence memory:
   :depth 4
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

(def higher-level-spec-diff
  {:column-dimensions [500]
   :ff-max-segments 5})

(def initial-input-val [:up 0])

(defn input-transform
  [[dir i]]
  (let [new-i (-> (case dir
                    :up (inc i)
                    :down (dec i))
                  (min numb-max)
                  (max 0))
        new-dir (util/rand-nth [:up :down])]
    [new-dir new-i]))

(def encoder
  (enc/encat 2
             (enc/category-encoder cat-bit-width [:down :up])
             (enc/linear-encoder numb-bit-width on-bits numb-domain)))

(defn world-seq
  "Returns an infinite lazy seq of sensory input values."
  []
  (iterate input-transform initial-input-val))

(defn n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                             n
                             (list* spec (repeat (merge spec higher-level-spec-diff))))))
