(ns org.nfrac.comportex.demos.letters
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]))

(def tokens (vec ".? abcdefghijklmnopqrstuvwxyz01234567890"))

(def bits-per-char 35)
(def bit-width (* bits-per-char (count tokens)))

(def spec
  {:column-dimensions [1000]
   :ff-init-frac 0.3
   :ff-potential-radius 1.0
   :ff-perm-inc 0.05
   :ff-perm-dec 0.005
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 1
   :global-inhibition? true
   :activation-level 0.02
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 5
   :max-segments 5
   :seg-max-synapse-count 18
   :seg-new-synapse-count 12
   :seg-stimulus-threshold 9
   :seg-learn-threshold 7
   :distal-perm-connected 0.20
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-init 0.16
   :distal-punish? false
   :distal-vs-proximal-weight 0.5
   })

(def higher-level-spec-diff
  {:column-dimensions [400]
   :ff-max-segments 5})

(defn clean-text
  [text]
  (-> (str/lower-case text)
      (str/replace #"[^A-Za-z0-9\.\?]+" " ")))

;; encoders expect a string in key :value of the input data item.

(def block-encoder
  (enc/pre-transform :value
                     (enc/category-encoder bit-width tokens)))

(def random-encoder
  (enc/pre-transform :value
                     (enc/unique-encoder [bit-width] bits-per-char)))

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (n-region-model n spec block-encoder))
  ([n spec encoder]
   (let [inp (core/sensory-input encoder)]
     (core/regions-in-series core/sensory-region inp n
                             (list* spec (repeat (merge spec higher-level-spec-diff)))))))
