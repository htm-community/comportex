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

(def block-sensor
  [:value
   (enc/category-encoder [bit-width] tokens)])

(def random-sensor
  [:value
   (enc/unique-encoder [bit-width] bits-per-char)])

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (n-region-model n spec block-sensor))
  ([n spec sensor]
   (core/regions-in-series n core/sensory-region
                           (list* spec (repeat (merge spec higher-level-spec-diff)))
                           {:input sensor})))
