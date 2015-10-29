(ns org.nfrac.comportex.demos.letters
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]))

(def bit-width 500)
(def n-on-bits 25)

(def spec
  {:column-dimensions [1000]
   :depth 8
   :distal {:perm-init 0.21}
   :distal-vs-proximal-weight 0.2
   })

(def higher-level-spec
  (util/deep-merge
   spec
   {:column-dimensions [800]
    :proximal {:max-segments 5}}))

(defn clean-text
  [text]
  (-> (str/lower-case text)
      (str/replace #"\s+" " ")))

(def random-sensor
  [:value
   (enc/unique-encoder [bit-width] n-on-bits)])

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (core/regions-in-series n core/sensory-region
                           (list* spec (repeat higher-level-spec))
                           {:input random-sensor})))
