(ns org.nfrac.comportex.demos.letters
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]))

(def bit-width 500)
(def n-on-bits 25)

(def params
  {:column-dimensions [1000]
   :depth 8
   :distal {:perm-init 0.21}
   :distal-vs-proximal-weight 0.2})


(def higher-level-params
  (util/deep-merge
   params
   {:column-dimensions [800]
    :proximal {:max-segments 5}}))

(defn clean-text
  [text]
  (-> (str/lower-case text)
      (str/replace #"\s+" " ")))

(def random-sensor
  [:value
   (enc/unique-encoder [bit-width] n-on-bits)])

(defn build
  ([]
   (build params))
  ([params]
   (cx/network {:layer-a (layer/layer-of-cells params)}
               {:input random-sensor})))
