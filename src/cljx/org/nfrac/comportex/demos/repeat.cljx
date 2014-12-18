(ns org.nfrac.comportex.demos.repeat
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def stimuli (mapv (comp keyword str) "abcdefghij"))
(def stimuli2 (mapv (comp keyword str) "klmnopqrstuvwxyz"))
(def signals [:start :again :pause :stop])
(def tokens (into signals stimuli stimuli2))

(def on-bits 20)
(def bit-width (* on-bits (count tokens)))

(def spec
  {:column-dimensions [1000]
   :depth 8
   :distal-punish? false})

(defn presentation
  [n stimulus-seq]
  (concat [:start]
          (apply concat
                 (interpose [:again] (repeat n stimulus-seq)))
          [:stop]
          [nil]))

(def block-encoder
  (enc/category-encoder bit-width tokens))

(defn repeatcat
  [n xs]
  (apply concat (repeat n xs)))

(defn world-seq
  []
  (concat (mapcat #(repeatcat 3 (presentation 1 [%])) stimuli)
          (mapcat #(repeatcat 3 (presentation 2 [%])) stimuli)
          (mapcat #(repeatcat 1 (presentation 2 %)) (partition 2 2 stimuli))
          (mapcat #(repeatcat 3 (presentation 1 [%])) stimuli2)
          (mapcat #(repeatcat 3 (presentation 2 [%])) stimuli2)
          (mapcat #(repeatcat 1 (presentation 2 %)) (partition 2 2 stimuli2))))

(defn n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region
                             (core/sensory-input block-encoder)
                             n spec)))
