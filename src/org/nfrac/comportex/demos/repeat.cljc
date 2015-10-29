(ns org.nfrac.comportex.demos.repeat
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def stimuli (mapv (comp keyword str) "abcd"))
(def stimuli2 (mapv (comp keyword str) "efghijklmnopqrstuvwxyz"))
(def signals [:start :again :pause :stop])
(def tokens (vec (concat signals stimuli stimuli2)))

(def n-on-bits 20)
(def bit-width (* n-on-bits (count tokens)))

(def spec
  {:column-dimensions [1000]
   :depth 5
   :distal {:punish? false}})

(def higher-level-spec
  (util/deep-merge
   spec
   {:column-dimensions [400]
    :proximal {:max-segments 5}}))

(defn presentation
  [n stimulus-seq]
  (concat [:start]
          (apply concat
                 (interpose [:again] (repeat n stimulus-seq)))
          [:stop]
          [nil]))

(def block-sensor
  [:value (enc/category-encoder [bit-width] tokens)])

(defn repeatcat
  [n xs]
  (apply concat (repeat n xs)))

(defn input-seq
  [stimuli reps]
  (->> (concat (mapcat #(repeatcat reps (presentation 1 [%])) stimuli)
               (mapcat #(repeatcat reps (presentation 2 [%])) stimuli)
               (mapcat #(repeatcat reps (presentation 2 %)) (partition 2 2 stimuli)))
       (map (partial hash-map :value))))

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (core/regions-in-series n core/sensory-region
                           (list* spec (repeat higher-level-spec))
                           {:input block-sensor})))
