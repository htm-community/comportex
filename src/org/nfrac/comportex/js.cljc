(ns comportex.js
  (:require
    [org.nfrac.comportex.core :as core]
    [org.nfrac.comportex.encoders :as encoders]
    [org.nfrac.comportex.protocols :as protocols]
  ))

;; Public API

;; TODO: use converters from js->clj and clj->js

;; core

(defn ^:export sensory-region
  [spec]
  (core/sensory-region spec))

(defn ^:export sensorimotor-region
  [spec]
  (core/sensorimotor-region spec))

(defn ^:export sense-node
  [topo sensory motor]
  (core/sense-node topo sensory motor))

(defn ^:export region-network
  [ff-deps region-builders region-specs main-sensors motor-sensors]
  (core/region-network ff-deps region-builders region-specs main-sensors motor-sensors))

(defn ^:export regions-in-series
  [n build-region specs sensors]
  (core/regions-in-series n build-region specs sensors))

(defn ^:export column-state-freqs
  [rgn]
  (core/column-state-freqs rgn))

(defn ^:export predicted-bit-votes
  [rgn]
  (core/predicted-bit-votes rgn))


(defn ^:export predictions
  [htm sense-id n-predictions]
  (core/predictions htm sense-id n-predictions))

(defn ^:export cell-excitation-breakdowns
  [htm prior-htm rgn-id lyr-id cell-ids]
  (core/cell-excitation-breakdowns htm prior-htm rgn-id lyr-id cell-ids))

(defn ^:export update-excitation-breakdown
  [breakdown]
  (core/update-excitation-breakdown breakdown))

;; protocols
;; mostly just protocols (ie. interfaces)

(defn ^:export htm-step
  [htm inval]
  (protocols/htm-step htm inval))


;; encoders

(defn ^:export vec-selector
  [& selectors]
  (encoders/vec-selector (js->cljselectors)))

(defn ^:export prediction-stats
  [x-bits bit-votes total-votes]
  (encoders/prediction-stats x-bits bit-votes total-votes))

(defn ^:export decode-by-brute-force
  [e try-values bit-votes]
  (encoders/decode-by-brute-force e try-values bit-votes))

(defn ^:export encat
  [encoders]
  (encoders/encat js->clj(encoders)))

(defn ^:export ensplat
  [encoders]
  (encoders/ensplat js->clj(encoders)))

(defn ^:export linear-encoder
  [dimensions n-active lower upper]
  (encoders/linear-encoder dimensions n-active [lower upper]))

(defn ^:export category-encoder
  [dimensions values]
  (encoders/category-encoder dimensions values))

(defn ^:export no-encoder
  [dimensions]
  (encoders/no-encoder js->clj(dimensions)))

(defn ^:export unique-sdr
  [x n-bits n-active]
  (encoders/unique-sdr x n-bits n-active)))

(defn ^:export unique-encoder
  [dimensions n-active]
  (encoders/unique-encoder dimensions n-active))

(defn ^:export linear-2d-encoder
  [dimensions n-active x-max y-max]
  (encoders/linear-2d-encoder dimensions n-active [x-max y-max]))

(defn ^:export coordinate-encoder
  [dimensions n-active scale-factors radii]
  (encoders/coordinate-encoder dimensions n-active scale-factors radii))

(defn ^:export sampling-linear-encoder
  [dimensions n-active lower upper radius]
  (encoders/sampling-linear-encoder dimensions n-active [lower upper] radius))

(defn ^:export sensor-cat
  [& sensors]
  (encoders/sensor-cat js->clj(sensors)))



