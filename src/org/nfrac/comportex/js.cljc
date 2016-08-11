(ns comportex.js
  (:require
    [org.nfrac.comportex.core :as core]
    [org.nfrac.comportex.encoders :as encoders]
    [org.nfrac.comportex.protocols :as protocols]
  ))

;; Public API

;; Converters from js->clj and clj->js

;; See protocols:
;; PHTM, PRegion, PFeedForward, PFeedBack, PFeedForwardMotor, PLayerOfCells
;; PSynapseGraph, PSegments, PSense, PSelector, PEncoder, PRestartable, PInterruptable
;; PTemporal, PParameterised, PTopological, PTopology

;; Convert JavaScript object or array into ClojureScript data structure. 
;; We can do this by using js->clj function that:
;; "Recursively transforms JavaScript arrays into ClojureScript vectors, 
;; and JavaScript objects into ClojureScript maps. With
;; option ‘:keywordize-keys true’ will convert object fields from
;; strings to keywords.

;;  `spec` is the parameter specification map. 

(defn js->spec
  [spec]
  (js->clj spec :keywordize-keys true))

(defn js->htm
  [htm]
  (js->clj htm))

(defn js->motor
  [motor]
  (js->clj motor))

(defn js->sensory
  [sensory]
  (js->clj sensory))

(defn js->topo
  [topo]
  (js->clj topo))

(defn js->region
  [region]
  (js->clj region))


;; core

(defn ^:export sensory-region
  [spec]
  (core/sensory-region (js->spec spec)))

(defn ^:export sensorimotor-region
  [spec]
  (core/sensorimotor-region (js->spec spec)))

(defn ^:export sense-node
  [topo sensory motor]
  (core/sense-node (js->clj [topo sensory motor])))

(defn ^:export region-network
  [ff-deps region-builders region-specs main-sensors motor-sensors]
  (core/region-network ff-deps region-builders region-specs main-sensors motor-sensors))

(defn ^:export regions-in-series
  [n build-region specs sensors]
  (core/regions-in-series (js->clj [n build-region specs sensors])))

(defn ^:export column-state-freqs
  [rgn]
  (core/column-state-freqs (js->clj rgn)))

(defn ^:export predicted-bit-votes
  [rgn]
  (core/predicted-bit-votes (js->clj rgn)))


(defn ^:export predictions
  [htm sense-id n-predictions]
  (core/predictions (js->clj [htm sense-id n-predictions])))

(defn ^:export cell-excitation-breakdowns
  [htm prior-htm rgn-id lyr-id cell-ids]
  (core/cell-excitation-breakdowns (js->clj [htm prior-htm rgn-id lyr-id cell-ids])))

(defn ^:export update-excitation-breakdown
  [breakdown]
  (core/update-excitation-breakdown (js->clj breakdown)))

;; protocols
;; mostly just protocols (ie. interfaces)

(defn ^:export htm-step
  [htm inval]
  (protocols/htm-step (js->clj [htm inval])))


;; encoders

(defn ^:export vec-selector
  [& selectors]
  (encoders/vec-selector (js->clj selectors)))

(defn ^:export prediction-stats
  [x-bits bit-votes total-votes]
  (encoders/prediction-stats (js->clj [x-bits bit-votes total-votes])))

(defn ^:export decode-by-brute-force
  [e try-values bit-votes]
  (encoders/decode-by-brute-force (js->clj [e try-values bit-votes])))

(defn ^:export encat
  [encoders]
  (encoders/encat (js->clj encoders)))

(defn ^:export ensplat
  [encoders]
  (encoders/ensplat (js->clj encoders)))

(defn ^:export linear-encoder
  [dimensions n-active lower upper]
  (encoders/linear-encoder (js->clj [dimensions n-active [lower upper] ])))

(defn ^:export category-encoder
  [dimensions values]
  (encoders/category-encoder (js->clj [dimensions values])))

(defn ^:export no-encoder
  [dimensions]
  (encoders/no-encoder (js->clj dimensions)))

(defn ^:export unique-sdr
  [x n-bits n-active]
  (encoders/unique-sdr (js->clj [x n-bits n-active])))

(defn ^:export unique-encoder
  [dimensions n-active]
  (encoders/unique-encoder (js->clj [dimensions n-active])))

(defn ^:export linear-2d-encoder
  [dimensions n-active x-max y-max]
  (encoders/linear-2d-encoder (js->clj [dimensions n-active [x-max y-max]])))

(defn ^:export coordinate-encoder
  [dimensions n-active scale-factors radii]
  (encoders/coordinate-encoder (js->clj [dimensions n-active scale-factors radii])))

(defn ^:export sampling-linear-encoder
  [dimensions n-active lower upper radius]
  (encoders/sampling-linear-encoder (js->clj [dimensions n-active [lower upper] radius])))

(defn ^:export sensor-cat
  [& sensors]
  (encoders/sensor-cat (js->clj sensors)))



