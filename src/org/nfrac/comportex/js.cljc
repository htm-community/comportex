(ns comportex.js
  (:require
    [org.nfrac.comportex.core :as core]
  ))

;; Public API

;; TODO: use converters from js->clj and clj->js

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


;; more ... Working in the dark here!