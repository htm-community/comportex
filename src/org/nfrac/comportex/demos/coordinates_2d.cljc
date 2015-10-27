(ns org.nfrac.comportex.demos.coordinates-2d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [abs round]]))

(def input-dim [30 30])
(def n-on-bits 30)
(def max-pos 45)
(def max-vel 5)
(def radius 15)

(def spec
  {:column-dimensions [20 50]
   :depth 8})

(def higher-level-spec
  {:column-dimensions [20 20]
   :proximal {:max-segments 5}})

(def initial-input-val
  {:x -10 :y -20 :vx 1 :vy 1 :ax 1 :ay 1})

(defn clamp-vec
  [[vx vy] max-mag]
  (let [mag (Math/sqrt (+ (* vx vx) (* vy vy)))
        scale (/ max-mag mag)]
    (if (> mag max-mag)
      [(* vx scale) (* vy scale)]
      [vx vy])))

(defn wrap
  [x lim]
  (-> x
      (+ lim)
      (mod (* 2 lim))
      (- lim)))

(defn input-transform
  [{:keys [x y vx vy ax ay]}]
  (let [[vx2 vy2] (clamp-vec [(+ vx ax) (+ vy ay)] max-vel)
        x2 (+ x vx)
        y2 (+ y vy)]
    {:x (round (wrap x2 max-pos))
     :y (round (wrap y2 max-pos))
     :vx vx2
     :vy vy2
     ;; if crossing the x axis, reverse ax
     :ax (if (not= (pos? y) (pos? y2))
           (* ax -1)
           ax)
     ;; if crossing the y axis, reverse ay
     :ay (if (not= (pos? x) (pos? x2))
           (* ay -1)
           ay)}))

(defn input-seq
  "Returns an infinite lazy seq of sensory input values."
  []
  (iterate input-transform initial-input-val))

(def sensor
  [(enc/vec-selector :x :y)
   (enc/coordinate-encoder input-dim n-on-bits [1 1] [radius radius])])

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (core/regions-in-series n core/sensory-region
                           (list* spec (repeat higher-level-spec))
                           {:input sensor})))
