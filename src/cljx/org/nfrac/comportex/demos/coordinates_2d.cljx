(ns org.nfrac.comportex.demos.coordinates-2d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [abs]]
            #+clj [clojure.core.async :as async]
            #+cljs [cljs.core.async :as async]))

(def input-dim [30 30])
(def on-bits 30)
(def max-pos 45)
(def max-vel 5)
(def radius 15)

(def spec
  {:column-dimensions [20 50]
   :depth 8})

(def initial-input-val
  {:x 10 :y 20 :vx 0 :vy 0 :ax 1 :ay 1})

(defn clamp
  [x lim]
  (-> x
      (min lim)
      (max (- lim))))

(defn wrap
  [x lim]
  (-> x
      (+ lim)
      (mod (* 2 lim))
      (- lim)))

(defn input-transform
  [{:keys [x y vx vy ax ay]}]
  {:x (wrap (+ x vx) max-pos)
   :y (wrap (+ y vy) max-pos)
   :vx (clamp (+ vx ax) max-vel)
   :vy (clamp (+ vy ay) max-vel)
   :ax (if (< (abs y) (abs vy))
         (if (pos? ax) -1 1)
         ax)
   :ay (if (< (abs x) (abs vx))
         (if (pos? ay) -1 1)
         ay)})

(def encoder
  (enc/pre-transform (fn [{:keys [x y]}]
                       {:coord [x y]
                        :radii [radius radius]})
   (enc/coordinate-encoder input-dim on-bits)))

(defn world
  "Returns a channel of sensory input values."
  []
  (doto (async/chan)
    (async/onto-chan (iterate input-transform initial-input-val))))

(defn n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                             n spec)))
