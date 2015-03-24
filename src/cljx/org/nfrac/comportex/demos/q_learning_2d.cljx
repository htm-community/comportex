(ns org.nfrac.comportex.demos.q-learning-2d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [org.nfrac.comportex.demos.q-learning-1d :refer [q-learn]]
            #+clj [clojure.core.async :refer [<! >! go]]
            #+cljs [cljs.core.async :refer [<! >!]])
    #+cljs (:require-macros [cljs.core.async.macros :refer [go]]))

(def input-dim [40 40])
(def on-bits 160)
(def grid-w 10)
(def grid-h 10)
(def radius 1.0)
(def empty-reward -3)
(def hazard-reward -200)
(def finish-reward 200)
(def surface
  (->>
   (for [x (range grid-w)]
     (for [y (range grid-h)]
       (case [x y]
         [(dec grid-w) (dec grid-h)] finish-reward
         [(- (quot grid-w 2) 2) (quot grid-h 2)] hazard-reward
         [(quot grid-w 2) (quot grid-h 2)] hazard-reward
         [(quot grid-w 2) (- (quot grid-h 2) 1)] hazard-reward
         [(dec grid-w) 0] hazard-reward
         empty-reward)))
   (mapv vec)))

(def initial-input-val
  {:x 0
   :y 0
   :dy 0
   :dx 0
   :reward 0})

(def spec
  {:column-dimensions [40 40]
   :depth 4
   :distal-punish? true
   :duty-cycle-period 500
   :boost-active-duty-ratio 0.01
   :ff-potential-radius 0.15
   :ff-init-frac 0.5})

(def action-spec
  {:column-dimensions [4 100]
   :activation-level 0.05
   :ff-potential-radius 1
   :ff-init-frac 0.25
   :ff-perm-inc 0.05
   :ff-perm-dec 0.05
   :ff-perm-connected 0.10
   :ff-perm-init-lo 0.35
   :ff-perm-init-hi 0.45
   ;; chosen for exploration - fresh connections fully boosted > 1.0:
   :max-boost 3.0
   :global-inhibition? true
   :boost-active-every 1
   :duty-cycle-period 250
   :boost-active-duty-ratio 0.05
   :depth 1
   :q-alpha 0.1
   :q-discount 0.5
   ;; disable learning
   :freeze? true})

;; lookup on columns of :action region
(def column->signal
  (zipmap (range)
          (for [motion [:up :down :left :right]
                influence (repeat 100 1.0)]
            [motion influence])))

(defn select-action
  [model]
  (let [alyr (get-in model [:regions :action :layer-3])
        acols (p/active-columns alyr)
        signals (map column->signal acols)]
    (->> signals
         (reduce (fn [m [motion influence]]
                   (assoc! m motion (+ (get m motion 0) influence)))
                 (transient {}))
         (persistent!)
         (apply max-key val)
         (key))))

(def action->movement
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]})

(defn make-model
  []
  (let [encoder (enc/pre-transform (fn [{:keys [x y]}]
                                     {:coord [x y]
                                      :radii [radius radius]})
                                   (enc/coordinate-encoder input-dim on-bits))
        mencoder (enc/encat 2
                            (enc/pre-transform :dx (enc/linear-encoder 100 30 [-1 1]))
                            (enc/pre-transform :dy (enc/linear-encoder 100 30 [-1 1])))
        sensory-input (core/sensorimotor-input encoder encoder)
        motor-input (core/sensorimotor-input nil mencoder)]
    (core/region-network {:rgn-1 [:input :motor]
                          :action [:rgn-1]}
                         {:input sensory-input
                          :motor motor-input
                          }
                         core/sensory-region
                         {:rgn-1 (assoc spec :lateral-synapses? false)
                          :action action-spec})))

(defn feed-world-c-with-actions!
  [in-model-steps-c out-world-c model-atom]
  (go
   (loop [inval initial-input-val]
     (>! out-world-c inval)
     (when-let [model (<! in-model-steps-c)]
       ;; scale reward to be comparable to [0-1] permanences
       (let [reward (* 0.01 (:z inval))]
         (swap! model-atom q-learn reward))
       (if (>= (abs (:z inval)) 100)
         ;; terminal state, restart
         (recur initial-input-val)
         (let [act (select-action model)
               [dx dy] (action->movement act)
               next-x (-> (+ (:x inval) dx)
                          (min (dec grid-w))
                          (max 0))
               next-y (-> (+ (:y inval) dy)
                          (min (dec grid-h))
                          (max 0))
               next-z (get-in surface [next-x next-y])]
           (recur {:x next-x
                   :y next-y
                   :dx dx
                   :dy dy
                   :z next-z})))))))

(comment
  (require '[clojure.core.async :as async :refer [>!! <!!]])
  (require '[org.nfrac.comportex.protocols :as p])
  (def world-c (async/chan))
  (def model (atom (make-model)))
  (def steps-c (async/chan))
  
  (feed-world-c-with-actions! steps-c world-c model)

  (def inv (<!! world-c))
  (def model2 (swap! model p/htm-step inv))
  (>!! steps-c model2)

  inv
  (get-in @model [:regions :action :layer-3 :state :Q-val])
  (get-in @model [:regions :action :layer-3 :state :Q-info])
  (get-in @model [:regions :action :layer-3 :state :active-cols])
  
  )
