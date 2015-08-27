(ns org.nfrac.comportex.demos.q-learning-2d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [org.nfrac.comportex.demos.q-learning-1d :refer [q-learn]]
            #?(:clj [clojure.core.async :refer [<! >! go]]
               :cljs [cljs.core.async :refer [<! >!]]))
    #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(def input-dim [10 40])
(def grid-w 7)
(def grid-h 7)
(def n-on-bits 40)
(def coord-radius 5) ;; so 11x11 grid = 121 candidates (and we choose 40)
(def surface-coord-scale 5) ;; so neighbouring positions (x or y +/- 1) share ~50% bits
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
   :z 0})

(def spec
  {:column-dimensions [30 30]
   :depth 4
   :distal-punish? true
   :duty-cycle-period 300
   :boost-active-duty-ratio 0.01
   :ff-potential-radius 0.15
   :ff-init-frac 0.5})

(def action-spec
  {:column-dimensions [4 10]
   :activation-level 0.20
   :ff-potential-radius 1
   :ff-init-frac 0.5
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
   :q-alpha 0.75
   :q-discount 0.9
   ;; do not want temporal pooling here - actions are not static
   :temporal-pooling-max-exc 0.0
   ;; disable learning
   :freeze? true})

(def action->movement
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]})

(defn possible-actions
  [[x y]]
  (cond-> #{:up :down :left :right}
          (zero? x) (disj :left)
          (zero? y) (disj :up)
          (= x (dec grid-w)) (disj :right)
          (= y (dec grid-h)) (disj :down)))

;; lookup on columns of :action region
(def column->signal
  (zipmap (range)
          (for [motion [:up :down :left :right]
                influence (repeat 10 1.0)]
            [motion influence])))

(defn select-action
  [model curr-pos]
  (let [alyr (get-in model [:regions :action :layer-3])
        acols (p/active-columns alyr)
        signals (map column->signal acols)
        poss (possible-actions curr-pos)]
    (->> signals
         (reduce (fn [m [motion influence]]
                   (assoc! m motion (+ (get m motion 0) influence)))
                 (transient {}))
         (persistent!)
         (filter (comp poss key))
         (apply max-key val)
         (key))))

(defn make-model
  []
  (let [sensor [(enc/vec-selector :x :y)
                (enc/coordinate-encoder input-dim n-on-bits
                                        [surface-coord-scale surface-coord-scale]
                                        [coord-radius coord-radius])]
        dx-sensor [:dx (enc/linear-encoder [100] 30 [-1 1])]
        dy-sensor [:dy (enc/linear-encoder [100] 30 [-1 1])]
        msensor (enc/sensor-cat dx-sensor dy-sensor)]
    (core/region-network {:rgn-1 [:input :motor]
                          :action [:rgn-1]}
                         (constantly core/sensory-region)
                         {:rgn-1 (assoc spec :lateral-synapses? false)
                          :action action-spec}
                         {:input sensor}
                         {:input sensor
                          :motor msensor})))

(defn feed-world-c-with-actions!
  [in-model-steps-c out-world-c model-atom]
  (go
   (loop [inval (assoc initial-input-val
                       :Q-map {})
          prev-htm @model-atom]
     (>! out-world-c inval)
     (when-let [htm (<! in-model-steps-c)]
       ;; scale reward to be comparable to [0-1] permanences
       (let [reward (* 0.01 (:z inval))
             ;; do the Q learning on previous step
             upd-htm (swap! model-atom q-learn prev-htm reward)
             ;; maintain map of state+action -> approx Q values, for diagnostics
             info (get-in upd-htm [:regions :action :layer-3 :Q-info])
             newQ (-> (+ (:Q-prev info 0) (:adj info 0))
                      (max -1.0)
                      (min 1.0))
             Q-map (assoc (:Q-map inval)
                          (select-keys inval [:x :y :dx :dy])
                          newQ)]
         (if (>= (abs (:z inval)) 100)
           ;; terminal state, restart
           (recur (assoc initial-input-val
                         :Q-map Q-map)
                  upd-htm)
           (let [x (:x inval)
                 y (:y inval)
                 act (select-action upd-htm [x y])
                 [dx dy] (action->movement act)
                 next-x (-> (+ x dx)
                            (min (dec grid-w))
                            (max 0))
                 next-y (-> (+ y dy)
                            (min (dec grid-h))
                            (max 0))
                 next-z (get-in surface [next-x next-y])]
             (recur {:x next-x
                     :y next-y
                     :dx dx
                     :dy dy
                     :z next-z
                     :Q-map Q-map}
                    upd-htm))))))))

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
