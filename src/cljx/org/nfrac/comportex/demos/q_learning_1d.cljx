(ns org.nfrac.comportex.demos.q-learning-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            #+clj [clojure.core.async :refer [<! >! go]]
            #+cljs [cljs.core.async :refer [<! >!]])
    #+cljs (:require-macros [cljs.core.async.macros :refer [go]]))

(def input-dim [1000])
(def on-bits 100)
(def radius 1.0)
(def surface [0 1 2 3 4 3 2 1
              0 1 2 3 4 5 4 3 2
              1 1 1 1 1 1
              1 2 3 4 5 6 7 8 7 6 5 4
              ])

(def initial-input-val
  {:x 5
   :y (surface 5)
   :dy 0
   :dx 0})

(def spec
  {:column-dimensions [1000]
   :depth 4
   :distal-punish? true
   :duty-cycle-period 500
   :boost-active-duty-ratio 0.01
   :ff-potential-radius 0.15
   :ff-init-frac 0.5})

(def action-spec
  {:column-dimensions [240]
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
          (for [motion [:left :right]
                influence (concat (repeat 40 -1)  ;; inhibit
                                  (repeat 80 1))] ;; excite
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
         (map (fn [[motion total]]
                (let [effect (max 0 total)]
                  (if (= motion :left) (- effect) effect))))
         (apply + 0)
         (int)
         (max -1)
         (min 1))))

(defn active-synapses
  [sg target-id ff-bits]
  (filter (fn [[in-id p]]
            (ff-bits in-id))
          (p/in-synapses sg target-id)))

(defn active-synapse-perms
  [sg target-id ff-bits]
  (keep (fn [[in-id p]]
          (when (ff-bits in-id) p))
        (p/in-synapses sg target-id)))

(defn mean [xs] (/ (apply + xs) (count xs)))

(defn q-learn
  [model reward]
  (update-in model [:regions :action :layer-3]
             (fn [lyr]
               (let [{:keys [ff-perm-init-lo q-alpha q-discount]} (p/params lyr)
                     ff-bits (or (:in-ff-bits (:state lyr)) #{})
                     acols (:active-cols (:state lyr))
                     prior-ff-bits (or (:in-ff-bits (:prior-state lyr)) #{})
                     prior-acols (:active-cols (:prior-state lyr))
                     psg (:proximal-sg lyr)
                     aperms (mapcat (fn [col]
                                      (active-synapse-perms psg col ff-bits))
                                    acols)
                     Qt-st+1 (if (seq aperms)
                               (- (mean aperms) ff-perm-init-lo) ;; include boost?
                               0)
                     Qt-st (:Q-val (:prior-state lyr) 0)
                     learn-value (+ reward (* q-discount Qt-st+1))
                     adjust (* q-alpha (- learn-value Qt-st))
                     up? (pos? adjust)]
                 (-> 
                  (p/layer-learn lyr)
                  (assoc :proximal-sg
                    (reduce (fn [psg col]
                              (p/reinforce-in-synapses
                               psg col (complement prior-ff-bits)
                               (constantly up?) (abs adjust) (abs adjust)))
                            psg
                            prior-acols))
                  (assoc-in [:state :Q-val] Qt-st+1)
                  (assoc-in [:state :Q-info] {:Qt Qt-st
                                              :reward reward
                                              :lrn learn-value
                                              :adj adjust
                                              :perms (count aperms)})))
               )))

(defn make-model
  []
  (let [encoder (enc/pre-transform (fn [{:keys [x y]}]
                                     {:coord [x y]
                                      :radii [radius radius]})
                                   (enc/coordinate-encoder input-dim on-bits))
        mencoder (enc/pre-transform :dx (enc/linear-encoder 100 30 [-1 1]))
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
       (let [reward (* 0.5 (:dy inval))]
         (swap! model-atom q-learn reward))
       (let [dx (select-action model)
             next-x (-> (+ (:x inval) dx)
                        (min (dec (count surface)))
                        (max 0))
             next-y (surface next-x)
             dy (- next-y (:y inval))]
         (recur {:x next-x
                 :y next-y
                 :dx dx
                 :dy dy}))))))

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
