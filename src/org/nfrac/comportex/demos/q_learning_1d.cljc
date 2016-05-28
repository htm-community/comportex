(ns org.nfrac.comportex.demos.q-learning-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            #?(:clj [clojure.core.async :refer [put!]]
               :cljs [cljs.core.async :refer [put!]])))

(def input-dim [400])
(def n-on-bits 40)
(def coord-radius 60) ;; so 60+1+60 = 121 candidates (and we choose 40)
(def surface-coord-scale 60) ;; so neighbouring positions (x +/- 1) share ~50% bits
(def surface [0 0.5 1 1.5 2 1.5 1 0.5
              0 1 2 3 4 5 4 3 2
              1 1 1 1 1 1
              1 2 3 4 5 6 7 8 6 4 2
              ])

(def initial-inval
  {:x 5
   :y (surface 5)
   :dy 0
   :action {:dx 0}})

(def spec
  {:column-dimensions [1000]
   :depth 4
   :distal {:punish? true}
   :duty-cycle-period 300
   :boost-active-duty-ratio 0.01
   :ff-potential-radius 0.15
   :ff-init-frac 0.5})

(def action-spec
  {:column-dimensions [30]
   :activation-level 0.20
   :ff-potential-radius 1.0
   :ff-init-frac 0.5
   :proximal {:perm-inc 0.05
              :perm-dec 0.05
              :perm-connected 0.10}
   :ff-perm-init-lo 0.35
   :ff-perm-init-hi 0.45
   ;; chosen for exploration - fresh connections fully boosted > 1.0:
   :max-boost 3.0
   :boost-active-every 1
   :duty-cycle-period 150
   :boost-active-duty-ratio 0.05
   :depth 1
   :q-alpha 0.2
   :q-discount 0.8
   ;; do not want temporal pooling here - actions are not static
   :temporal-pooling-max-exc 0.0
   ;; disable learning; custom learning function below
   :freeze? true})

(def direction->action
  {:left {:dx -1}
   :right {:dx 1}})

;; lookup on columns of :action region
(def column->signal
  (zipmap (range)
          (for [direction [:left :right]
                influence (repeat 15 1.0)]
            [direction influence])))

(defn select-action
  [htm]
  (let [alyr (get-in htm [:regions :action :layer-3])
        acols (p/active-columns alyr)
        signals (map column->signal acols)]
    (->> signals
         (reduce (fn [m [motion influence]]
                   (assoc! m motion (+ (get m motion 0) influence)))
                 (transient {}))
         (persistent!)
         (shuffle)
         (apply max-key val)
         (key)
         (direction->action))))

(defn apply-action
  [inval]
  (let [x (:x inval)
        dx (:dx (:action inval))
        next-x (-> (+ x dx)
                   (min (dec (count surface)))
                   (max 0))
        next-y (surface next-x)
        dy (- next-y (:y inval))]
    (assoc inval
           :x next-x
           :y next-y
           :dy dy)))

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
  [htm prev-htm reward]
  (update-in htm [:regions :action :layer-3]
             (fn [lyr]
               (let [prev-lyr (get-in prev-htm [:regions :action :layer-3])
                     {:keys [ff-perm-init-lo q-alpha q-discount]} (p/params lyr)
                     ff-bits (or (:in-ff-bits (:state lyr)) #{})
                     acols (:active-cols (:state lyr))
                     prev-ff-bits (or (:in-ff-bits (:state prev-lyr)) #{})
                     prev-acols (:active-cols (:state prev-lyr))
                     psg (:proximal-sg lyr)
                     ;; Q = estimate of optimal future value = average active perm.
                     aperms (mapcat (fn [col]
                                      (active-synapse-perms psg [col 0 0] ff-bits))
                                    acols)
                     Q-est (if (seq aperms)
                             (- (mean aperms) ff-perm-init-lo) ;; TODO include boost
                             0)
                     Q-old (:Q-val (:Q-info lyr) 0)
                     learn-value (+ reward (* q-discount Q-est))
                     adjust (* q-alpha (- learn-value Q-old))
                     op (if (pos? adjust) :reinforce :punish)
                     seg-updates (map (fn [col]
                                        (syn/seg-update [col 0 0] op nil nil))
                                      prev-acols)]
                 (->
                  (p/layer-learn lyr)
                  (assoc :proximal-sg
                         (p/bulk-learn psg seg-updates prev-ff-bits
                                       (abs adjust) (abs adjust) 0.0))
                  (assoc :Q-info {:Q-val Q-est
                                  :Q-old Q-old
                                  :reward reward
                                  :lrn learn-value
                                  :adj adjust
                                  :perms (count aperms)}))))))

(defn make-model
  []
  (let [sensor [(enc/vec-selector :x)
                (enc/coordinate-encoder input-dim n-on-bits [surface-coord-scale]
                                        [coord-radius])]
        msensor [[:action :dx]
                 (enc/linear-encoder [100] 30 [-1 1])]]
    (core/region-network {:rgn-1 [:input :motor]
                          :action [:rgn-1]}
                         (constantly core/sensory-region)
                         {:rgn-1 (assoc spec :lateral-synapses? false)
                          :action action-spec}
                         {:input sensor}
                         {:input sensor
                          :motor msensor})))

(defn htm-step-with-action-selection
  [world-c]
  (fn [htm inval]
    (let [;; do first part of step, but not depolarise yet (depends on action)
          htm-a (-> htm
                    (p/htm-sense inval :sensory)
                    (p/htm-activate)
                    (p/htm-learn))
          ;; scale reward to be comparable to [0-1] permanences
          reward (* 0.5 (:dy inval))
          ;; do the Q learning update on action layer
          upd-htm (q-learn htm-a htm reward)
          ;; maintain map of state+action -> approx Q values, for diagnostics
          info (get-in upd-htm [:regions :action :layer-3 :Q-info])
          newQ (-> (+ (:Q-old info 0) (:adj info 0))
                   (max -1.0)
                   (min 1.0))
          Q-map (assoc (:Q-map inval)
                       (select-keys inval [:x :action])
                       newQ)
          action (select-action upd-htm)
          inval-with-action (assoc inval
                                   :action action
                                   :prev-action (:action inval)
                                   :Q-map Q-map)]
      ;; calculate the next position
      (let [new-inval (apply-action inval-with-action)]
        (put! world-c new-inval))
      (-> upd-htm
          (p/htm-sense inval-with-action :motor)
          (p/htm-depolarise)))))

(comment
  (require '[clojure.core.async :as async :refer [>!! <!!]])
  (def world-c (async/chan))
  (def model (atom (make-model)))
  (def step (htm-step-with-action-selection world-c))

  (def inval initial-inval)
  (swap! model step inval)
  (def inval (<!! world-c))

  inval
  (get-in @model [:regions :action :layer-3 :Q-info])
  (get-in @model [:regions :action :layer-3 :state :active-cols])

  )
