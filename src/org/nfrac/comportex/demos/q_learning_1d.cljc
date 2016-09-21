(ns org.nfrac.comportex.demos.q-learning-1d
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [clojure.core.async :refer [put!]]))

(def input-dim [400])
(def n-on-bits 20)
(def surface [0 0.5 1 1.5 2 1.5 1 0.5
              0 1 2 3 4 5 4 3 2
              1 1 1 1 1 1
              1 2 3 4 5 6 7 8 6 4 2])


(def initial-inval
  {:x 5
   :y (surface 5)
   :dy 0
   :action {:dx 0}})

(def params
  {:column-dimensions [500]
   :depth 1
   :max-boost 3.0
   :boost-active-every 50
   :duty-cycle-period 500
   :boost-active-duty-ratio 0.05
   :adjust-overlap-duty-ratio 0
   :float-overlap-duty-ratio 0
   :ff-potential-radius 1.0
   :ff-init-frac 0.5})

(def action-params
  {:column-dimensions [30]
   :activation-level 0.20
   :ff-potential-radius 1.0
   :ff-init-frac 0.5
   :proximal {:perm-inc 0.05
              :perm-dec 0.05
              :perm-connected 0.10
              :stimulus-threshold 1
              :learn? false} ;; using Q learning instead
   :ff-perm-init [0.35 0.45]
   ;; chosen for exploration - fresh connections fully boosted > 1.0:
   :max-boost 3.0
   :boost-active-every 100
   :duty-cycle-period 500
   :boost-active-duty-ratio 0.05
   :adjust-overlap-duty-ratio 0
   :float-overlap-duty-ratio 0
   :depth 1
   :q-alpha 0.2
   :q-discount 0.8
   ;; do not want temporal pooling here - actions are not static
   :stable-activation-steps 1})

(def direction->action
  {:left {:dx -1}
   :right {:dx 1}})

(defn possible-directions
  [x]
  (cond
    (zero? x) #{:right}
    (== x (dec (count surface))) #{:left}
    :else #{:left :right}))

;; lookup on columns of :action layer
(def column->signal
  (zipmap (range)
          (for [direction [:left :right]
                influence (repeat 15 1.0)]
            [direction influence])))

(defn select-action
  [htm curr-pos]
  (let [alyr (get-in htm [:layers :action])
        acols (:active-columns (cx/layer-state alyr))
        signals (map column->signal acols)
        poss (possible-directions curr-pos)]
    (->> signals
         (reduce (fn [m [motion influence]]
                   (assoc! m motion (+ (get m motion 0) influence)))
                 (transient (zipmap [:left :right] (repeat 0))))
         (persistent!)
         (filter (comp poss key))
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
          (syn/in-synapses sg target-id)))

(defn active-synapse-perms
  [sg target-id ff-bits]
  (keep (fn [[in-id p]]
          (when (ff-bits in-id) p))
        (syn/in-synapses sg target-id)))

(defn mean [xs] (/ (apply + xs) (count xs)))

(defn q-learn
  [lyr reward]
  (let [{:keys [ff-perm-init q-alpha q-discount]} (cx/params lyr)
        [p-ref _] ff-perm-init
        ff-bits (or (-> lyr :active-state :in-ff-signal :bits set) #{})
        prev-ff-bits (or (-> lyr :prior-active-state :in-ff-signal :bits set) #{})
        acols (:active-cols (:active-state lyr))
        prev-acols (:active-cols (:prior-active-state lyr))
        psg (:proximal-sg lyr)
        ;; Q = estimate of optimal future value = average active perm.
        aperms (mapcat (fn [col]
                         (active-synapse-perms psg [col 0 0] ff-bits))
                       acols)
        Q-est (if (seq aperms)
                (- (mean aperms) p-ref) ;; TODO include boost
                0)
        Q-old (:Q-val (:Q-info lyr) 0)
        learn-value (+ reward (* q-discount Q-est))
        adjust (* q-alpha (- learn-value Q-old))
        op (if (pos? adjust) :reinforce :punish)
        seg-updates (map (fn [col]
                           (syn/seg-update [col 0 0] op nil nil))
                         prev-acols)]
    (->
     (cx/layer-learn lyr)
     (assoc :proximal-sg
            (syn/bulk-learn psg seg-updates prev-ff-bits
                            (abs adjust) (abs adjust) 0.0))
     (assoc :Q-info {:Q-val Q-est
                     :Q-old Q-old
                     :reward reward
                     :lrn learn-value
                     :adj adjust
                     :perms (count aperms)}))))

(defn build
  []
  (let [sensor [:x
                (enc/linear-encoder
                 input-dim n-on-bits [-5 (+ (count surface) 5)])]
        msensor [[:action :dx]
                 (enc/linear-encoder [100] 30 [-1 1])]]
    (cx/network {:layer-a (layer/layer-of-cells
                            (assoc params :lateral-synapses? false))
                 :action (layer/layer-of-cells action-params)}
                {:input sensor
                 :motor msensor}
                {:ff-deps {:layer-a [:input]
                           :action [:layer-a]}
                 :lat-deps {:layer-a [:motor]}})))

(defn htm-step-with-action-selection
  [world-c]
  (fn [htm inval]
    (let [;; do first part of step, but not depolarise yet (depends on action)
          htm-a (-> htm
                    (cx/htm-sense inval :ff)
                    (cx/htm-activate)
                    ;(cx/htm-learn) do not do normal learning in action layer:
                    (update-in [:layers :layer-a] cx/layer-learn))
          ;; scale reward to be comparable to [0-1] permanences
          reward (* 0.5 (:dy inval))
          ;; do the Q learning update on action layer
          upd-htm (update-in htm-a [:layers :action] q-learn reward)
          ;; maintain map of state+action -> approx Q values, for diagnostics
          info (get-in upd-htm [:layers :action :Q-info])
          newQ (-> (+ (:Q-old info 0) (:adj info 0))
                   (max -1.0)
                   (min 1.0))
          Q-map (assoc (:Q-map inval)
                       (select-keys inval [:x :action])
                       newQ)
          action (select-action upd-htm (:x inval))
          inval-with-action (assoc inval
                                   :action action
                                   :prev-action (:action inval)
                                   :Q-map Q-map)]
      ;; calculate the next position
      (let [new-inval (apply-action inval-with-action)]
        (put! world-c new-inval))
      (-> upd-htm
          (cx/htm-sense inval-with-action :lat)
          (cx/htm-depolarise)))))

(comment
  (require '[clojure.core.async :as async :refer [>!! <!!]])
  (def world-c (async/chan))
  (def model (atom (build)))
  (def step (htm-step-with-action-selection world-c))

  (def inval initial-inval)
  (swap! model step inval)
  (def inval (<!! world-c))

  inval
  (get-in @model [:layers :action :Q-info])
  (get-in @model [:layers :action :active-state :active-cols]))
