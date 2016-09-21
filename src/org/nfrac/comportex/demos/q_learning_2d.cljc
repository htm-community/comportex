(ns org.nfrac.comportex.demos.q-learning-2d
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [org.nfrac.comportex.demos.q-learning-1d :refer [q-learn]]
            [clojure.core.async :refer [put!]]))

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

(def initial-inval
  {:x 0
   :y 0
   :z 0
   :action {:dx 0
            :dy 0}})

(def params
  {:column-dimensions [30 30]
   :depth 4
   :boost-active-every 100
   :duty-cycle-period 300
   :boost-active-duty-ratio 0.02
   :adjust-overlap-duty-ratio 0
   :float-overlap-duty-ratio 0
   :ff-potential-radius 0.15
   :ff-init-frac 0.5})

(def action-params
  {:column-dimensions [4 10]
   :activation-level 0.20
   :ff-potential-radius 1
   :ff-init-frac 0.5
   :proximal {:perm-inc 0.05
              :perm-dec 0.05
              :perm-connected 0.10
              :stimulus-threshold 1
              :learn? false} ;; using Q learning instead
   :ff-perm-init [0.35 0.45]
   ;; chosen for exploration - fresh connections fully boosted > 1.0:
   :max-boost 3.0
   :boost-active-every 1
   :duty-cycle-period 250
   :boost-active-duty-ratio 0.05
   :adjust-overlap-duty-ratio 0
   :float-overlap-duty-ratio 0
   :depth 1
   :q-alpha 0.75
   :q-discount 0.9
   ;; do not want temporal pooling here - actions are not static
   :stable-activation-steps 1})

(def direction->action
  {:up {:dx 0 :dy -1}
   :down {:dx 0 :dy 1}
   :left {:dx -1 :dy 0}
   :right {:dx 1 :dy 0}})

(defn possible-directions
  [[x y]]
  (cond-> #{:up :down :left :right}
          (zero? x) (disj :left)
          (zero? y) (disj :up)
          (= x (dec grid-w)) (disj :right)
          (= y (dec grid-h)) (disj :down)))

;; lookup on columns of :action layer
(def column->signal
  (zipmap (range)
          (for [motion [:up :down :left :right]
                influence (repeat 10 1.0)]
            [motion influence])))

(defn select-action
  [htm curr-pos]
  (let [alyr (get-in htm [:layers :action])
        acols (:active-columns (cx/layer-state alyr))
        signals (map column->signal acols)
        poss (possible-directions curr-pos)]
    (->> signals
         (reduce (fn [m [motion influence]]
                   (assoc! m motion (+ (get m motion 0) influence)))
                 (transient (zipmap [:up :down :left :right] (repeat 0))))
         (persistent!)
         (filter (comp poss key))
         (apply max-key val)
         (key)
         (direction->action))))

(defn apply-action
  [inval]
  (let [x (:x inval)
        y (:y inval)
        dx (:dx (:action inval))
        dy (:dy (:action inval))
        next-x (-> (+ x dx)
                   (min (dec grid-w))
                   (max 0))
        next-y (-> (+ y dy)
                   (min (dec grid-h))
                   (max 0))
        next-z (get-in surface [next-x next-y])]
    (assoc inval
           :x next-x
           :y next-y
           :z next-z)))

(defn build
  []
  (let [sensor [(enc/vec-selector :x :y)
                (enc/coordinate-encoder input-dim n-on-bits
                                        [surface-coord-scale surface-coord-scale]
                                        [coord-radius coord-radius])]
        dx-sensor [[:action :dx] (enc/linear-encoder [100] 30 [-1 1])]
        dy-sensor [[:action :dy] (enc/linear-encoder [100] 30 [-1 1])]
        msensor (enc/sensor-cat dx-sensor dy-sensor)]
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
          reward (* 0.01 (:z inval))
          terminal-state? (>= (abs (:z inval)) 100)
          ;; do the Q learning update on action layer (except initially)
          upd-htm (if (:prev-action inval)
                    (update-in htm-a [:layers :action] q-learn reward)
                    (assoc-in htm-a [:layers :action :Q-info] {}))
          ;; maintain map of state+action -> approx Q values, for diagnostics
          info (get-in upd-htm [:layers :action :Q-info])
          newQ (-> (+ (:Q-old info 0) (:adj info 0))
                   (max -1.0)
                   (min 1.0))
          Q-map (assoc (:Q-map inval)
                       (select-keys inval [:x :y :action])
                       newQ)
          action (select-action upd-htm [(:x inval) (:y inval)])
          inval-with-action (assoc inval
                                   :action action
                                   :prev-action (:action inval)
                                   :Q-map Q-map)]
      ;; calculate the next position
      (let [new-inval (if terminal-state?
                        ;; restart
                        (assoc initial-inval :Q-map Q-map)
                        ;; continuing
                        (apply-action inval-with-action))]
        (put! world-c new-inval))
      (cond-> upd-htm
          true
          (cx/htm-sense inval-with-action :lat)
          true
          (cx/htm-depolarise)
          terminal-state?
          (cx/break :tm)))))

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
