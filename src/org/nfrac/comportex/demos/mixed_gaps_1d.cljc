(ns org.nfrac.comportex.demos.mixed-gaps-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def bit-width 400)
(def n-on-bits 25)

;; for block encoder
(def numb-max 15)
(def numb-domain [0 numb-max])

(def spec
  {:column-dimensions [1000]
   :ff-init-frac 0.2
   :ff-potential-radius 1.0
   :ff-perm-inc 0.10
   :ff-perm-dec 0.01
   :duty-cycle-period 100000
   })

(def higher-level-spec-diff
  {:column-dimensions [400]
   :ff-max-segments 5})

(def patterns
  {:run-0-5 [0 1 2 3 4 5]
   :rev-5-1 [5 4 3 2 1]
   :run-6-10 [6 7 8 9 10]
   :jump-6-12 [6 7 8 11 12]
   :twos [0 2 4 6 8 10 12 14]
   :saw-10-15 [10 12 11 13 12 14 13 15]})

(def pattern-order (keys patterns))

(def gap-range
  (->> (vals patterns) (map count) (reduce +) (long) (* 2)))

(defn initial-world
  []
  (assoc {} (first pattern-order) 0))

(defn world-transform
  [input]
  (reduce (fn [m [id values]]
            (if-let [index (m id)]
              ;; pattern is currently active
              (if (< index (dec (count (patterns id))))
                ;; continue
                (update-in m [id] inc)
                ;; finished
                (dissoc m id))
              ;; pattern is not currently active
              (if (zero? (rand-int gap-range)) ;; mutation! impurity!
                ;; start
                (assoc m id 0)
                m)))
          input
          patterns))

(defn current-values
  [world]
  (map (fn [[id index]]
         (get-in patterns [id index]))
       world))

(defn input-seq
  "Returns an infinite lazy seq of sensory input values."
  []
  (->> (iterate world-transform (initial-world))
       (map #(assoc % :values (current-values %)))))

(def block-sensor
  [:values
   (enc/ensplat
    (enc/linear-encoder [bit-width] n-on-bits numb-domain))])

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (core/regions-in-series n core/sensory-region
                           (list* spec (repeat (merge spec higher-level-spec-diff)))
                           {:input block-sensor})))
