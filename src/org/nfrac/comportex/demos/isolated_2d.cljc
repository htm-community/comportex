(ns org.nfrac.comportex.demos.isolated-2d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def input-size [50 50])
(def n-on-bits 64)

;; for block encoder
(def numb-domain [10 10])

;; for coordinate encoder
(def radius (-> (Math/sqrt (* 2 n-on-bits)) ;; select 50%
                (/ 2)
                (long)))
;; number of coordinates between integers:
(def resolution (* 2 radius
                   0.8)) ;; 0.2 overlap between successive integers

(def spec
  {:column-dimensions [20 50]
   :ff-init-frac 0.3
   :ff-potential-radius 0.2
   :global-inhibition? false
   :duty-cycle-period 100000
   :inhibition-base-distance 1
   })

(def higher-level-spec-diff
  {:column-dimensions [20 20]
   :global-inhibition? true
   :ff-potential-radius 1.0
   :ff-max-segments 5})

(def patterns
  {:down-1 (mapv vector (repeat 1) (range 10))
   :down-right (into (mapv vector (repeat 1) (range 5))
                     (mapv vector (range 1 10 2) (repeat 5)))
   :diag-tl-br (mapv vector (range 10) (range 10))
   :rand-10 (vec (repeatedly 10 #(vector (util/rand-int 0 10)
                                         (util/rand-int 0 10))))
   })

(def pattern-order (keys patterns))

(def gap-length 1)

(defn initial-world
  []
  (let [id (first pattern-order)]
    {:id id
     :values (patterns id)
     :index 0}))

(defn world-transform
  [{:keys [id values index] :as input}]
  (if (< index (dec (count values)))
    ;; continuing sequence
    (update-in input [:index] inc)
    ;; reached the end of a sequence
    (if id
      ;; start gap
      {:id nil
       :values (repeat gap-length nil)
       :index 0}
      ;; start another pattern
      (let [id (util/rand-nth pattern-order)]
        {:id id
         :values (patterns id)
         :index 0}))))

(defn attach-current-value
  [m]
  (assoc m :value (get (:values m) (:index m))))

(defn input-seq
  "Returns an infinite lazy seq of sensory input values."
  []
  (->> (iterate world-transform (initial-world))
       (map attach-current-value)))

(def block-sensor
  [:value
   (enc/linear-2d-encoder input-size n-on-bits numb-domain)])

(def coord-sensor
  [:value
   (enc/coordinate-encoder input-size n-on-bits [resolution resolution]
                           [radius radius])])

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (core/regions-in-series n core/sensory-region
                           (list* spec (repeat (merge spec higher-level-spec-diff)))
                           {:input block-sensor})))
