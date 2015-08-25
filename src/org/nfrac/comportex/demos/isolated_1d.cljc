(ns org.nfrac.comportex.demos.isolated-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def bit-width 300)
(def on-bits 20)

;; for block encoder
(def numb-max 15)
(def numb-domain [0 numb-max])

;; for coordinate encoder
(def radius on-bits) ;; select 50%
;; number of coordinates between integers:
(def resolution (* 2 radius
                   0.8)) ;; 0.2 overlap between successive integers

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

(def gap-length 5)

(defn initial-input
  []
  (let [id (first pattern-order)]
    {:id id
     :values (patterns id)
     :index 0}))

(defn input-transform
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

(defn current-value
  [m]
  (get (:values m) (:index m)))

(def block-encoder
  (enc/pre-transform current-value
                     (enc/linear-encoder bit-width on-bits numb-domain)))

(def coord-encoder
  (enc/pre-transform (fn [m]
                       (when-let [x (current-value m)]
                         {:coord [(* x resolution)]
                          :radii [radius]}))
                     (enc/coordinate-encoder [bit-width] on-bits)))

(defn world-seq
  "Returns an infinite lazy seq of sensory input values."
  []
  (iterate input-transform (initial-input)))

(defn n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region
                             block-encoder
                             n
                             (list* spec (repeat (merge spec higher-level-spec-diff))))))
