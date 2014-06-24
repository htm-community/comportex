(ns org.nfrac.comportex.perf-test
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.set :as set]
            #+clj [criterium.core :as crit]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]))

(def bit-width 400)
(def on-bits 25)
(def numb-max 15)
(def numb-domain [0 numb-max])

(def patterns
  [(range 0 5)
   (range 3 9)
   (range 8 12)
   (reverse (range 0 15))])

(defn mix-patterns
  "Returns an infinite sequence of sets of numbers."
  [patterns]
  (->> patterns
       (map #(apply concat (repeat %)))
       (apply map (fn [& xs] (set xs)))))

(defn crouching-head-hidden-tail
  "Returns the first element, with the rest in metadata to avoid
   printing an infinite sequence."
  [xs]
  (-> (first xs)
      (with-meta {::next (next xs)})))

;; a function not a value; do not hold on to the head of an infinite seq.
(defn initial-input
  []
  (let [inseq (mix-patterns patterns)]
    (crouching-head-hidden-tail inseq)))

(defn input-transform
  [v]
  (crouching-head-hidden-tail (::next (meta v))))

(def efn
  (enc/superpose-encoder
   (enc/linear-number-encoder bit-width on-bits numb-domain)))

(def small
  {:ncol 300
   :input-size 200
   :potential-radius 100
   :activation-level 0.06
   :global-inhibition true
   :stimulus-threshold 2
   :sp-perm-inc 0.04
   :sp-perm-dec 0.01
   :sp-perm-connected 0.1
   :duty-cycle-period 1000
   :max-boost 2.0
   ;; sequence memory:
   :depth 5
   :new-synapse-count 10
   :activation-threshold 7
   :min-threshold 5
   :connected-perm 0.20
   :initial-perm 0.16
   :permanence-inc 0.04
   :permanence-dec 0.01
   })

(defn model
  []
  (let [gen (core/generator (initial-input) input-transform efn
                            {:bit-width bit-width})
        spec (assoc small
               :input-size bit-width
               :potential-radius (quot bit-width 4))]
    (core/cla-model gen spec)))

#+clj
(deftest sm-perf-test
  (util/set-seed! 0)
  (let [m1 (-> (iterate core/step (model))
               (nth 200))]
    (crit/with-progress-reporting
      (crit/bench
       (do (util/set-seed! 0)
           ;(-> (iterate core/step m1) (nth 10))
           (core/step m1)
           )))))
