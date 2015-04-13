(ns org.nfrac.comportex.sequence-memory-test
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            #?(:clj [clojure.test :as t
                     :refer (is deftest testing run-tests)]
               :cljs [cemerick.cljs.test :as t
                      :refer-macros (is deftest testing run-tests)])))

(def bit-width 200)
(def items [:a :b :c :d :e :f :g :h])

(def patterns
  [[:a :b :c :d]
   [:a :b :f :g]
   [:a :b :b :a]
   [:h :g :f :e]
   [:d :e :a :d]])

(def initial-input [0 0])

(defn input-transform
  [[i j]]
  (let [patt (get patterns i)]
    (if (< (inc j) (count patt))
      [i (inc j)]
      ;; finished pattern, choose another one
      [(util/rand-int 0 (dec (count patterns)))
       0])))

(def encoder
  (enc/pre-transform #(get-in patterns %)
                     (enc/category-encoder bit-width items)))

(def spec
  {:column-dimensions [400]
   :input-dimensions [200]
   :ff-potential-radius 0.5
   :ff-perm-inc 0.04
   :ff-perm-dec 0.01
   :ff-perm-connected 0.1
   :ff-stimulus-threshold 2
   :duty-cycle-period 1000
   :max-boost 2.0
   ;; sequence memory:
   :depth 5
   :seg-new-synapse-count 10
   :seg-stimulus-threshold 7
   :seg-learn-threshold 5
   :distal-perm-connected 0.20
   :distal-perm-inc 0.04
   :distal-perm-dec 0.01
   :distal-perm-init 0.16
   :global-inhibition? false
   :activation-level 0.04
   })

(defn model
  []
  (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                          1 spec))

(defn world-seq
  "Returns a sequence of sensory input values."
  []
  (iterate input-transform initial-input))

(deftest sm-test
  (util/set-seed! 0)
  (let [[warmups continued] (split-at 500 (world-seq))
        m1 (reduce p/htm-step (model) warmups)
        rgn (first (core/region-seq m1))]
    (testing "Numbers of lateral dendrite segments"
      (let [n-cols (p/size (p/topology rgn))
            lyr (:layer-3 rgn)
            depth (p/layer-depth lyr)
            distal-sg (:distal-sg lyr)
            nsegs-by-cell (for [col (range n-cols)
                                ci (range depth)]
                            (->> (p/cell-segments distal-sg [col ci])
                                 (filter seq)
                                 (count)))]
        (is (>= (apply max nsegs-by-cell) 1)
            "Some cells have grown lateral dendrite segments.")
        (is (pos? (util/quantile nsegs-by-cell 0.90))
            "At least 10% of cells have grown lateral dendrite segments.")
        (is (>= (apply max nsegs-by-cell) 2)
            "Some cells have grown multiple lateral dendrite segments.")))
    (testing "Column / cell activation"
      (let [sums (->> (take 100 continued)
                      (reductions p/htm-step m1)
                      (map (comp first core/region-seq))
                      (map core/column-state-freqs)
                      (apply merge-with +))]
        (is (> (:active-predicted sums) 0)
            "Some column activations are predicted.")
        (is (> (:active-predicted sums) (:active sums))
            "Most column activations are predicted.")
        (is (> (:predicted sums) 0)
            "Some columns were predicted but are not active.")
        (is (< (+ (:active sums)
                  (:active-predicted sums)
                  (:predicted sums))
               (* (:size sums) 0.25))
            "Less than 25% of columns are active or predicted.")))))
