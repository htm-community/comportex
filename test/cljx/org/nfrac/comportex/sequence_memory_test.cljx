(ns org.nfrac.comportex.sequence-memory-test
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.cljs.test
                           :refer (is deftest testing run-tests)]))

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
  {:column-dimensions [300]
   :input-dimensions [200]
   :ff-potential-radius 100
   :ff-perm-inc 0.04
   :ff-perm-dec 0.01
   :ff-perm-connected 0.1
   :ff-stimulus-threshold 2
   :global-inhibition true
   :activation-level 0.06
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
   })

(defn model
  []
  (let [input (core/sensory-input initial-input input-transform encoder)]
    (core/tree core/sensory-region spec
               [input])))

(deftest sm-test
  (util/set-seed! 0)
  (testing "Sequence memory"
   (let [m1 (-> (iterate p/feed-forward-step (model))
                (nth 500))
         rgn (:region m1)]
     (let [ncols (p/size (p/topology rgn))
           lyr (:layer rgn)
           depth (p/layer-depth lyr)
           distal-sg (:distal-sg lyr)
           nsegs-by-cell (for [col (range ncols)
                               ci (range depth)]
                           (->> (p/cell-segments distal-sg [col ci])
                                (filter seq)
                                (count)))]
       (is (>= (apply max nsegs-by-cell) 1)
           "Some cells have grown lateral dendrite segments.")
       (is (pos? (util/quantile nsegs-by-cell 0.90))
           "At least 10% of cells have grown lateral dendrite segments.")
       (is (>= (apply max nsegs-by-cell) 2)
           "Some cells have grown multiple lateral dendrite segments."))
     (let [sums (->> m1
                     (iterate p/feed-forward-step)
                     (take 100)
                     (map :region)
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
