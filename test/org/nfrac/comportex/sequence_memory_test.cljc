(ns org.nfrac.comportex.sequence-memory-test
  (:require [org.nfrac.comportex.hierarchy :as hier]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.test :as t
             :refer (is deftest testing run-tests)]))

(def bit-width 200)
(def items [:a :b :c :d :e :f :g :h])

(def patterns
  [[:a :b :c :d]
   [:a :b :f :g]
   [:a :b :b :a]
   [:h :g :f :e]
   [:d :e :a :d]])

(defn attach-current-value
  [m]
  (assoc m :value (get-in patterns (:path m))))

(defn input-seq
  "Returns a sequence of sensory input values."
  []
  (apply concat (repeat (flatten patterns))))

(def sensor
  [[]
   (enc/category-encoder [bit-width] items)])

(def params
  {})

(defn model
  []
  (hier/regions-in-series 1 hier/sensory-region [params] {:input sensor}))

(deftest sm-test
  (let [[warmups continued] (split-at 500 (input-seq))
        m1 (reduce p/htm-step (model) warmups)
        rgn (first (hier/region-seq m1))]
    (testing "Numbers of lateral dendrite segments"
      (let [n-cols (p/size (p/topology rgn))
            lyr (:layer-3 rgn)
            depth (p/layer-depth lyr)
            distal-sg (:distal-sg lyr)
            cells-with-segs (for [col (range n-cols)
                                  ci (range depth)
                                  :when (->> (p/cell-segments distal-sg [col ci])
                                             (filter seq)
                                             (seq))]
                              [col ci])]
        (is (seq cells-with-segs)
            "Some cells have grown lateral dendrite segments.")
        (is (>= (count (distinct (map first cells-with-segs))) (* 0.10 n-cols))
            "At least 10% of columns have grown lateral dendrite segments.")
        (is (>= (apply max 0 (vals (frequencies (map first cells-with-segs))))
                2)
            "Multiple cells in some columns have grown lateral dendrite segments.")))
    (testing "Column / cell activation"
      (let [sums (->> (take 100 continued)
                      (reductions p/htm-step m1)
                      (map (comp first hier/region-seq))
                      (map hier/column-state-freqs)
                      (apply merge-with +))]
        (is (> (+ (:active sums) (:active-predicted sums)) 0)
            "Some columns were active.")
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
