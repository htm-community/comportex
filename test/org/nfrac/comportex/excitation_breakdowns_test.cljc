(ns org.nfrac.comportex.excitation-breakdowns-test
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            #?(:clj [clojure.test :as t
                     :refer (is deftest testing run-tests)]
               :cljs [cemerick.cljs.test :as t
                      :refer-macros (is deftest testing run-tests)])))

(def bit-width 200)
(def n-on-bits 20)

(def inputs
  [:a :b :c :d
   :a :b :f :g
   :a :b :b :a
   :h :g :f :e
   :d :e :a :d])

(def initial-input {:index 0})

(defn input-transform
  [m]
  (update m :index #(mod (inc %) (count inputs))))

(defn world-seq
  "Returns a sequence of sensory input values."
  []
  (->> (iterate input-transform initial-input)
       (map #(assoc % :value (get inputs %)))))

(def sensor
  [:value
   (enc/unique-encoder [bit-width] n-on-bits)])

(def spec
  {})

(defn model
  []
  (core/regions-in-series 2 core/sensory-region (repeat spec)
                          {:input sensor}))

(deftest exc-bd-test
  (let [[warmups continued] (split-at 50 (world-seq))
        prev-htm (reduce p/htm-step (model) warmups)
        htm (p/htm-step prev-htm (first continued))]
    (testing "Cell excitation breakdowns"
      (let [lyr (get-in htm [:regions :rgn-0 :layer-3])
            lc (p/learnable-cells lyr)
            bd (core/cell-excitation-breakdowns htm prev-htm :rgn-0 :layer-3
                                                (conj lc [0 0]))]
        (is (every? (comp pos? :total) (map bd lc))
            "All total excitation in range.")
        (is (every? (comp pos? first vals :proximal-unstable) (map bd lc))
            "Some proximal excitation on each active column")
        (is (every? (comp zero? :temporal-pooling) (vals bd))
            "Zero TP excitation in first layer.")
        (is (every? (comp map? :distal) (vals bd))
            "Distal keys hold maps.")))))
