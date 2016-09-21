(ns org.nfrac.comportex.spatial-pooling-test
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.test.check.random :as random]
            [clojure.set :as set]
            [clojure.test :as t
             :refer (is deftest testing run-tests)]))

(def numb-bits 127)
(def numb-on-bits 21)
(def numb-max 100)
(def numb-domain [0 numb-max])
(def n-in-items 5)
(def bit-width (* numb-bits n-in-items))

(defn input-seq
  []
  (->> [[] (random/make-random 42)]
       (iterate (fn [[_ rng]]
                  (let [[rng1 rng2] (random/split rng)]
                    ;; a list of n-in-items random ints
                    [(->> (random/split-n rng1 n-in-items)
                          (map #(util/rand-int % numb-max)))
                     rng2])))
       (map first)
       (rest)))

(def params {:column-dimensions [1000]
             :ff-potential-radius 0.5
             :duty-cycle-period 600
             :depth 1
             :distal {:max-segments 1}
             :lateral-synapses? false})


(def sensor
  [[]
   (enc/encat (repeat n-in-items
                      (enc/linear-encoder [numb-bits] numb-on-bits numb-domain)))])

(defn build
  []
  (cx/network {:layer-a (layer/layer-of-cells params)}
              {:input sensor}))

(deftest sp-test
  (let [htm-step+cols (fn [this input]
                        (let [x (cx/htm-step this input)]
                          (assoc-in x [:active-columns-at (cx/timestep x)]
                                    (-> (first (cx/layer-seq x))
                                        cx/layer-state
                                        :active-columns))))
        htm1 (reduce htm-step+cols (build) (take 500 (input-seq)))
        lyr (first (cx/layer-seq htm1))
        n-cols (:n-columns lyr)]
    (testing "Column activation is distributed and moderated."
      (is (pos? (count (:active-columns (cx/layer-state lyr))))
          "Some columns are active.")
      (is (pos? (util/quantile (:active-duty-cycles lyr) 0.9))
          "At least 10% of columns have been active.")
      (let [nactive-ts (for [t (range 400 500)]
                         (count (get-in htm1 [:active-columns-at t])))]
        (is (every? #(< % (* n-cols 0.20)) nactive-ts)
            "Inhibition limits active columns in each time step."))
      (let [sg (:proximal-sg lyr)
            nsyns (for [col (range n-cols)]
                    (count (syn/sources-connected-to sg [col 0 0])))]
        (is (>= (apply min nsyns) 1)
            "All columns have at least one connected input synapse."))
      (let [bs (:boosts lyr)]
        (is (== 1.0 (util/quantile bs 0.1))
            "At least 10% of columns are unboosted.")))

    (testing "Column activation acts as a Locality Sensitive Hashing function."
      (let [in (repeat n-in-items 50)
            m (->>
               (for [[k d] [[:orig 0]
                            [:near 2]
                            [:mid 8]
                            [:far 25]]
                     :let [this-in (mapv (partial + d) in)
                           htm2 (-> (cx/htm-sense htm1 this-in nil)
                                    (cx/htm-activate))
                           lyr (first (cx/layer-seq htm2))]]
                 [k (:active-columns (cx/layer-state lyr))])
               (into {}))]
        (is (> (count (set/intersection (:orig m) (:near m)))
               (* (count (:orig m)) 0.5))
            "Minor noise leads to a majority of columns remaining active.")
        (is (< (count (set/intersection (:orig m) (:mid m)))
               (count (set/intersection (:orig m) (:near m))))
            "Increasing noise level reduces similarity of active column set - near")
        (is (< (count (set/intersection (:orig m) (:far m)))
               (count (set/intersection (:orig m) (:mid m))))
            "Increasing noise level reduces similarity of active column set - far")))))
