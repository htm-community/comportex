(ns org.nfrac.comportex.spatial-pooling-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.hierarchy :as hier]
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

(defn model
  []
  (hier/regions-in-series 1 hier/sensory-region [params]
                          {:input sensor}))

(deftest sp-test
  (let [htm-step+cols (fn [this input]
                        (let [x (p/htm-step this input)]
                          (assoc-in x [:active-columns-at (p/timestep x)]
                                    (-> (first (hier/region-seq x))
                                        :layer-3
                                        p/layer-state
                                        :active-columns))))
        m1 (reduce htm-step+cols (model) (take 500 (input-seq)))
        rgn (first (hier/region-seq m1))
        lyr (:layer-3 rgn)
        n-cols (p/size-of lyr)]
    (testing "Column activation is distributed and moderated."
      (is (pos? (count (:active-columns (p/layer-state lyr))))
          "Some columns are active.")
      (is (pos? (util/quantile (:active-duty-cycles lyr) 0.9))
          "At least 10% of columns have been active.")
      (let [nactive-ts (for [t (range 400 500)]
                         (count (get-in m1 [:active-columns-at t])))]
        (is (every? #(< % (* n-cols 0.20)) nactive-ts)
            "Inhibition limits active columns in each time step."))
      (let [sg (:proximal-sg lyr)
            nsyns (for [col (range n-cols)]
                    (count (p/sources-connected-to sg [col 0 0])))]
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
                           [_ encoder] sensor
                           ff-bits (into #{} (p/encode encoder this-in))
                           rgn2 (p/region-step rgn ff-bits)]]
                 [k (:active-columns (p/layer-state (:layer-3 rgn2)))])
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
