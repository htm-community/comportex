(ns org.nfrac.comportex.spatial-pooling-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.set :as set]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.cljs.test
                           :refer (is deftest testing run-tests)]))

(def numb-bits 127)
(def numb-on-bits 21)
(def numb-max 100)
(def numb-domain [0 numb-max])
(def n-in-items 5)
(def bit-width (* numb-bits n-in-items))

(def initial-input
  (repeat n-in-items (quot numb-max 2)))

(defn input-transform
  "Ignore previous value, generate new random one."
  [_]
  (repeatedly n-in-items #(util/rand-int 0 numb-max)))

(def spec {:column-dimensions [1000]
           :input-dimensions [bit-width]
           :ff-potential-radius (quot bit-width 2)
           :global-inhibition true
           :duty-cycle-period 600
           :depth 1
           :max-segments 1
           :lateral-synapses? false
           })

(def encoder
  (enc/encat n-in-items
             (enc/linear-encoder numb-bits numb-on-bits numb-domain)))

(defn model
  []
  (let [input (core/sensory-input initial-input input-transform encoder)]
    (core/tree core/sensory-region spec
               [input])))

(deftest sp-test
  (util/set-seed! 0)
  (let [model1 (->> (iterate (fn [m]
                           (-> (p/feed-forward-step m)
                               (assoc-in [:active-columns-at (p/timestep m)]
                                         (p/active-columns (:layer-3 (:region m))))))
                         (model))
                (take 500)
                (last))
        rgn (:region model1)
        cf (:column-field rgn)
        n-cols (p/size-of cf)]
    (testing "Column activation is distributed and moderated."
      (is (pos? (util/quantile (:overlap-duty-cycles cf) 0.01))
          "At least 99% of columns have overlapped with input at least once.")
      (is (pos? (util/quantile (:active-duty-cycles cf) 0.9))
          "At least 10% of columns have been active.")
      (let [nactive-ts (for [t (range 400 500)]
                         (count (get-in model1 [:active-columns-at t])))]
        (is (every? #(< % (* n-cols 0.20)) nactive-ts)
            "Inhibition limits active columns in each time step."))
      (let [sg (:ff-sg cf)
            nsyns (for [col (range n-cols)]
                    (count (p/sources-connected-to sg col)))]
        (is (>= (apply min nsyns) 1)
            "All columns have at least one connected input synapse."))
      (let [bs (:boosts cf)]
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
                           rgn2 (p/region-step rgn (p/encode encoder 0 this-in) #{})]]
                 [k (p/active-columns (:layer-3 rgn2))])
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
