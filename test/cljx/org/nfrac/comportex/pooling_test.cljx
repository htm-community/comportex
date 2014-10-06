(ns org.nfrac.comportex.pooling-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.pooling :as pooling]
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

(defn gen-ins
  []
  (repeatedly n-in-items #(util/rand-int 0 numb-max)))

(def spec {:column-dimensions [1000]
           :input-dimensions [bit-width]
           :ff-potential-radius (quot bit-width 2)
           :global-inhibition true
           :duty-cycle-period 600})

(def encoder
  (enc/encat n-in-items
             (enc/linear-encoder numb-bits numb-on-bits numb-domain)))

(deftest pooling-test
  (let [efn (partial enc/encode encoder 0)
        cf (pooling/column-field spec)
        ncol (p/size (p/topology cf))
        cf1k (reduce (fn [cf in]
                       (-> cf
                           (assoc-in [:active-columns-at (p/timestep cf)]
                                     (p/active-columns cf))
                           (p/columns-step in #{} {} true)))
                     cf
                     (map efn (repeatedly 1000 gen-ins)))]
    
    (testing "Column activation is distributed and moderated."
      (is (pos? (util/quantile (:overlap-duty-cycles cf1k) 0.01))
          "At least 99% of columns have overlapped with input at least once.")
      (is (pos? (util/quantile (:active-duty-cycles cf1k) 0.8))
          "At least 20% of columns have been active.")
      (let [nactive-ts (for [t (range 900 1000)]
                         (count (get-in cf1k [:active-columns-at t])))]
        (is (every? #(< % (* ncol 0.2)) nactive-ts)
            "Inhibition limits active columns in each time step."))
      (let [sg (:ff-sg cf1k)
            nsyns (for [col (range ncol)]
                    (count (p/sources-connected-to sg col)))]
        (is (>= (apply min nsyns) 1)
            "All columns have at least one connected input synapse."))
      (let [bs (:boosts cf1k)]
        (is (== 1.0 (util/quantile bs 0.1))
            "At least 10% of columns are unboosted.")))

    (testing "Spatial pooler acts as a Locality Sensitive Hashing function."
      (let [in (repeat n-in-items 50)
            m (->>
               (for [[k d] [[:orig 0]
                            [:near 2]
                            [:mid 8]
                            [:far 25]]
                     :let [this-in (mapv (partial + d) in)
                           cf2 (p/columns-step cf1k (efn this-in) #{} {} true)]]
                 [k (p/active-columns cf2)])
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
