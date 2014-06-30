(ns org.nfrac.comportex.pooling-test
  (:require [org.nfrac.comportex.pooling :as p]
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
(def n-in-items 3)
(def bit-width (* numb-bits n-in-items))

(def initial-input
  (repeat n-in-items (quot numb-max 2)))

(defn gen-ins
  []
  (repeatedly n-in-items #(util/rand-int 0 numb-max)))

(defn active-columns-at
  [r t]
  (reduce (fn [s col]
            (if (contains? (:active-history col) t)
              (conj s (:id col)) s))
          #{} (:columns r)))

(def efn
  (enc/juxtapose-encoder
   (enc/linear-number-encoder numb-bits numb-on-bits numb-domain)))

(def spec {:ncol 200
           :input-size bit-width
           :potential-radius (quot bit-width 2)
           :global-inhibition true
           :stimulus-threshold 2
           :duty-cycle-period 600})

(deftest pooling-test
  (let [ncol (:ncol spec)
        r (p/region spec)
        r1k (reduce (fn [r in] (p/pooling-step r in true))
                    r
                    (map efn (repeatedly 1000 gen-ins)))]
    
    (testing "Spatial pooler column activation is distributed and moderated."
      (let [noverlaps (map (comp count :overlap-history) (:columns r1k))]
        (is (every? pos? noverlaps)
            "All columns have overlapped with input at least once."))
      (let [nactive (map (comp count :active-history) (:columns r1k))]
        (is (pos? (util/quantile nactive 0.8))
            "At least 20% of columns have been active."))
      (let [nactive-ts (for [t (range 900 1000)]
                         (count (active-columns-at r1k t)))]
        (is (every? #(< % (* ncol 0.6)) nactive-ts)
            "Inhibition limits active columns in each time step."))
      (let [nsyns (map (comp count :connected :in-synapses) (:columns r1k))]
        (is (>= (apply min nsyns) 1)
            "All columns have at least one connected input synapse."))
      (let [bs (map :boost (:columns r1k))]
        (is (== 1.0 (util/quantile bs 0.3))
            "At least 30% of columns are unboosted.")))

    (testing "Spatial pooler acts as a Locality Sensitive Hashing function."
      (let [in (repeat n-in-items 50)
            in-far (mapv (partial + 25) in)
            in-near (mapv (partial + 10) in)
            in-nearer (mapv (partial + 4) in)
            ac (:active-columns (p/pooling-step r1k (efn in) true))
            acfr (:active-columns (p/pooling-step r1k (efn in-far) true))
            acnr (:active-columns (p/pooling-step r1k (efn in-near) true))
            acnrr (:active-columns (p/pooling-step r1k (efn in-nearer) true))]
        (is (> (count (set/intersection ac acnrr))
               (* (count ac) 0.5))
            "Minor noise leads to a majority of columns remaining active.")
        (is (< (count (set/intersection ac acnr))
               (count (set/intersection ac acnrr)))
            "Increasing noise level reduces similarity of active column set - near")
        (is (< (count (set/intersection ac acfr))
               (count (set/intersection ac acnr)))
            "Increasing noise level reduces similarity of active column set - far")))))
