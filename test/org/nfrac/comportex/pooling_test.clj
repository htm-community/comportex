(ns org.nfrac.comportex.pooling-test
  (:use clojure.test)
  (:require (org.nfrac.comportex [pooling :as p]
                                 [sequence-memory :as sm])))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(comment
  (require 'org.nfrac.comportex.pooling-test :reload-all)
  (in-ns 'org.nfrac.comportex.pooling-test)
  (use 'clojure.pprint)
  (use 'clojure.repl)
  
  (def r (p/region (assoc p/spatial-pooler-defaults
                     :ncol 20
                     :input-size 40
                     :potential-radius 10
                     :active-per-inh-area 3
                     :stimulus-threshold 2
                     :duty-cycle-period 20)))

  (def ins (map (fn [t] (into (sorted-set)
                             (concat (remove neg? (range (- t 10) 40 10))
                                     (remove neg? (range (- t 11) 40 10)))))
                (range 40)))

  (pprint ins)

  (def r-ts (reductions (fn [r in] (p/pooling-step r in))
                        r ins))

  (map :overlap-history (:columns (last r-ts)))
  (map :active-history (:columns (last r-ts)))
  (map :boost (:columns (last r-ts)))

  (pprint (last r-ts))

  (map :active-columns r-ts)

  (def r-ts (reductions (fn [r in] (p/pooling-step r in))
                        r (apply concat (repeat 10 ins))))

  (map println (partition 40 40 (map (comp count :active-columns) r-ts)))

  (map #(p/mean (map :boost (:columns %))) r-ts)

  (map #(p/mean (map (comp count :connected :in-synapses) (:columns %))) r-ts)
  
  )
