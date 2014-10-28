(ns org.nfrac.comportex.perf-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.demos.simple-sentences :as demo1]
            [org.nfrac.comportex.demos.isolated-1d :as demo2]
            #+clj [criterium.core :as crit]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]))


#+clj
(deftest one-region-perf-test
  (util/set-seed! 0)
  (testing "one region, wide range of ff synapses"
    (let [m1 (-> (iterate p/htm-step (demo1/n-region-model 1))
                 (nth 50))]
      (crit/quick-bench
       (do (util/set-seed! 0)
           (->> (iterate p/htm-step m1)
                (take 50)
                (last)))))))

#+clj
(deftest two-region-perf-test
  (util/set-seed! 0)
  (testing "two regions"
    (let [m1 (-> (iterate p/htm-step (demo2/n-region-model 2))
                 (nth 50))]
      (crit/quick-bench
       (do (util/set-seed! 0)
           (->> (iterate p/htm-step m1)
                (take 50)
                (last)))))))
