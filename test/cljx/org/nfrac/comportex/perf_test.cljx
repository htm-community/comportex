(ns org.nfrac.comportex.perf-test
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.demos.mixed-gaps-1d :as demo]
            [clojure.set :as set]
            #+clj [criterium.core :as crit]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]))

#+clj
(deftest sm-perf-test
  (util/set-seed! 0)
  (let [m1 (-> (iterate core/feed-forward-step (demo/n-region-model 2))
               (nth 300))]
    (crit/with-progress-reporting
      (crit/bench
       (do (util/set-seed! 0)
           (->> (iterate core/feed-forward-step m1)
                (take 50)
                (last)))))))
