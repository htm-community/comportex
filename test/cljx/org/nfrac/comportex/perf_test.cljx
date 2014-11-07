(ns org.nfrac.comportex.perf-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.demos.simple-sentences :as demog]
            [org.nfrac.comportex.demos.isolated-1d :as demo1d]
            [org.nfrac.comportex.demos.isolated-2d :as demo2d]
            #+clj [criterium.core :as crit]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]))


#+clj
(deftest perf-creation-global-test
  (util/set-seed! 0)
  (let [info "[1000] global, 30% potential, creation time"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (do (util/set-seed! 0)
           (demog/n-region-model 1))))))

#+clj
(deftest perf-creation-local-2d-test
  (util/set-seed! 0)
  (let [info "[20 50] local, radius 0.2 * 30% potential, creation time"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (do (util/set-seed! 0)
           (demo2d/n-region-model 1))))))

#+clj
(deftest perf-global-test
  (util/set-seed! 0)
  (let [info "[1000] global, 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (let [m1 (->> (demog/n-region-model 1)
                    (iterate p/htm-step)
                    (take 50)
                    (last))]
        (crit/quick-bench
         (do (util/set-seed! 0)
             (->> (iterate p/htm-step m1)
                  (take 50)
                  (last))))))))

#+clj
(deftest perf-local-1d-test
  (util/set-seed! 0)
  (let [info "[1000] local, radius 0.1 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (let [m1 (->> (demo1d/n-region-model 1)
                    (iterate p/htm-step)
                    (take 50)
                    (last))]
        (crit/quick-bench
         (do (util/set-seed! 0)
             (->> (iterate p/htm-step m1)
                  (take 50)
                  (last))))))))

#+clj
(deftest perf-local-2d-test
  (util/set-seed! 0)
  (let [info "[20 50] local, radius 0.2 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (let [m1 (->> (demo2d/n-region-model 1)
                    (iterate p/htm-step)
                    (take 50)
                    (last))]
        (crit/quick-bench
         (do (util/set-seed! 0)
             (->> (iterate p/htm-step m1)
                  (take 50)
                  (last))))))))
