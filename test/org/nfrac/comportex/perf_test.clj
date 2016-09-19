(ns org.nfrac.comportex.perf-test
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topography :as topo]
            [org.nfrac.comportex.util :as util]
            [clojure.test.check.random :as random]
            [org.nfrac.comportex.demos.isolated-1d :as demoi1d]
            [org.nfrac.comportex.demos.directional-steps-1d :as demo1d]
            [org.nfrac.comportex.demos.coordinates-2d :as demo2d]
            [org.nfrac.comportex.demos.sensorimotor-1d :as demo2layer]
            [criterium.core :as crit]
            [clojure.test :as t :refer (is deftest testing run-tests)]))

;; TODO this sucks

(deftest perf-creation-global-test
  (let [info "[1000] global, 30% potential, creation time"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (demoi1d/build (assoc demoi1d/params :ff-potential-radius 1.0))))))

(deftest perf-creation-local-1d-test
  (let [info "[800] local, radius 0.2 * 30% potential, creation time"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (demo1d/build)))))

(defn perf-test-50*
  [htm inseq]
  (let [[warmups test-ins] (split-at 50 (take 100 inseq))
        m1 (reduce cx/htm-step htm warmups)]
    (crit/quick-bench
     (reduce cx/htm-step m1 test-ins))))

(deftest perf-global-test
  (let [info "[1000] global, 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demo1d/build (assoc demo1d/params
                                          :spatial-pooling :standard
                                          :ff-potential-radius 1.0))
                     (demo1d/input-seq)))))

(deftest perf-local-1d-test
  (let [info "[800] local, radius 0.2 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demo1d/build demo1d/params)
                     (demo1d/input-seq)))))

(deftest perf-global-2d-test
  (let [info "[20 50] global, radius 0.2 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demo2d/build demo2d/params)
                     (demo2d/input-seq)))))

(deftest perf-global-1d-2layer-test
  (let [info "[800] * [800] global, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demo2layer/build demo2layer/params)
                     (demo2layer/input-seq)))))

(deftest perf-inh-test
  (let [topo (topo/make-topography [20 50])
        n (topo/size topo)
        n-on (util/round (* n 0.02))
        inh-radius 15
        inh-base-dist 1
        exc (into {}
                  (map (fn [rng]
                         (let [[rng1 rng2] (random/split rng)
                               x (util/rand rng1 0 5.0)]
                           [(util/rand-int rng2 n)
                            (* x x)]))) ;; skewed distribution of excitations
                  (random/split-n (random/make-random 42)
                                  (* 0.2 n))) ;; 20% are over stimulus threshold
        info "[20 50], radius 0.2, local inhibition"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (inh/inhibit-locally exc topo inh-radius inh-base-dist n-on)))
    (let [info "[20 50], radius 0.2, global inhibition"]
      (testing info
        (println (str (newline) info))
        (crit/quick-bench
         (inh/inhibit-globally exc n-on))))))
