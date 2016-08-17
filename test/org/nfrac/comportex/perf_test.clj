(ns org.nfrac.comportex.perf-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util]
            [clojure.test.check.random :as random]
            [org.nfrac.comportex.demos.isolated-1d :as demoi1d]
            [org.nfrac.comportex.demos.directional-steps-1d :as demo1d]
            [org.nfrac.comportex.demos.coordinates-2d :as demo2d]
            [criterium.core :as crit]
            [clojure.test :as t :refer (is deftest testing run-tests)]))

;; TODO this sucks

(deftest perf-creation-global-test
  (let [info "[1000] global, 30% potential, creation time"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (demoi1d/n-region-model
        1 (assoc demoi1d/params :ff-potential-radius 1.0))))))

(deftest perf-creation-local-1d-test
  (let [info "[800] local, radius 0.2 * 30% potential, creation time"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (demo1d/n-region-model 1)))))

(defn perf-test-50*
  [htm inseq]
  (let [[warmups test-ins] (split-at 50 (take 100 inseq))
        m1 (reduce p/htm-step htm warmups)]
    (crit/quick-bench
     (reduce p/htm-step m1 test-ins))))

(deftest perf-global-test
  (let [info "[1000] global, 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demo1d/n-region-model 1 (assoc demo1d/params
                                                     :spatial-pooling :standard
                                                     :ff-potential-radius 1.0))
                     (demo1d/input-seq)))))

(deftest perf-local-1d-test
  (let [info "[800] local, radius 0.2 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demo1d/n-region-model 1 demo1d/params)
                     (demo1d/input-seq)))))

(deftest perf-global-2d-test
  (let [info "[20 50] global, radius 0.2 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demo2d/n-region-model 1 demo2d/params)
                     (demo2d/input-seq)))))

(deftest perf-global-1d-2r-test
  (let [info "[1000] * [400] global, 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (perf-test-50* (demoi1d/n-region-model 2 demoi1d/params)
                     (demoi1d/input-seq)))))

(deftest perf-inh-test
  (let [topo (topology/make-topology [20 50])
        n (p/size topo)
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
