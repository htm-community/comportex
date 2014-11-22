(ns org.nfrac.comportex.perf-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.demos.directional-steps-1d :as demo1d]
            [org.nfrac.comportex.demos.isolated-2d :as demo2d]
            #+clj [clojure.core.async :as async :refer [<!!]]
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
           (demo1d/n-region-model
            1 (assoc demo1d/spec :ff-potential-radius 1.0)))))))

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
      (let [in-c (demo1d/world)
            m1 (let [m (demo1d/n-region-model
                        1 (assoc demo1d/spec :global-inhibition? true
                                 :ff-potential-radius 1.0))]
                 (reduce p/htm-step m
                         (repeatedly 50 #(<!! in-c))))
            test-inputs (repeatedly 50 #(<!! in-c))]
        (crit/quick-bench
         (do (util/set-seed! 0)
             (reduce p/htm-step m1 test-inputs)))))))

#+clj
(deftest perf-local-1d-test
  (util/set-seed! 0)
  (let [info "[1000] local, radius 0.1 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (let [in-c (demo1d/world)
            m1 (let [m (demo1d/n-region-model 1)]
                 (reduce p/htm-step m
                         (repeatedly 50 #(<!! in-c))))
            test-inputs (repeatedly 50 #(<!! in-c))]
        (crit/quick-bench
         (do (util/set-seed! 0)
             (reduce p/htm-step m1 test-inputs)))))))

#+clj
(deftest perf-local-2d-test
  (util/set-seed! 0)
  (let [info "[20 50] local, radius 0.2 * 30% potential, 50 steps"]
    (testing info
      (println (str (newline) info))
      (let [in-c (demo2d/world)
            m1 (let [m (demo2d/n-region-model 1)]
                 (reduce p/htm-step m
                         (repeatedly 50 #(<!! in-c))))
            test-inputs (repeatedly 50 #(<!! in-c))]
        (crit/quick-bench
         (do (util/set-seed! 0)
             (reduce p/htm-step m1 test-inputs)))))))

#+clj
(deftest perf-inh-test
  (util/set-seed! 0)
  (let [topo (topology/make-topology [20 50])
        n (p/size topo)
        inh-radius 15
        inh-base-dist 1
        exc (->> (repeatedly #(let [x (util/rand 0 5.0)] (* x x))) ;; skew
                 (zipmap (util/sample (* 0.2 n) ;; 20% over stimulus threshold
                                      (range n))))
        info "[20 50], radius 0.2, local inhibition"]
    (testing info
      (println (str (newline) info))
      (crit/quick-bench
       (inh/inhibit-locally exc topo inh-radius inh-base-dist)))
    (let [info "[20 50], radius 0.2, global inhibition"]
      (testing info
        (println (str (newline) info))
        (crit/quick-bench
         (inh/inhibit-globally exc 0.02 n))))))
