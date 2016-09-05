(ns org.nfrac.comportex.spec-checks
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.fancy-generators :refer [fancy-gens]]
            [clojure.spec.test :as stest]
            [clojure.test.check.clojure-test :as ctcc]
            [clojure.test :as t
             :refer (is deftest testing run-tests)]
            [org.nfrac.comportex.repl]))

(org.nfrac.comportex.repl/truncate-large-data-structures)

(alter-var-root #'ctcc/*report-shrinking* (constantly true))
(alter-var-root #'ctcc/*report-trials* (constantly ctcc/trial-report-periodic))

(alias 'stc 'clojure.spec.test.check)
(def opts {::stc/opts {:num-tests 50}
           :gen fancy-gens})

(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.layer))
(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.protocols))

(deftest layer-light-fns-test
  (-> `[layer/segment-activation
        layer/best-matching-segment
        layer/best-segment-excitations-and-paths
        layer/best-by-column
        layer/total-excitations
        layer/column-active-cells
        layer/select-active-cells
        layer/new-segment-id
        layer/segment-new-synapse-source-ids]
      (stest/check opts)
      (stest/summarize-results)))

;; requires synapse graph generator
#_
(deftest layer-sg-fns-test
  (-> `[layer/select-winner-cell-and-seg
        layer/select-winner-cells-and-segs
        layer/learning-updates
        layer/learn-distal
        layer/punish-distal
        layer/compute-distal-state
        layer/layer-learn-proximal]
      (stest/check (assoc-in opts [::stc/opts :num-tests] 50))
      (stest/summarize-results)))

;; requires layer-of-cells generator
(deftest layer-heavy-fns-test
  (->
   `[p/layer-activate]
     ;layer/spatial-pooling
     ;layer/temporal-pooling]
   (stest/check (assoc-in opts [::stc/opts :num-tests] 100))
   (stest/summarize-results)))
