(ns org.nfrac.comportex.spec-checks
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
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

(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.cells))
(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.protocols))

(deftest cells-light-fns-test
  (-> `[cells/segment-activation
        cells/best-matching-segment
        cells/best-segment-excitations-and-paths
        cells/best-by-column
        cells/total-excitations
        cells/column-active-cells
        cells/select-active-cells
        cells/new-segment-id
        cells/segment-new-synapse-source-ids]
      (stest/check opts)
      (stest/summarize-results)))

;; requires synapse graph generator
#_
(deftest cells-sg-fns-test
  (-> `[cells/select-winner-cell-and-seg
        cells/select-winner-cells-and-segs
        cells/learning-updates
        cells/learn-distal
        cells/punish-distal
        cells/compute-distal-state
        cells/layer-learn-proximal]
      (stest/check (assoc-in opts [::stc/opts :num-tests] 50))
      (stest/summarize-results)))

;; requires layer-of-cells generator
(deftest cells-heavy-fns-test
  (->
   `[p/layer-activate]
     ;cells/spatial-pooling
     ;cells/temporal-pooling]
   (stest/check (assoc-in opts [::stc/opts :num-tests] 200))
   (stest/summarize-results)))
