(ns org.nfrac.comportex.spec-tests
  (:require [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.spatial-pooling-test]
            [org.nfrac.comportex.sequence-memory-test]
            [org.nfrac.comportex.encoders-test]
            [org.nfrac.comportex.topology-test]
            [clojure.spec.test :as stest]
            [clojure.test :as t
             :refer (is deftest testing run-tests)]
            [org.nfrac.comportex.repl]))

(org.nfrac.comportex.repl/truncate-large-data-structures)

(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.cells))

(run-tests 'org.nfrac.comportex.spatial-pooling-test
           'org.nfrac.comportex.sequence-memory-test
           'org.nfrac.comportex.encoders-test
           'org.nfrac.comportex.topology-test)
