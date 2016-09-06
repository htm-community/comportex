(ns org.nfrac.comportex.spec-tests
  (:require [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.spatial-pooling-test]
            [org.nfrac.comportex.sequence-memory-test]
            [org.nfrac.comportex.encoders-test]
            [org.nfrac.comportex.topography-test]
            [clojure.spec.test :as stest]
            [clojure.test :as t
             :refer (is deftest testing run-tests)]
            [org.nfrac.comportex.repl]))

(org.nfrac.comportex.repl/truncate-large-data-structures)

(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.protocols))
(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.layer))
(stest/instrument (stest/enumerate-namespace 'org.nfrac.comportex.encoders))

(run-tests 'org.nfrac.comportex.spatial-pooling-test
           'org.nfrac.comportex.sequence-memory-test
           'org.nfrac.comportex.topography-test)
