(ns org.nfrac.comportex.topology-test
  (:require [org.nfrac.comportex.topology :as topology
             :refer [combined-dimensions]]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]
            #+cljs [cemerick.cljs.test :as t
                    :refer-macros (is deftest testing run-tests)]))

(deftest combined-dimensions-test
  (testing "Combining dimensions"
    (is (= [15] (combined-dimensions [5] [10])))
    (is (= [7 5] (combined-dimensions [5 5] [10])))
    (is (= [5 7 5] (combined-dimensions [5 5 5] [10])))
    (is (= [7 25] (combined-dimensions [5 5 5] [10 5])))
    (is (= [15 5 5] (combined-dimensions [5 5 5] [10 5 5])))))
