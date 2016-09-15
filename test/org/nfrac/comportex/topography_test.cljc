(ns org.nfrac.comportex.topography-test
  (:require [org.nfrac.comportex.topography :as topography
             :refer [combined-dimensions]]
            [clojure.test :as t
             :refer (is deftest testing run-tests)]))

(deftest combined-dimensions-test
  (testing "Combining dimensions"
    (is (= [15] (combined-dimensions [5] [10]))
        "1D & 1D")
    (is (= [7 5] (combined-dimensions [5 5] [10]))
        "2D & 1D compatible")
    (is (= [42] (combined-dimensions [7 5] [7]))
        "2D & 1D incompatible")
    (is (= [14 5] (combined-dimensions [7 5] [7 5]))
        "2D & 2D compatible")
    (is (= [10 7] (combined-dimensions [5 7] [7 5]))
        "2D & 2D incompatible, then 2D & 1D compatible")
    (is (= [7 5 5] (combined-dimensions [5 5 5] [50]))
        "3D & 1D compatible")
    (is (= [280] (combined-dimensions [5 7 7] [7 5]))
        "3D & 1D incompatible, 2D & 1D incompatible")
    (is (= [7 5 5] (combined-dimensions [5 5 5] [10 5]))
        "3D & 2D compatible")
    (is (= [7 25] (combined-dimensions [5 5 5] [5 10]))
        "3D & 2D incompatible, 2D & 2D incompatible, then 2D & 1D compatible")
    (is (= [15 5 5] (combined-dimensions [5 5 5] [10 5 5]))
        "3D & 3D compatible")
    (is (= [6 5 20] (combined-dimensions [5 5 20] [5 4 5]))
        "3D & 3D incompatible, then 3D & 2D compatible"))
  #_(is (= [6 7 5 7 5] (combined-dimensions [5 7 5 7 5] [7 5 7 5]))
        "5D & 4D compatible"))
