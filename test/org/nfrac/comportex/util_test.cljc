(ns org.nfrac.comportex.util-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]
            #?(:clj [clojure.test :as t
                     :refer (is deftest testing run-tests)]
               :cljs [cljs.test :as t
                      :refer-macros (is deftest testing run-tests)])))


(def aligned-values
  [[[2 2 2] [1 2 3 4 5]] ;; 0
   [[] []] ;; 1
   [[5] [1 3 4]] ;; 2
   [[10 8 2] [5 15 17 18]]]) ;; 3


(def unaligned-values
  [[[2 2 2] [[1] [0 1] [0 1]]] ;; 0
   [[] []] ;; 1
   [[5] [[1 3 4]]] ;; 2
   [[10 8 2] [[5] [5 7] [0]]]]) ;; 3


(deftest unalign-indices-test
  (dotimes [i (count aligned-values)]
    (let [in (nth aligned-values i)
          [_ expected] (nth unaligned-values i)
          actual (apply util/unalign-indices in)]
      (is (= expected actual)))))

(deftest align-indices-test
  (dotimes [i (count unaligned-values)]
    (let [in (nth unaligned-values i)
          [_ expected] (nth aligned-values i)
          actual (apply util/align-indices in)]
      (is (= expected actual)))))
