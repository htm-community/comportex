(ns org.nfrac.comportex.encoders-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [clojure.set :as set]
            #?(:clj [clojure.test :as t
                     :refer (is deftest testing run-tests)]
               :cljs [cemerick.cljs.test :as t
                      :refer-macros (is deftest testing run-tests)])))


(deftest encode-decode-test
  (let [e (enc/category-encoder 10 [:a :b :c])]
    (is (= :a (:value (first (p/decode e (frequencies (p/encode e :a)) 1)))))
    (is (= :b (:value (first (p/decode e (frequencies (p/encode e :b)) 1)))))
    (is (= :c (:value (first (p/decode e (frequencies (p/encode e :c)) 1))))))
  )

(deftest coordinate-encoder-test
  (let [e (enc/coordinate-encoder [1000] 40)
        coords [0 10 30 90 270]
        radius 80
        bitsets (zipmap coords
                        (map #(set (p/encode e {:coord [%] :radii [radius]}))
                             coords))
        overlap (fn [s1 s2] (count (set/intersection s1 s2)))]
    (is (> (overlap (bitsets 0) (bitsets 10))
           (overlap (bitsets 0) (bitsets 30))))
    (is (> (overlap (bitsets 10) (bitsets 0))
           (overlap (bitsets 10) (bitsets 30))))
    (is (> (overlap (bitsets 30) (bitsets 10))
           (overlap (bitsets 30) (bitsets 90))))
    (is (> (overlap (bitsets 90) (bitsets 30))
           (overlap (bitsets 90) (bitsets 270))))
    ))
