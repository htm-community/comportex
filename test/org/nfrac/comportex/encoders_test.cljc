(ns org.nfrac.comportex.encoders-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            #?(:clj [clojure.test :as t
                     :refer (is deftest testing run-tests)]
               :cljs [cemerick.cljs.test :as t
                      :refer-macros (is deftest testing run-tests)])))

(def a-category-encoder (enc/category-encoder  10  [:a :b :c]))

(defn bit-list-to-bit-votes [bit-list]
  (reduce (fn [xs x] (merge-with + xs {x 1}))
   {}
   bit-list
  ))

(deftest encode-decode-test
 (is (= (:value (first (p/decode a-category-encoder (bit-list-to-bit-votes (p/encode a-category-encoder :a)) 1))) :a))
 (is (= (:value (first (p/decode a-category-encoder (bit-list-to-bit-votes (p/encode a-category-encoder :b)) 1))) :b))
 (is (= (:value (first (p/decode a-category-encoder (bit-list-to-bit-votes (p/encode a-category-encoder :c)) 1))) :c))
 )
