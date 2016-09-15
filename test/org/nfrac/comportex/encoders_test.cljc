(ns org.nfrac.comportex.encoders-test
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [clojure.set :as set]
            [clojure.spec.test :as stest]
            [clojure.test.check.clojure-test :as ctcc]
            [clojure.test :as t
             :refer (is deftest testing run-tests)]))

(alter-var-root #'ctcc/*report-shrinking* (constantly true))
(alter-var-root #'ctcc/*report-trials* (constantly ctcc/trial-report-periodic))

(alias 'stc 'clojure.spec.test.check)

(deftest encode-decode-test
  (let [e (enc/category-encoder [10] [:a :b :c])]
    (is (= :a (:value (first (p/decode e (frequencies (p/encode e :a)) 1)))))
    (is (= :b (:value (first (p/decode e (frequencies (p/encode e :b)) 1)))))
    (is (= :c (:value (first (p/decode e (frequencies (p/encode e :c)) 1)))))))


(deftest coordinate-encoder-test
  (let [radius 80
        e (enc/coordinate-encoder [1000] 40 [1 1] [radius radius])
        coords [0 10 30 90 270]
        bitsets (zipmap coords
                        (map #(set (p/encode e [%]))
                             coords))
        overlap (fn [s1 s2] (count (set/intersection s1 s2)))]
    (is (> (overlap (bitsets 0) (bitsets 10))
           (overlap (bitsets 0) (bitsets 30))))
    (is (> (overlap (bitsets 10) (bitsets 0))
           (overlap (bitsets 10) (bitsets 30))))
    (is (> (overlap (bitsets 30) (bitsets 10))
           (overlap (bitsets 30) (bitsets 90))))
    (is (> (overlap (bitsets 90) (bitsets 30))
           (overlap (bitsets 90) (bitsets 270))))))

(def opts {::stc/opts {:num-tests 100}})

(deftest exercise-encoders-test
  (stest/instrument
   (concat (stest/enumerate-namespace 'org.nfrac.comportex.protocols)
           (stest/enumerate-namespace 'org.nfrac.comportex.encoders)))
  ;; generate random encoders with random parameters and encode random inputs!
  (-> `p/encode
      (stest/check opts)
      (stest/summarize-results))
  (stest/unstrument))

(comment
 ;; this is kind of the same thing but is faster, not sure why:
 (let [e-gen (s/gen ::p/encoder)]
   (doseq [e (gen/sample e-gen 100)
           :let [_ (prn e)
                 inval-gen (p/input-generator e)]
           inval (gen/sample inval-gen 20)]
     (p/encode e inval))))
