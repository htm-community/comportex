(ns org.nfrac.comportex.fancy-generators
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.layer :as layer]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [com.gfredericks.test.chuck :as chuck]
            [com.gfredericks.test.chuck.generators :as gen']))

(defn layer-of-cells-gen
  []
  (gen'/for [params (s/gen ::layer/params)
             :let [n-in (reduce * (:input-dimensions params))]
             bits (s/gen (s/every (s/int-in 0 n-in) :distinct true))
             sbits (gen'/subsequence bits)]
     (-> (layer/layer-of-cells params)
         (p/layer-activate bits sbits)
         (p/layer-learn)
         (p/layer-depolarise #{} #{} #{}))))

(defn layer-activate-args-gen
  []
  (gen'/for [layer (layer-of-cells-gen)
             :let [params (p/params layer)
                   n-in (reduce * (:input-dimensions params))]
             bits (s/gen (s/every (s/int-in 0 n-in) :distinct true))
             sbits (gen'/subsequence bits)]
    [layer
     bits
     sbits]))

(def fancy-gens
  {::layer/layer-of-cells layer-of-cells-gen
   ::p/layer-activate-args layer-activate-args-gen})

#_
(doseq [[i args] (map-indexed vector (gen/sample ((-> fancy-gens ::p/layer-activate-args)) 200))]
  (println i #_(p/params (first args)) (rest args))
  (s/explain ::p/layer-activate-args args))
