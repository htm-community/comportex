(ns org.nfrac.comportex.fancy-generators
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.topography :as topo]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]
            [clojure.spec.test :as stest]
            [com.gfredericks.test.chuck :as chuck]
            [com.gfredericks.test.chuck.generators :as gen']))

(defn signal-generator
  [src-topo]
  (let [n-in (topo/size src-topo)]
    (gen'/for [bits (s/gen (s/every (s/int-in 0 n-in) :distinct true))
               sbits (gen'/subsequence bits)]
      {:bits bits
       ::layer/stable-bits sbits
       :topography src-topo})))

(defn layer-of-cells-stepped-gen
  []
  (gen'/for [layer (s/gen ::layer/layer-of-cells)
             :let [embedding (:embedding (p/params layer))]
             ff-sig (signal-generator (:ff-topo embedding))
             :let [fb-sig {:bits ()
                           :topography topo/empty-topography}]]
     (-> layer
         (p/layer-activate ff-sig)
         (p/layer-learn)
         (p/layer-depolarise fb-sig fb-sig))))

(defn layer-activate-args-gen
  []
  (gen'/for [layer (layer-of-cells-stepped-gen)
             :let [embedding (:embedding (p/params layer))]
             ff-sig (signal-generator (:ff-topo embedding))]
    [layer ff-sig]))

(def fancy-gens
  {::layer/layer-of-cells layer-of-cells-stepped-gen
   ::p/layer-activate-args layer-activate-args-gen})

#_
(doseq [[i args] (map-indexed vector (gen/sample ((-> fancy-gens ::p/layer-activate-args)) 200))]
  (println i #_(p/params (first args)) (rest args))
  (s/explain ::p/layer-activate-args args))
