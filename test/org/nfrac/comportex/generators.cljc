(ns org.nfrac.comportex.generators
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [com.gfredericks.test.chuck :as chuck]
            [com.gfredericks.test.chuck.generators :as gen']))

(defn layer-of-cells-gen
  []
  (gen'/for [params (s/gen ::cells/params)
             :let [n-in (reduce * (:input-dimensions params))]
             bits (s/gen (s/every (s/int-in 0 n-in) :kind set?))
             sbits (gen'/subset bits)]
     (-> (cells/layer-of-cells params)
         (p/layer-activate bits sbits)
         (p/layer-learn)
         (p/layer-depolarise #{} #{} #{}))))

(defn layer-activate-args-gen
  []
  (gen'/for [layer (layer-of-cells-gen)
             :let [params (p/params layer)
                   n-in (reduce * (:input-dimensions params))]
             bits (s/gen (s/every (s/int-in 0 n-in) :kind set?))
             sbits (gen'/subset bits)]
    [layer
     bits
     sbits]))

(def fancy-gens
  {::cells/layer-of-cells layer-of-cells-gen
   ::p/layer-activate-args layer-activate-args-gen})
