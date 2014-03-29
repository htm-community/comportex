(ns org.nfrac.comportex.sequence-memory
  (:require (org.nfrac.comportex [pooling :as pooling])
            [clojure.data.generators :as gen]
            [clojure.set :as set]))

(def sequence-memory-defaults
  {:depth 8
   :initial-perm 0.11
   :connected-perm 0.50
   :min-threshold 8
   :new-synapse-count 15
   :permanence-inc 0.10
   :permanence-dec 0.10
   :activation-threshold 12
   })

(defn segment
  [id {:as spec}]
  ;; how to initialise?
  {:synapses {}
   :active? false
   }
  )

(defn cell
  [id column-id {:as spec :keys [ncol depth segments]}]
  {:id id
   :column-id column-id ;; need this?
   :segments (mapv segment (range segments) (repeat spec))
   :active? false
   :predictive? false
   })

(defn column-with-sequence-memory
  [col {:as spec :keys [depth]}]
  (assoc col
    :cells (mapv cell (range depth) (repeat spec))))

(defn with-sequence-memory
  [rgn spec]
  (assoc rgn
    :columns (mapv column-with-sequence-memory (:columns rgn))))
