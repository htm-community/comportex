(ns org.nfrac.comportex.sequence-memory
  (:require (org.nfrac.comportex [pooling :as pooling])
            [clojure.data.generators :as gen]
            [clojure.set :as set]))

(def sequence-memory-defaults
  {:depth 8
   :init-segment-count 1
   :init-synapse-count 15
   :new-synapse-count 15
   :activation-threshold 10
   :min-threshold 5
   :initial-perm 0.11
   :connected-perm 0.50
   :permanence-inc 0.10
   :permanence-dec 0.10
   })

;; CONSTRUCTION

(defn random-segment
  [i column-id {:as spec :keys [ncol depth init-synapse-count initial-perm]}]
  (let [cell-ids (->> (repeatedly #(vector (gen/uniform 0 ncol)
                                           (gen/uniform 0 depth)))
                      (remove (fn [[c _]] (= c column-id)))
                      (distinct)
                      (take init-synapse-count))
        syns (into {} (map vector cell-ids (repeat initial-perm)))]
    {:synapses syns
     }))

(defn init-cell
  [idx column-id {:as spec :keys [ncol depth init-segment-count]}]
  {:id [column-id idx]
   :segments (mapv random-segment (range init-segment-count)
                   (repeat column-id) (repeat spec))
   })

(defn column-with-sequence-memory
  [col {:as spec :keys [depth]}]
  (assoc col
    :cells (mapv init-cell (range depth) (repeat (:id col)) (repeat spec))))

(defn with-sequence-memory
  [rgn spec]
  (let [fullspec (merge (:spec rgn) spec)]
    (assoc rgn
      :columns (mapv column-with-sequence-memory
                     (:columns rgn) (repeat fullspec)))))

;; ACTIVATION

(defn segment-activation
  [seg active-cells pcon]
  (count (filterv (fn [[id p]]
                    (and (>= pcon p)
                         (active-cells id)))
                  (:synapses seg))))

(defn cell-active-segments
  [cell active-cells th pcon]
  (filter (fn [seg]
            (>= (segment-activation seg active-cells pcon)
                th))
          (:segments cell)))

(defn cell-predictive?
  [cell active-cells rgn]
  (let [act-th (:activation-threshold (:spec rgn))
        pcon (:connected-perm (:spec rgn))]
    (seq (cell-active-segments cell act-th pcon))))

(defn column-predictive-cells
  [col active-cells rgn]
  (keep (fn [cell]
          (when (cell-predictive? cell active-cells rgn)
            (:id cell)))
        (:cells col)))

(defn active-cells-by-column
  [rgn active-columns prev-active-cells]
  (->> active-columns
       (map (fn [i]
              (let [col (nth (:columns rgn) i)
                    pcids (column-predictive-cells col prev-active-cells rgn)
                    burst? (empty? pcids)
                    cids (if burst? (map :id (:cells col)) pcids)]
                [i {:cell-ids cids :bursting? burst?}])))
       (into {})))

;; LEARNING

(defn segment-update-synapses
  [seg]
  )

(defn cell-update-synapses
  [cell]
  )

(defn column-update-all-synapses
  [col]
  )

(defn update-all-synapses
  [rgn]
  )

;; ORCHESTRATION

(defn sequence-memory-step
  [rgn active-columns t]
  (let [prev-ac (:active-cells rgn #{})
        acbc (active-cells-by-column rgn active-columns prev-ac)
        new-ac (set (mapcat :cell-ids acbc))]
    (-> rgn
        (update-all-synapses)
        (assoc :active-cells new-ac))))
