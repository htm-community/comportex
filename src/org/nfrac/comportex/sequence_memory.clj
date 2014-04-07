(ns org.nfrac.comportex.sequence-memory
  (:require (org.nfrac.comportex [pooling :as pooling])
            [clojure.data.generators :as gen]
            [clojure.set :as set]))

(def sequence-memory-defaults
  {:depth 8
   :init-segment-count 0
   :new-synapse-count 15
   :activation-threshold 12
   :min-threshold 8
   :initial-perm 0.11
   :connected-perm 0.50
   :permanence-inc 0.10
   :permanence-dec 0.10
   })

;; CONSTRUCTION

;; TODO maybe don't need this, just start empty
(defn random-segment
  [i column-id {:as spec :keys [ncol depth new-synapse-count initial-perm]}]
  (let [cell-ids (->> (repeatedly #(vector (gen/uniform 0 ncol)
                                           (gen/uniform 0 depth)))
                      (remove (fn [[c _]] (= c column-id)))
                      (distinct)
                      (take new-synapse-count))
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
      :spec fullspec
      :columns (mapv column-with-sequence-memory
                     (:columns rgn) (repeat fullspec)))))

;; ACTIVATION

(defn segment-activation
  [seg active-cells pcon]
  (count (filter (fn [[id p]]
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
  [cell active-cells spec]
  (let [act-th (:activation-threshold spec)
        pcon (:connected-perm spec)]
    (seq (cell-active-segments cell active-cells act-th pcon))))

(defn column-predictive-cells
  [col active-cells spec]
  (keep (fn [cell]
          (when (cell-predictive? cell active-cells spec)
            (:id cell)))
        (:cells col)))

(defn active-cells-by-column
  [rgn active-columns prev-cells]
  (->> active-columns
       (map (fn [i]
              (let [col (nth (:columns rgn) i)
                    pcids (column-predictive-cells col prev-cells (:spec rgn))
                    burst? (empty? pcids)
                    cids (if burst? (map :id (:cells col)) pcids)]
                [i {:cell-ids cids :bursting? burst?}])))
       (into {})))

;; LEARNING

(defn most-active-segment
  "Returns the index of the segment in the cell having the most active
   synapses, followed by its number of active synapses, in a map with
   keys :segment-idx and :activation. If no segments exist,
   returns :segment-idx nil and :activation zero."
  [cell active-cells spec]
  (let [pcon (:connected-perm spec)
        acts (map-indexed (fn [i seg]
                            {:segment-idx i
                             :activation (segment-activation seg active-cells pcon)})
                          (:segments cell))]
    (if (seq acts)
      (apply max-key :activation acts)
      ;; no segments exist
      {:segment-idx nil
       :activation 0.0})))

(defn best-matching-segment-and-cell
  "Finds the segment in the column having the most active synapses,
   as long as this is at least min-threshold (note that this is lower
   than the usual activation-threshold). Returns indices of the
   segment and its containing cell in a map with keys :segment-idx
   and :cell-id.

   If no such segments exist in the column, returns the cell with the
   fewest segments, and :segment-idx nil."
  [col active-cells spec]
  (let [th (:min-threshold spec)
        maxs (map (fn [cell]
                    (assoc (most-active-segment cell active-cells spec)
                      :cell-id (:id cell)))
                  (:cells col))
        best (apply max-key :activation maxs)]
    (if (>= (:activation best) th)
      best
      ;; no sufficient activation, return cell with fewest segments
      {:cell-id (:id (apply min-key (comp count :segments) (:cells col)))})))

(defn segment-reinforce
  [seg active-cells spec]
  (let [pinc (:permanence-inc spec)
        pdec (:permanence-dec spec)
        syns (->> (:synapses seg)
                  (mapv (fn [[id p]]
                          (if (active-cells id)
                            [id (min 1.0 (+ p pinc))]
                            [id (max 0.0 (- p pdec))])))
                  (into {}))]
    (assoc seg :synapses syns)))

(defn grow-new-synapses
  [seg column-id active-cells n spec]
  (if-not (pos? n)
    seg
   (let [existing-ids (set (keys (:synapses seg)))
         cell-ids (->> active-cells
                       (remove (fn [[c _]] (= c column-id)))
                       (remove existing-ids)
                       (gen/reservoir-sample n))
         syns (map vector cell-ids (repeat (:initial-perm spec)))]
     (update-in seg [:synapses] into syns))))

(defn grow-new-segment
  [cell active-cells spec]
  (let [[column-id _] (:id cell)
        n (:new-synapse-count spec)
        seg0 {:synapses {}}
        seg (grow-new-synapses seg0 column-id active-cells n spec)]
    (update-in cell [:segments] conj seg)))

(defn segment-extend
  [seg cell active-cells spec]
  (let [pcon (:connected-perm spec)
        na (segment-activation seg active-cells pcon)
        n (- (:new-synapse-count spec) na)
        [column-id _] (:id cell)]
    (-> seg
        (segment-reinforce active-cells spec)
        (grow-new-synapses column-id active-cells n spec))))

(defn bursting-column-learn
  [col active-cells spec]
  (let [sc (best-matching-segment-and-cell col active-cells spec)
        [_ idx] (:cell-id sc)
        cell (nth (:cells col) idx)]
    (if-let [seg-idx (:segment-idx sc)]
      ;; there is a matching segment, extend it
      (update-in col [:cells idx :segments seg-idx] segment-extend cell active-cells spec)
      ;; no matching segment, create a new one
      (update-in col [:cells idx] grow-new-segment active-cells spec))))

(defn predicted-column-learn
  [col active-cells prev-cells spec]
  (let [idxs (keep (fn [[c i]]
                     (when (= c (:id col)) i))
                   active-cells)
        ;; TODO: how to choose from multiple active cells?
        ;; prefer if activated by a "learn-state" cell (not bursting)?
        idx (gen/rand-nth idxs)
        cell (nth (:cells col) idx)
        seg-idx (:segment-idx (most-active-segment cell prev-cells spec))]
    (update-in col [:cells idx :segments seg-idx]
               segment-reinforce prev-cells spec)))

(defn learn
  [rgn active-columns active-cells prev-cells burst-cols]
  (reduce (fn [r id]
            (update-in r [:columns id]
                       (fn [col]
                         (if (burst-cols id)
                           (bursting-column-learn col prev-cells (:spec rgn))
                           (predicted-column-learn col active-cells prev-cells (:spec rgn))))))
          rgn active-columns))

;; ORCHESTRATION

(defn sequence-memory-step
  [rgn active-columns]
  (let [prev-ac (:active-cells rgn #{})
        acbc (active-cells-by-column rgn active-columns prev-ac)
        new-ac (set (mapcat :cell-ids (vals acbc)))
        burst-cols (set (keep (fn [[i m]] (when (:bursting? m) i)) acbc))]
    (-> rgn
        (assoc :active-cells new-ac)
        (learn active-columns new-ac prev-ac burst-cols))))
