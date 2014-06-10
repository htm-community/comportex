(ns org.nfrac.comportex.sequence-memory
  "Sequence Memory as in the CLA (not temporal pooling!).

   One difference from the Numenta white paper is that _predictive_
   states are not calculated acrosss the whole region, only on active
   columns to determine their active cells."
  (:require [org.nfrac.comportex.util :as util]
            [clojure.set :as set]))

(def sequence-memory-defaults
  "Default parameter specification map for sequence memory. This gets
   merged with the specification map for pooling (should use
   namespaced keywords?). Mostly based on values from NuPIC.

   * `depth` - number of cells per column.

   * `new-synapse-count` - number of synapses on a new dendrite
     segment.

   * `activation-threshold` - number of active synapses on a dendrite
     segment required for it to become active.

   * `min-threshold` - number of active synapses on a dendrite segment
     required for it to be reinforced and extended on a bursting column.

   * `initial-perm` - permanence value for new synapses on dendrite
     segments.

   * `permanence-inc` - amount by which to increase synapse permanence
     when reinforcing dendrite segments.

   * `permanence-dec` - amount by which to decrease synapse permanence
     when reinforcing dendrite segments."
  {:depth 8
   :new-synapse-count 15
   :activation-threshold 12
   :min-threshold 8
   :initial-perm 0.11
   :connected-perm 0.50
   :permanence-inc 0.10
   :permanence-dec 0.10
   })

;; ## Construction

(defn init-cell
  "Constructs a cell, initially without any dendrite segments, at
   index `idx` in the column."
  [idx column-id]
  {:id [column-id idx]
   :segments []})

(defn column-with-sequence-memory
  "Constructs `depth` cells in a vector, attaching to key `:cells` in
   the column."
  [col {:as spec :keys [depth]}]
  (assoc col
    :cells (mapv init-cell (range depth) (repeat (:id col)))))

(defn with-sequence-memory
  "Takes a region `rgn` constructed as a spatial pooler, and extends
   it with sequence memory capability. That is, it adds individual
   cell representations to each column. Specifically, specification
   key `:depth` gives the number of cells in each column. Any keys
   given in the original `spec` will override the values in
   `sequence-memory-defaults`."
  [rgn]
  (let [spec (merge sequence-memory-defaults (:spec rgn))]
    (assoc rgn
      :spec spec
      :columns (mapv column-with-sequence-memory
                     (:columns rgn) (repeat spec)))))

;; ## Activation

(defn segment-activation
  [seg active-cells pcon]
  (count (filter (fn [[id p]]
                   (and (>= p pcon)
                        (active-cells id)))
                  (:synapses seg))))

(defn cell-active-segments
  "Returns a seq of the segments in the cell with activation at or
   above the activation threshold `th`, only considering synapses with
   permanence values at or above `pcon`. Each segment has its
   activation level (number of active segments) added under key
   `:activation` and its index under key `:segment-idx`."
  [cell active-cells th pcon]
  (keep-indexed (fn [i seg]
                  (let [act (segment-activation seg active-cells pcon)]
                    (when (>= act th)
                      (assoc seg :activation act :segment-idx i))))
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

(defn predictive-cells
  "Returns all cell ids that are in the predictive state, in a map
   grouped and keyed by column id. Uses the stored `:active-cells`."
  [rgn]
  (let [ac (:active-cells rgn #{})]
    (->> (:columns rgn)
         (keep (fn [col]
                 (let [cs (column-predictive-cells col ac (:spec rgn))]
                   (when (seq cs)
                     [(:id col) cs]))))
         (into {}))))

(defn predicted-bit-votes
  "Returns a map from input bit index to the number of connections to
   it from columns in the predictive state. `pc` is the predictive
   cells given as a map keyed by column id."
  [rgn pc]
  (let [pcids (keys pc)
        columns (:columns rgn)]
    (reduce (fn [inb cid]
              (let [col (columns cid)
                    syns (:connected (:in-synapses col))]
                (reduce (fn [inb i]
                          (assoc inb i
                                 (inc (inb i 0))))
                        inb (keys syns))))
            {} pcids)))

(defn predicted-bits
  "Returns the set of input bit indices which have at least `nmin`
   connections from predictive columns. `pc` is the predictive cells
   given as a map keyed by column id."
  [rgn pc nmin]
  (->> (predicted-bit-votes rgn pc)
       (keep (fn [[i votes]]
               (when (>= votes nmin) i)))
       (into #{})))

(defn active-cells-by-column
  "Finds the active cells grouped by their column id. Returns a map
   from (the active) column ids to sub-keys `:cell-ids` (a sequence of
   cell ids in the column) and `:bursting?` (true if the feed-forward
   input was unpredicted and so all cells become active).

  * `active-columns` - the set of active column ids (from the spatial
    pooling step)

  * `prev-cells` - the set of active cell ids from the previous
    iteration."
  [rgn active-columns prev-cells]
  (->> active-columns
       (map (fn [i]
              (let [col (nth (:columns rgn) i)
                    pcids (column-predictive-cells col prev-cells (:spec rgn))
                    burst? (empty? pcids)
                    cids (if burst? (map :id (:cells col)) pcids)]
                [i {:cell-ids cids :bursting? burst?}])))
       (into {})))

;; ## Learning

(defn most-active-segment
  "Returns the index of the segment in the cell having the most active
   synapses, together with its number of active synapses, in a map with
   keys `:segment-idx` and `:activation`. If no segments exist,
   then `:segment-idx` is nil and `:activation` is zero."
  [cell active-cells pcon]
  (let [acts (cell-active-segments cell active-cells 0 pcon)]
    (if (seq acts)
      (apply max-key :activation acts)
      ;; no segments exist
      {:segment-idx nil
       :activation 0.0})))

(defn best-matching-segment-and-cell
  "Finds the segment in the column having the most active synapses,
   even if their permanence is below the normal connected threshold.
   There must be at least `min-threshold` synapses (note that this is
   lower than the usual `activation-threshold`). Returns indices of
   the segment and its containing cell in a map with keys
   `:segment-idx` and `:cell-id`.

   If no such segments exist in the column, returns the cell with the
   fewest segments, and `:segment-idx` nil."
  [col active-cells spec]
  (let [th (:min-threshold spec)
        maxs (map (fn [cell]
                    (assoc (most-active-segment cell active-cells 0.0)
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
                  (map (fn [[id p]]
                         (if (active-cells id)
                           [id (min 1.0 (+ p pinc))]
                           [id (max 0.0 (- p pdec))])))
                  (into {}))]
    (assoc seg :synapses syns)))

(defn segment-punish
  [seg active-cells spec]
  (let [pdec (:permanence-dec spec)
        syns (->> (:synapses seg)
                  (map (fn [[id p]]
                         (if (active-cells id)
                           [id (max 0.0 (- p pdec))]
                           [id p])))
                  (into {}))]
    (assoc seg :synapses syns)))

(defn grow-new-synapses
  [seg column-id learn-cells n spec]
  (if-not (pos? n)
    seg
    (let [my-cols (->> (:synapses seg)
                       (keys)
                       (map first)
                       (into #{column-id}))
          cell-ids (->> learn-cells
                        (remove (fn [[c _]] (my-cols c)))
                        (util/shuffle)
                        (take n))
          syns (map vector cell-ids (repeat (:initial-perm spec)))]
      (update-in seg [:synapses] into syns))))

(defn grow-new-segment
  [cell learn-cells spec]
  (let [[column-id _] (:id cell)
        n (:new-synapse-count spec)
        seg0 {:synapses {}}
        seg (grow-new-synapses seg0 column-id learn-cells n spec)]
    (update-in cell [:segments] conj seg)))

(defn segment-extend
  [seg cell active-cells learn-cells spec]
  (let [na (segment-activation seg active-cells 0.0) ;; include disconnected
        n (- (:new-synapse-count spec) na)
        [column-id _] (:id cell)]
    ;; TODO: should reinforce all active cells or just learn cells?
    (-> seg
        (segment-reinforce active-cells spec)
        (grow-new-synapses column-id learn-cells n spec))))

(defn segment-cull
  [seg]
  (let [syns (:synapses seg)
        new-syns (remove (comp zero? val) syns)]
    (if (< (count new-syns) (count syns))
      (assoc seg :synapses (into {} new-syns))
      seg)))

(defn bursting-column-learn
  [rgn cid learn-cells prev-cells]
  (let [spec (:spec rgn)
        col (get-in rgn [:columns cid])
        ;; choose the learning segment and cell
        ;; TODO - break ties by permanence
        ;; TODO - punish other active segments in the cell/column?
        ;; prefer cells that were activated from other "learn" cells
        scl (best-matching-segment-and-cell col learn-cells spec)
        sc (if (:segment-idx scl) scl
               ;; fall back to activation from all active cells
               (best-matching-segment-and-cell col prev-cells spec))
        [_ idx] (:cell-id sc)
        cell (nth (:cells col) idx)
        c2 (if-let [seg-idx (:segment-idx sc)]
             ;; there is a matching segment, extend it
             (update-in col [:cells idx :segments seg-idx]
                        (fn [seg]
                          (-> (segment-cull seg)
                              (segment-extend cell prev-cells learn-cells spec))))
             ;; no matching segment, create a new one
             (update-in col [:cells idx] grow-new-segment learn-cells spec))]
    (-> rgn
        (assoc-in [:columns cid] c2)
        (update-in [:learn-cells] conj (:cell-id sc)))))

(defn predicted-column-learn
  [rgn cid cell-ids prev-cells]
  (let [spec (:spec rgn)
        pcon (:connected-perm spec)
        col (get-in rgn [:columns cid])
        c2 (reduce (fn [col [_ idx]]
                     ;; TODO: only the most active segment or all active segments?
                     (let [cell (get-in col [:cells idx])
                           seg-idx (-> (most-active-segment cell prev-cells spec)
                                       :segment-idx)]
                       (update-in col [:cells idx :segments seg-idx]
                                  (fn [seg]
                                    (-> (segment-cull seg)
                                        (segment-reinforce prev-cells spec))))))
                   col cell-ids)]
    (-> rgn
        (assoc-in [:columns cid] c2)
        (update-in [:learn-cells] into cell-ids))))

(defn cell-punish
  [cell prev-cells spec]
  (let [th (:activation-threshold spec)
        pcon (:connected-perm spec)
        as (cell-active-segments cell prev-cells th pcon)]
    (reduce (fn [cell seg]
              (let [i (:segment-idx seg)]
                (update-in cell [:segments i]
                           segment-punish prev-cells spec)))
            cell as)))

(defn column-punish
  [col prev-cells spec]
  (reduce (fn [col cell]
            (let [[_ idx] (:id cell)]
              (update-in col [:cells idx] cell-punish
                         prev-cells spec)))
          col (:cells col)))

(defn punish
  "Punish segments which predicted activation on columns which turned off."
  [rgn new-cells prev-cells]
  (let [cids (set (map first new-cells))
        prev-cids (set (map first prev-cells))
        off-cids (set/difference prev-cids cids)
        spec (:spec rgn)]
    (reduce (fn [r cid]
              (let [col (get-in r [:columns cid])]
                ;; TODO compare to predicted set - if not then skip
                (update-in r [:columns cid] column-punish prev-cells spec)))
            rgn off-cids)))

(defn cull-segments
  "Removes any segments with fewer than the minimum number of synapses
   required for activation."
  [rgn]
  ;; TODO
  (reduce (fn [r cid]
            r)
          rgn (:active-columns rgn)))

(defn learn
  [rgn acbc new-cells burst-cols prev-cells]
  (let [active-columns (keys acbc)
        learn-cells (:learn-cells rgn #{})
        rgn0 (assoc rgn :learn-cells #{})
        spec (:spec rgn)]
    ;; TODO: only reinforce newly active cells?
    ;; (set/difference new-cells prev-cells)
    (->
     (reduce (fn [r cid]
               (if (burst-cols cid)
                 (bursting-column-learn r cid learn-cells prev-cells)
                 (predicted-column-learn r cid (:cell-ids (acbc cid))
                                         prev-cells)))
             rgn0 active-columns)
     (punish new-cells prev-cells))))

;; ## Orchestration

(defn sequence-memory-step
  "Given a set of active columns (from the spatial pooling step),
   performs an iteration of the CLA sequence memory algorithm:

   * determines the new set of active cells (using also the set of
     active cells from the previous iteration) and stores it in
     `:active-cells`.
      * determines the set of _bursting_ columns (indicating
        unpredicted inputs) and stores it in `:bursting-columns`.
   * performs learning by forming and updating lateral
     connections (synapses on dendrite segments)."
  [rgn active-columns]
  (let [prev-ac (:active-cells rgn #{})
        acbc (active-cells-by-column rgn active-columns prev-ac)
        new-ac (set (mapcat :cell-ids (vals acbc)))
        burst-cols (set (keep (fn [[i m]] (when (:bursting? m) i)) acbc))]
    (-> rgn
        (assoc :active-cells new-ac
               :bursting-columns burst-cols)
        (learn acbc new-ac burst-cols prev-ac))))
