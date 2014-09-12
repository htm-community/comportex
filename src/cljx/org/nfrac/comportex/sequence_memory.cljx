(ns org.nfrac.comportex.sequence-memory
  "Sequence Memory as in the CLA (not temporal pooling!)."
  (:require [org.nfrac.comportex.util :as util :refer [count-filter]]
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
     when reinforcing dendrite segments.

   * `punish?` - whether to negatively reinforce synapses on segments
     incorrectly predicting activation."
  {:depth 8
   :new-synapse-count 15
   :activation-threshold 12
   :min-threshold 8
   :initial-perm 0.11
   :connected-perm 0.50
   :permanence-inc 0.10
   :permanence-dec 0.10
   :punish? true
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
  "Returns the number of active cells to which the segment is
   connected, i.e. where synapse permanence is equal to or greater than
   `pcon`."
  [seg active-cells pcon]
  (count-filter (fn [[id p]]
                  (and (>= p pcon)
                       (active-cells id)))
                (:synapses seg)))

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
  "Returns logical true if the cell has any active dendrite segments."
  [cell active-cells spec]
  (let [act-th (:activation-threshold spec)
        pcon (:connected-perm spec)]
    (loop [segs (:segments cell)]
      (when-let [seg (first segs)]
        (if (>= (segment-activation seg active-cells pcon)
                act-th)
          true
          (recur (next segs)))))))

(defn column-predictive-cells
  [col active-cells spec]
  (keep (fn [cell]
          (when (cell-predictive? cell active-cells spec)
            (:id cell)))
        (:cells col)))

(defn predictive-cells
  "Returns all cell ids that are in the predictive state, in a map
   grouped and keyed by column id."
  [rgn active-cells]
  (->> (:columns rgn)
       (keep (fn [col]
               (let [cs (column-predictive-cells col active-cells (:spec rgn))]
                 (when (seq cs)
                   [(:id col) cs]))))
       (into {})))

(defn predicted-bit-votes
  "Returns a map from input bit index to the number of connections to
   it from columns in the predictive state. `pc` is the predictive
   cells given as a map keyed by column id."
  [rgn pc]
  (let [pcids (keys pc)
        columns (:columns rgn)]
    (->> pcids
         (reduce (fn [m cid]
                   (let [col (columns cid)
                         syns (:connected (:in-synapses col))]
                     (reduce (fn [m i]
                               (assoc! m i (inc (get m i 0))))
                             m (keys syns))))
                 (transient {}))
         (persistent!))))

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

  * `pred-cells` - the set of predicted cell ids from the previous
    iteration in a map keyed by column id.

  * `tp-cols` - the set of temporal pooling column ids.

  * `learn-cells` - the set of learn-state cells from the previous
    step, i.e. the active cells but with bursting columns having a
    single representative cell. These are the ones that continue to be
    active in temporal pooling."
  [rgn active-columns pred-cells tp-cols learn-cells]
  (let [tpc (-> (group-by (comp tp-cols first) learn-cells)
                (dissoc nil))
        pred-on-cells (merge pred-cells tpc)]
    (->> active-columns
         (map (fn [i]
                (let [col (nth (:columns rgn) i)
                      pcids (pred-on-cells i)
                      burst? (empty? pcids)
                      cids (if burst? (map :id (:cells col)) pcids)]
                  [i {:cell-ids cids :bursting? burst?}])))
         (into {}))))

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
  "Finds the segment having the most active synapses within `cells`,
   even if their permanence is below the normal connected threshold.
   There must be at least `min-threshold` synapses (note that this is
   lower than the usual `activation-threshold`). Returns indices of
   the segment and its containing cell in a map with keys
   `:segment-idx` and `:cell-id`.

   If no such segments exist in `cells`, returns the cell with the
   fewest segments, and `:segment-idx` nil."
  [cells active-cells spec]
  (let [th (:min-threshold spec)
        maxs (map (fn [cell]
                    (assoc (most-active-segment cell active-cells 0.0)
                      :cell-id (:id cell)))
                  cells)
        best (apply max-key :activation maxs)]
    (if (>= (:activation best) th)
      best
      ;; no sufficient activation, return cell with fewest segments
      {:cell-id (:id (apply min-key (comp count :segments) cells))})))

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
  "Adds a new segment to the cell with synapses to a selection of the
   learn cells from previous time step, unless there are too few to
   meet the minimum threshold."
  [cell learn-cells spec]
  (let [[column-id _] (:id cell)
        n (:new-synapse-count spec)
        seg0 {:synapses {}}
        seg (grow-new-synapses seg0 column-id learn-cells n spec)]
    (if (< (count (:synapses seg))
           (:min-threshold spec))
      cell
      (update-in cell [:segments] conj seg))))

(defn segment-extend
  [seg cid active-cells learn-cells spec]
  (let [na (segment-activation seg active-cells 0.0) ;; include disconnected
        n (- (:new-synapse-count spec) na)]
    (grow-new-synapses seg cid learn-cells n spec)))

(defn segment-cull
  "Remove synapses with zero permanence from the segment."
  [seg]
  (let [syns (:synapses seg)
        new-syns (remove (comp zero? val) syns)]
    (if (< (count new-syns) (count syns))
      (assoc seg :synapses (into {} new-syns))
      seg)))

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

(defn punish
  "Punish segments which predicted activation on cells which did
   not become active."
  [rgn active-cells prev-cells prev-pred-cells]
  (let [bad-cells (set/difference prev-pred-cells
                                  active-cells)
        spec (:spec rgn)]
    (reduce (fn [r cell-id]
              (let [[cid idx] cell-id]
                (update-in r [:columns cid :cells idx]
                           cell-punish prev-cells spec)))
            rgn bad-cells)))

(defn column-learning-segments
  [col bursting? cell-ids prev-active spec]
  (if bursting?
    (list (best-matching-segment-and-cell (:cells col) prev-active spec))
    ;; predicted column - all active cells become learning cells
    (let [pcon (:connected-perm spec)]
      (for [cell-id cell-ids
            :let [[_ idx] cell-id
                  cell (nth (:cells col) idx)]]
        (assoc (most-active-segment cell prev-active pcon)
          :cell-id cell-id)))))

(defn cell-cull-segments
  "Removes any segments having less than the minimum number of
   synapses required for activation."
  [cell spec]
  (let [th (:min-threshold spec)
        csegs (remove (fn [s] (< (count (:synapses s)) th))
                      (:segments cell))]
    (if (< (count csegs)
           (count (:segments cell)))
      (assoc cell :segments (vec csegs))
      cell)))

(defn cell-learn
  ""
  [cell seg-idx bursting? learn-cells prev-ac spec]
  (let [[cid _] (:id cell)]
    (if seg-idx
      ;; there is a matching segment, reinforce and/or extend it
      (update-in cell [:segments seg-idx]
                 (fn [seg]
                   (cond-> seg
                           true (segment-cull)
                           true (segment-reinforce prev-ac spec)
                           bursting? (segment-extend cid prev-ac learn-cells
                                                     spec))))
      ;; no matching segment, create a new one
      ;; (also remove any unused ones)
      (-> cell
          (cell-cull-segments spec)
          (grow-new-segment learn-cells spec)))))

(defn learn
  [rgn acbc ac burst-cols prev-ac prev-pc]
  (let [learn-cells (:learn-cells rgn #{})
        spec (:spec rgn)
        rgn0 (assoc rgn :learn-cells #{})]
    (cond->
     (reduce-kv (fn [r cid acm]
                  (let [col (get-in r [:columns cid])
                        bursting? (burst-cols cid)
                        scs (column-learning-segments
                             col bursting? (:cell-ids acm) prev-ac
                             spec)]
                    (reduce (fn [r {seg-idx :segment-idx
                                    [_ idx] :cell-id}]
                              (-> r
                                  (update-in [:columns cid :cells idx]
                                             cell-learn seg-idx bursting? learn-cells
                                             prev-ac spec)
                                  (update-in [:learn-cells] conj [cid idx])))
                            r scs)))
                rgn0 acbc)
     ;; allow this phase of learning as an option
     (:punish? spec)
     (punish ac prev-ac prev-pc))))

;; ## Orchestration

(defn sequence-memory-step
  "Given a set of active columns (from the spatial pooling step),
   performs an iteration of the CLA sequence memory algorithm:

   * determines the new set of active cells (using also the set of
     predictive cells from the previous iteration) and stores it in
     `:active-cells`.
   * determines the set of _bursting_ columns (indicating
     unpredicted inputs) and stores it in `:bursting-columns`.
   * determines the set of predictive cells and stores it in
     `:predictive-cells`.
   * determines the subset of active cells from non-bursting columns
     and stores it in `:signal-cells`.
   * if `learn?`, performs learning by forming and updating lateral
     connections (synapses on dendrite segments)."
  [rgn active-columns learn?]
  (let [prev-ac (:active-cells rgn #{})
        prev-pc (:predictive-cells rgn #{})
        prev-pcbc (:predictive-cells-by-column rgn {})
        tp-cols (set (keys (:temporal-pooling-scores rgn {})))
        prev-lc (if learn? (:learn-cells rgn #{}) (:signal-cells rgn #{}))
        acbc (active-cells-by-column rgn active-columns prev-pcbc tp-cols prev-lc)
        burst-cols (set (keep (fn [[i m]] (when (:bursting? m) i)) acbc))
        new-ac (set (mapcat :cell-ids (vals acbc)))
        signal-ac (set (mapcat :cell-ids
                               (vals (apply dissoc acbc burst-cols))))
        pcbc (predictive-cells rgn new-ac)
        pc (set (mapcat val pcbc))]
    (cond->
     (assoc rgn
       :active-cells new-ac
       :bursting-columns burst-cols
       :signal-cells signal-ac
       :predictive-cells pc
       :predictive-cells-by-column pcbc
       :prev-predictive-cells-by-column prev-pcbc)
     learn? (learn acbc new-ac burst-cols prev-ac prev-pc))))