(ns org.nfrac.comportex.sequence-memory
  "Sequence Memory as in the CLA (not temporal pooling!)."
  (:require [org.nfrac.comportex.util :as util
             :refer [count-filter remap]]
            [clojure.set :as set]))

(def sequence-memory-defaults
  "Default parameter specification map for sequence memory. This gets
   merged with the specification map for pooling (should use
   namespaced keywords?). Mostly based on values from NuPIC.

   * `depth` - number of cells per column.

   * `max-segments` - maximum number of segments per cell.

   * `max-synapse-count` - maximum number of synapses per segment.

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
   :max-segments 5
   :max-synapse-count 22
   :new-synapse-count 15
   :activation-threshold 12
   :min-threshold 8
   :initial-perm 0.11
   :connected-perm 0.50
   :permanence-inc 0.10
   :permanence-dec 0.10
   :punish? true
   })

;;; ## Synapse indexing

(defn cell-uidx
  [depth [cid ci]]
  (+ ci (* cid depth)))

(defn lateral-excitation-from
  [rgn depth cell-id]
  (let [uidx (cell-uidx depth cell-id)]
    (get-in rgn [::lat-index uidx])))

(defn init-lat-index
  "Builds an internal index from each cell to the set of segments
   `[cid ci si]` connected to it. This speeds up predictive cell
   calculations."
  [rgn]
  (let [spec (:spec rgn)
        depth (:depth spec)
        n-cells (* depth (:ncol spec))]
    (assoc rgn ::lat-index
           (vec (repeat n-cells #{})))))

(defn reinforce-lat-synapses
  [rgn cid ci si skip? reinforce?]
  (let [spec (:spec rgn)
        pinc (:permanence-inc spec)
        pdec (:permanence-dec spec)
        pcon (:connected-perm spec)
        syns (get-in rgn [:columns cid :cells ci :segments si :synapses])
        sg (util/group-by-maps (fn [id2 p]
                                 (if (skip? id2)
                                   :skip
                                   (if (reinforce? id2)
                                     (cond
                                      (== p 1.0) :skip
                                      (and (< p pcon)
                                           (>= p (- pcon pinc))) :promote
                                      :else :up)
                                     (cond
                                      (<= p pdec) :cull
                                      (and (>= p pcon)
                                           (< p (+ pcon pdec))) :demote
                                      :else :down))))
                               syns)
        new-syns (merge (:skip sg)
                        (remap #(min (+ % pinc) 1.0) (:up sg))
                        (remap #(min (+ % pinc) 1.0) (:promote sg))
                        (remap #(- % pdec) (:down sg))
                        (remap #(- % pdec) (:demote sg)))
        seg-path [cid ci si]
        ->uidx (partial cell-uidx (:depth spec))
        promote-uidxs (map ->uidx (keys (:promote sg)))
        demote-uidxs (map ->uidx (keys (:demote sg)))
        new-lati (-> (::lat-index rgn)
                     (util/update-each promote-uidxs #(conj % seg-path))
                     (util/update-each demote-uidxs #(disj % seg-path)))]
    (-> rgn
        (assoc-in [:columns cid :cells ci :segments si :synapses] new-syns)
        (assoc ::lat-index new-lati))))

(defn segment-reinforce
  [rgn cid ci si active-cells]
  (reinforce-lat-synapses rgn cid ci si (constantly false) active-cells))

(defn segment-punish
  [rgn cid ci si active-cells]
  (reinforce-lat-synapses rgn cid ci si active-cells (constantly false)))

(defn disj-lat-synapses
  [rgn cid ci si syn-cell-ids]
  (let [spec (:spec rgn)
        ->uidx (partial cell-uidx (:depth spec))
        uidxs (map ->uidx syn-cell-ids)
        seg-path [cid ci si]]
    (-> rgn
        (update-in [:columns cid :cells ci :segments si :synapses]
                   (fn [syns] (apply dissoc syns syn-cell-ids)))
        (update-in [::lat-index]
                   util/update-each uidxs #(disj % seg-path)))))

(defn conj-lat-synapses
  [rgn cid ci si syn-cell-ids]
  (let [spec (:spec rgn)
        pini (:initial-perm spec)
        pcon (:connected-perm spec)
        max-syns (:max-synapse-count spec)
        ->uidx (partial cell-uidx (:depth spec))
        uidxs (map ->uidx syn-cell-ids)
        seg-path [cid ci si]
        osyns (get-in rgn [:columns cid :cells ci :segments si :synapses])
        syns (merge osyns (zipmap syn-cell-ids (repeat pini)))]
    (cond->
     (assoc-in rgn [:columns cid :cells ci :segments si :synapses]
               syns)
     (>= pini pcon)
     (update-in [::lat-index]
                util/update-each uidxs #(conj % seg-path))
     ;; if too many synapses, remove those with lowest permanence
     (> (count syns) max-syns)
     (disj-lat-synapses cid ci si
                        (->> (sort-by val syns)
                             (keys)
                             (take (- (count syns) max-syns)))))))

;;; ## Construction

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
    (-> (assoc rgn
          :spec spec
          :columns (mapv column-with-sequence-memory
                         (:columns rgn) (repeat spec)))
        (init-lat-index))))

;;; ## Activation

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

(defn predictive-cells
  "Returns the set of all cell ids that are in the predictive state
   through lateral excitation."
  [rgn active-cells]
  (let [spec (:spec rgn)
        act-th (:activation-threshold spec)
        depth (:depth spec)]
    (->> active-cells
         (mapcat (partial lateral-excitation-from rgn depth))
         (frequencies)
         (keep (fn [[k n]]
                 (when (>= n act-th)
                   (let [[cid ci _] k]
                     [cid ci]))))
         (into #{}))))

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
                         syns (:connected (:ff-synapses col))]
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

  * `ctpcbc` - the set of continuing temporal pooling cells in a map
    keyed by column id. These override any predicted cells in the
    column."
  [rgn active-columns pred-cells ctpcbc]
  (let [pred-on-cells (merge pred-cells ctpcbc)]
    (->> active-columns
         (map (fn [i]
                (let [col (nth (:columns rgn) i)
                      pcids (pred-on-cells i)
                      burst? (empty? pcids)
                      cids (if burst? (map :id (:cells col)) pcids)]
                  [i {:cell-ids cids :bursting? burst?}])))
         (into {}))))

;;; ## Learning

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

(defn new-segment-id
  "Returns a segment index on the cell at which to grow a new segment.
   It may refer to the end of the existing vector to append to it, or
   it may refer to an existing segment that is to be culled before the
   new one grows. If the maximum number of segments has been reached,
   an existing one is chosen to be replaced based on having the fewest
   connected synapses, or fewest synapses to break ties."
  [rgn cid ci]
  (let [spec (:spec rgn)
        max-segs (:max-segments spec)
        max-syns (:max-synapse-count spec)
        min-syns (:min-threshold spec)
        pcon (:connected-perm spec)
        segs (get-in rgn [:columns cid :cells ci :segments])]
    (if (>= (count segs) max-segs)
      ;; select the one with fewest connected synapses or fewest synapses
      (apply min-key (fn [si]
                       (let [syns (:synapses (get segs si))]
                         (+ (count-filter #(>= % pcon) (vals syns))
                            (/ (count syns) max-syns))))
             (range (count segs)))
      ;; have not reached limit; choose any dead segment or append
      (or (some (fn [[si seg]]
                  (when (< (count (:synapses seg)) min-syns)
                    si))
                (map-indexed vector segs))
          (count segs)))))

(defn- segment-new-synapse-cell-ids
  [seg cid learn-cells n]
  (let [syns (:synapses seg)
        my-cols (->> (keys syns)
                     (map first)
                     (into #{cid}))]
    (->> learn-cells
         (remove (fn [[c _]] (my-cols c)))
         (util/shuffle)
         (take n))))

(defn new-segment
  []
  {:synapses {}})

(defn grow-new-segment
  "Adds a new segment on the cell with synapses to a selection of the
   learn cells from previous time step, unless there are too few to
   meet the minimum threshold."
  [rgn cid ci learn-cells]
  (let [spec (:spec rgn)
        n (:new-synapse-count spec)
        min-syns (:min-threshold spec)
        si (new-segment-id rgn cid ci)
        seg0 (new-segment)
        syn-cell-ids (segment-new-synapse-cell-ids seg0 cid learn-cells n)]
    (if (< (count syn-cell-ids) min-syns)
      rgn
      ;; clear out any existing synapses first
      (let [osyns (get-in rgn [:columns cid :cells ci :segments si :synapses])]
        (cond-> rgn
                (seq osyns) (disj-lat-synapses cid ci si (keys osyns))
                true (conj-lat-synapses cid ci si syn-cell-ids))))))

(defn segment-extend
  [rgn cid ci si active-cells learn-cells]
  (let [seg [:columns cid :cells ci :segments si]
        na (segment-activation seg active-cells 0.0) ;; include disconnected
        n (- (:new-synapse-count (:spec rgn)) na)]
    (if (pos? n)
      (->> (segment-new-synapse-cell-ids seg cid learn-cells n)
           (conj-lat-synapses rgn cid ci si))
      rgn)))

(defn punish-cell
  [rgn cid ci prev-cells]
  (let [spec (:spec rgn)
        th (:activation-threshold spec)
        pcon (:connected-perm spec)
        cell (get-in rgn [:columns cid :cells ci])
        asegs (cell-active-segments cell prev-cells th pcon)]
    (reduce (fn [r seg]
              (let [si (:segment-idx seg)]
                (segment-punish r cid ci si prev-cells)))
            rgn asegs)))

(defn punish
  "Punish segments which predicted activation on cells which did
   not become active."
  [rgn active-cells prev-cells prev-pred-cells]
  (let [bad-cells (set/difference prev-pred-cells
                                  active-cells)]
    (reduce (fn [r [cid ci]]
              (punish-cell r cid ci prev-cells))
            rgn bad-cells)))

(defn column-learning-segments
  [col bursting? col-tpc cell-ids prev-active spec]
  (if col-tpc
    ;; continuing temporal pooling cell - choose a segment for the one cell
    (let [[cid ci] col-tpc
          cell (get (:cells col) ci)]
      (list (best-matching-segment-and-cell [cell] prev-active spec)))
    (if bursting?
      ;;bursting column - choose a segment and cell
      (list (best-matching-segment-and-cell (:cells col) prev-active spec))
      ;; predicted column - all active cells become learning cells
      (let [pcon (:connected-perm spec)]
        (for [cell-id cell-ids
              :let [[_ idx] cell-id
                    cell (nth (:cells col) idx)]]
          (assoc (most-active-segment cell prev-active pcon)
            :cell-id cell-id))))))

(defn learn-on-segment
  [rgn cid ci si bursting? learn-cells prev-ac]
  (if si
    ;; there is a matching segment, reinforce and/or extend it
    (cond-> rgn
            true (segment-reinforce cid ci si prev-ac)
            bursting? (segment-extend cid ci si prev-ac learn-cells))
    ;; no matching segment, create a new one
    (grow-new-segment rgn cid ci learn-cells)))

(defn learn
  [rgn acbc ac burst-cols prev-ac prev-pc ctpcbc]
  (let [learn-cells (:learn-cells rgn #{})
        spec (:spec rgn)
        rgn0 (assoc rgn :learn-cells #{} :learn-segments {})]
    (cond->
     (reduce-kv (fn [r cid {col-ac :cell-ids}]
                  (let [col (get-in r [:columns cid])
                        bursting? (burst-cols cid)
                        col-tpc (first (ctpcbc cid))
                        scs (column-learning-segments
                             col bursting? col-tpc col-ac prev-ac spec)]
                    (reduce (fn [r {si :segment-idx
                                   [_ ci] :cell-id}]
                              (-> (learn-on-segment r cid ci si bursting?
                                                    learn-cells prev-ac)
                                  (update-in [:learn-cells] conj [cid ci])
                                  (update-in [:learn-segments] assoc [cid ci] si)))
                            r scs)))
                rgn0 acbc)
     ;; allow this phase of learning as an option
     (:punish? spec)
     (punish ac prev-ac prev-pc))))

;;; ## Orchestration

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
        prev-tpc (:temporal-pooling-cells rgn #{})
        tp-cols (keys (:temporal-pooling-scores rgn {}))
        ;; continuing temporal pooling cells, by column
        ctpcbc (-> (group-by first prev-tpc)
                   (select-keys tp-cols))
        acbc (active-cells-by-column rgn active-columns prev-pcbc ctpcbc)
        burst-cols (set (keep (fn [[i m]] (when (:bursting? m) i)) acbc))
        ac (set (mapcat :cell-ids (vals acbc)))
        tpc (set (mapcat (comp :cell-ids acbc) tp-cols))
        signal-ac (set (mapcat :cell-ids
                               (vals (apply dissoc acbc burst-cols))))
        pc (predictive-cells rgn ac)
        pcbc (util/group-by-sets first pc)]
    (cond->
     (assoc rgn
       :active-cells ac
       :bursting-columns burst-cols
       :signal-cells signal-ac
       :temporal-pooling-cells tpc
       :predictive-cells pc
       :predictive-cells-by-column pcbc
       :prev-predictive-cells-by-column prev-pcbc)
     learn? (learn acbc ac burst-cols prev-ac prev-pc ctpcbc))))
