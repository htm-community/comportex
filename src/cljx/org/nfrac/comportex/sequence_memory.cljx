(ns org.nfrac.comportex.sequence-memory
  "Sequence Memory. Learning on distal dendrite segments.

   **Argument name conventions:**

   * `col` -- a column id, an integer index in the region.
   * `ci` -- a cell id, an integer index in the column.
   * `si` -- a segment id, an integer index in the cell.
   * `cell-id` -- a vector `[col ci]`.
   * `seg-path` -- a vector `[col ci si]`.

   * `ac` -- the set of ids of active cells.
   * `pc` -- the set of ids of predictive cells.
   * `tpc` -- the set of ids of temporal pooling cells.
   * `lc` -- the set of ids of learnable cells (winner cells).
   * `a-cols` -- the set of ids of active columns.
   * `acbc` -- active cells by column, keyed by column id.
   * `syns` -- incoming synapses as a map from source id to permanence.
"
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util
             :refer [count-filter remap]]
            [clojure.set :as set]))

(def sm-parameter-defaults
  "Default parameter specification map for sequence memory.

   * `lateral-synapses?` - whether distal synapses can grow laterally
     to other cells in this layer.

   * `extra-distal-size` - size of bit field available to grow distal
     synapses to, not including this layer's own cells if lateral
     connections are allowed. So this is for motor inputs from below,
     and/or top-down connections.

   * `depth` - number of cells per column.

   * `max-segments` - maximum number of segments per cell.

   * `seg-max-synapse-count` - maximum number of synapses per segment.

   * `seg-new-synapse-count` - number of synapses on a new dendrite
     segment.

   * `seg-stimulus-threshold` - number of active synapses on a
     dendrite segment required for it to become active.

   * `seg-learn-threshold` - number of active synapses on a dendrite
     segment required for it to be reinforced and extended on a
     bursting column.

   * `distal-perm-inc` - amount by which to increase synapse
     permanence when reinforcing dendrite segments.

   * `distal-perm-dec` - amount by which to decrease synapse permanence
     when reinforcing dendrite segments.

   * `distal-perm-connected` - permanence value at which a synapse is
     functionally connected. Permanence values are defined to be
     between 0 and 1.

   * `distal-perm-init` - permanence value for new synapses on
     dendrite segments.

   * `distal-punish?` - whether to negatively reinforce synapses on
     segments incorrectly predicting activation."
  {:lateral-synapses? true
   :extra-distal-size 0
   :depth 8
   :max-segments 5
   :seg-max-synapse-count 22
   :seg-new-synapse-count 15
   :seg-stimulus-threshold 12
   :seg-learn-threshold 8
   :distal-perm-inc 0.10
   :distal-perm-dec 0.10
   :distal-perm-connected 0.50
   :distal-perm-init 0.11
   :distal-punish? true
   })

;;; ## Activation

(defn segment-activation
  "Returns the number of active cells to which the synapses are
   connected, i.e. where synapse permanence is equal to or greater than
   `pcon`."
  [syns ac pcon]
  (count-filter (fn [[id p]]
                  (and (>= p pcon)
                       (ac id)))
                syns))

(defn cell-active-segments
  "Returns a seq of the segments in the cell with activation at or
   above the activation threshold `th`, only considering synapses with
   permanence values at or above `pcon`. Each segment has its
   activation level (number of active segments) added under key
   `:activation` and its index under key `:segment-idx`."
  [cell-segs ac th pcon]
  (keep-indexed (fn [si syns]
                  (let [act (segment-activation syns ac pcon)]
                    (when (>= act th)
                      {:activation act :segment-idx si})))
                cell-segs))

(defn cell-depolarisation
  "Returns the degree of cell depolarisation, i.e. the number of
   active distal dendrite segments, in a map keyed by cell id."
  [seg-exc act-th]
  (->> seg-exc
       (keep (fn [[k n]]
               (when (>= n act-th)
                 (let [[col ci _] k]
                   [col ci]))))
       (frequencies)))

(defn active-cells-by-column
  "Finds the active cells grouped by their column id. Returns a map
   from (the active) column ids to sub-keys `:cell-ids` (a sequence of
   cell ids in the column) and `:bursting?` (true if the feed-forward
   input was unpredicted and so all cells become active).

  * `a-cols` - the set of active column ids.

  * `pcbc` - the sets of predicted cell ids from the previous iteration
    in a map keyed by column id.

  * `ctpcbc` - the set of continuing temporal pooling cells in a map
    keyed by column id. These override any predicted cells in the
    column."
  [a-cols pcbc ctpcbc depth]
  (let [go-cbc (merge pcbc ctpcbc)]
    (->> a-cols
         (map (fn [col]
                ;; note that a TP column can be "bursting" here i.e. unpredicted
                (let [burst? (not (pcbc col))
                      go-cell-ids (go-cbc col)
                      cell-ids (or (seq (go-cbc col))
                                   (map #(vector col %) (range depth)))]
                  [col {:cell-ids cell-ids :bursting? burst?}])))
         (into {}))))

;;; ## Learning

(defn most-active-segment
  "Returns the index of the segment in the cell having the most active
   synapses, together with its number of active synapses, in a map
   with keys `:segment-idx` and `:activation`. If no segments exist,
   then `:segment-idx` is nil and `:activation` is zero."
  [cell-segs ac pcon]
  (let [acts (cell-active-segments cell-segs ac 0 pcon)]
    (if (seq acts)
      (apply max-key :activation acts)
      ;; no segments exist
      {:segment-idx nil
       :activation 0.0})))

(defn best-matching-segment-and-cell
  "Finds the segment on given cells having the most active synapses,
   even if their permanence is below the normal connected threshold.
   There must be at least `:seg-learn-threshold` synapses (note that
   this is lower than the usual `:seg-stimulus-threshold`). Returns
   indices of the segment and its containing cell in a map with keys
   `:segment-idx` and `:cell-id`. If no such segments exist, returns
   the cell with the fewest segments, and `:segment-idx` nil."
  [distal-sg cell-ids ac spec]
  (let [maxs (map (fn [cell-id]
                    (let [segs (p/cell-segments distal-sg cell-id)]
                      (assoc (most-active-segment segs ac 0.0)
                        :cell-id cell-id)))
                  cell-ids)
        best (apply max-key :activation maxs)]
    (if (>= (:activation best)
            (:seg-learn-threshold spec))
      best
      ;; no sufficient activation, return random cell
      {:cell-id (util/rand-nth cell-ids)})))

(defn new-segment-id
  "Returns a segment index on the cell at which to grow a new segment.
   It may refer to the end of the existing vector to append to it, or
   it may refer to an existing segment that is to be culled before the
   new one grows. If the maximum number of segments has been reached,
   an existing one is chosen to be replaced based on having the fewest
   connected synapses, or fewest synapses to break ties."
  [distal-sg col ci spec]
  (let [max-segs (:max-segments spec)
        max-syns (:seg-max-synapse-count spec)
        min-syns (:seg-learn-threshold spec)
        pcon (:distal-perm-connected spec)
        segs (p/cell-segments distal-sg [col ci])]
    (if (>= (count segs) max-segs)
      ;; select the one with fewest connected, or fewest synapses, or first
      (apply min-key (fn [si]
                       (let [syns (nth segs si)
                             n-conn (count-filter #(>= % pcon) (vals syns))]
                         (+ (* n-conn max-syns)
                            (count syns)
                            (/ si (count segs)))))
             (range (count segs)))
      ;; have not reached limit; append
      (count segs))))

(defn- segment-new-synapse-cell-ids
  [seg host-col lc n]
  (let [seg-cols (->> (keys seg)
                      (map first)
                      (into #{host-col}))]
    (->> lc
         (remove (fn [[col _]] (seg-cols col)))
         (util/shuffle)
         (take n))))

(defn grow-new-segment
  "Adds a new segment on the cell with synapses to a selection of the
   learn cells from previous time step, unless there are too few to
   meet the minimum threshold."
  [distal-sg col ci lc spec]
  (let [n (:seg-new-synapse-count spec)
        min-syns (:seg-learn-threshold spec)
        si (new-segment-id distal-sg col ci spec)
        syn-cell-ids (segment-new-synapse-cell-ids {} col lc n)]
    (if (< (count syn-cell-ids) min-syns)
      distal-sg
      ;; clear out any existing synapses first
      (let [seg-path [col ci si]
            osyns (p/in-synapses distal-sg seg-path)]
        (cond-> distal-sg
                (seq osyns) (p/disj-synapses seg-path (keys osyns))
                true (p/conj-synapses seg-path syn-cell-ids
                                      (:distal-perm-init spec)))))))

(defn segment-extend
  [distal-sg seg-path ac lc spec]
  (let [col (first seg-path)
        seg (p/in-synapses distal-sg seg-path)
        na (segment-activation seg ac 0.0) ;; include disconnected
        n (- (:seg-new-synapse-count spec) na)]
    (if (pos? n)
      (let [ids (segment-new-synapse-cell-ids seg col lc n)]
        (p/conj-synapses distal-sg seg-path ids (:distal-perm-init spec)))
      distal-sg)))

(defn segment-reinforce
  [sg seg-path ac pinc pdec]
  (p/reinforce-in-synapses sg seg-path (constantly false) ac pinc pdec))

(defn segment-punish
  [sg seg-path ac pdec]
  (p/reinforce-in-synapses sg seg-path ac (constantly false) 0.0 pdec))

(defn learn-on-segment
  [distal-sg col ci si bursting? prev-lc prev-ac spec]
  (let [pinc (:distal-perm-inc spec)
        pdec (:distal-perm-dec spec)]
    (if si
      ;; there is a matching segment, reinforce and/or extend it
      (cond-> distal-sg
              true (segment-reinforce [col ci si] prev-ac pinc pdec)
              bursting? (segment-extend [col ci si] prev-ac prev-lc spec))
      ;; no matching segment, create a new one
      (grow-new-segment distal-sg col ci prev-lc spec))))

(defn punish-cell
  [distal-sg col ci prev-ac th pcon pdec]
  (let [cell-segs (p/cell-segments distal-sg [col ci])
        asegs (cell-active-segments cell-segs prev-ac th pcon)]
    (reduce (fn [sg seg]
              (let [si (:segment-idx seg)]
                (segment-punish sg [col ci si] prev-ac pdec)))
            distal-sg asegs)))

(defn punish
  "Punish segments which predicted activation on cells which did
   not become active. Ignore any which are still predictive."
  [layer ac prev-ac pc prev-pc spec]
  (let [th (:seg-stimulus-threshold spec)
        pcon (:distal-perm-connected spec)
        pdec (:distal-perm-dec spec)
        bad-cells (set/difference prev-pc
                                  ac
                                  pc)]
    (assoc layer :distal-sg
           (reduce (fn [sg [col ci]]
                     (punish-cell sg col ci prev-ac th pcon pdec))
                   (:distal-sg layer)
                   bad-cells))))

(defn column-learning-segments
  [distal-sg cell-ids col-tpc bursting? prev-ac spec]
  (if col-tpc
    ;; continuing temporal pooling cell - choose a segment for the one cell
    (list (best-matching-segment-and-cell
           distal-sg [col-tpc] prev-ac spec))
    (if bursting?
      ;;bursting column - choose a segment and cell
      (list (best-matching-segment-and-cell
             distal-sg cell-ids prev-ac spec))
      ;; predicted column - all active cells become learning cells
      (let [pcon (:distal-perm-connected spec)]
        (for [cell-id cell-ids
              :let [cell-segs (p/cell-segments distal-sg cell-id)]]
          (assoc (most-active-segment cell-segs prev-ac pcon)
            :cell-id cell-id))))))

(defn learn
  [layer acbc ac burst-cols prev-ac pc prev-pc ctpcbc]
  (let [spec (:spec layer)
        distal-sg (:distal-sg layer)
        prev-lc (:learn-cells layer)
        layer0 (assoc layer :learn-cells #{} :learn-segments {})]
    (cond->
     (reduce-kv (fn [lyr col {col-ac :cell-ids}]
                  (let [bursting? (burst-cols col)
                        col-tpc (first (ctpcbc col))
                        scs (column-learning-segments
                             distal-sg col-ac col-tpc bursting? prev-ac spec)]
                    ;; there can be multiple learning cells per column:
                    (reduce (fn [lyr {si :segment-idx
                                     [_ ci] :cell-id}]
                              (-> lyr
                                  (update-in [:distal-sg] learn-on-segment
                                             col ci si bursting? prev-lc prev-ac
                                             spec)
                                  (update-in [:learn-cells] conj [col ci])
                                  (update-in [:learn-segments] assoc [col ci] si)))
                            lyr scs)))
                layer0 acbc)
     ;; allow this phase of learning as an option
     (:distal-punish? spec)
     (punish ac prev-ac pc prev-pc spec))))

;;; ## Orchestration

(defrecord LayerOfCells
    [spec topology distal-sg
     burst-cols active-cells learn-cells signal-cells
     tp-cells pred-cells prior-pred-cells
     distal-exc]
  p/PLayerOfCells
  (layer-step
    [this a-cols tp-cols extra-distal learn?]
    (let [;; continuing temporal pooling cells, by column
          ctpcbc (-> (group-by first tp-cells)
                     (select-keys tp-cols))
          acbc (let [pcbc (util/group-by-sets first pred-cells)]
                 (active-cells-by-column a-cols pcbc ctpcbc (:depth spec)))
          bcols (set (keep (fn [[i m]] (when (:bursting? m) i)) acbc))
          ac (set (mapcat :cell-ids (vals acbc)))
          tpc (set (mapcat (comp :cell-ids acbc) tp-cols))
          signal-ac (set (mapcat :cell-ids
                                 (vals (apply dissoc acbc bcols))))
          seg-exc (syn/excitations distal-sg ac)
          cell-exc (cell-depolarisation seg-exc (:seg-stimulus-threshold spec))
          pc (set (keys cell-exc))]
      (cond->
       (assoc this
         :active-cells ac
         :burst-cols bcols
         :signal-cells signal-ac
         :tp-cells tpc
         :pred-cells pc
         :prior-pred-cells pred-cells
         :distal-exc cell-exc)
       learn? (learn acbc ac bcols active-cells pc pred-cells ctpcbc))))
  (layer-depth [_]
    (:depth spec))
  (bursting-columns [this]
    burst-cols)
  (active-cells [this]
    (:active-cells this))
  (learnable-cells [this]
    learn-cells)
  (signal-cells [this]
    (:signal-cells this))
  (temporal-pooling-cells [this]
    tp-cells)
  (predictive-cells [this]
    pred-cells)
  (prior-predictive-cells [this]
    prior-pred-cells)
  (depolarisation [this]
    distal-exc)
  p/PParameterised
  (params [_]
    spec))

(defn layer-of-cells
  [spec]
  (let [spec (merge sm-parameter-defaults spec)
        col-dim (:column-dimensions spec)
        col-topo (topology/make-topology col-dim)
        n-cols (p/size col-topo)
        depth (:depth spec)
        max-segs (:max-segments spec)
        max-syns (:seg-max-synapse-count spec)
        pcon (:distal-perm-connected spec)
        distal-sg (syn/synapse-graph-by-segments
                   n-cols depth max-segs pcon max-syns true)]
    (map->LayerOfCells
     {:spec spec
      :topology col-topo
      :distal-sg distal-sg
      :burst-cols #{}
      :active-cells #{}
      :learn-cells #{}
      :signal-cells #{}
      :tp-cells #{}
      :pred-cells #{}
      :prior-pred-cells #{}
      :distal-exc {}
      })))
