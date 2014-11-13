(ns org.nfrac.comportex.cells
  "Cell activation and sequence memory.

   **Argument name conventions:**

   * `col` -- a column id, an integer index in the region.
   * `ci` -- a cell id, an integer index in the column.
   * `si` -- a segment id, an integer index in the cell.
   * `cell-id` -- a vector `[col ci]`.
   * `seg-path` -- a vector `[col ci si]`.

   * `ff-bits` -- the set of indices of any active feed-forward input bits.
   * `ac` -- the set of ids of active cells.
   * `pc` -- the set of ids of predictive cells.
   * `tpc` -- the set of ids of temporal pooling cells.
   * `lc` -- the set of ids of learnable cells (winner cells).
   * `a-cols` -- the set of ids of active columns.
   * `acbc` -- active cells by column, keyed by column id.
   * `syns` -- incoming synapses as a map from source id to permanence.
"
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.columns :as columns]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util
             :refer [count-filter remap round]]
            [clojure.set :as set]))

(def parameter-defaults
  "Default parameter specification map.

   * `input-dimensions` - size of input bit grid as a vector, one
     dimensional `[size]`, two dimensional `[width height]`, etc.

   * `column-dimensions` - size of column field as a vector, one
     dimensional `[size]` or two dimensional `[width height]`.

   * `ff-potential-radius` - range of potential feed-forward synapse
     connections, as a fraction of the longest single dimension in the
     input space.

   * `ff-init-frac` - fraction of inputs within radius that will be
     part of the initially connected set.

   * `ff-perm-inc` - amount to increase a synapse's permanence value
     by when it is reinforced.

   * `ff-perm-dec` - amount to decrease a synapse's permanence value
     by when it is not reinforced.

   * `ff-perm-connected` - permanence value at which a synapse is
     functionally connected. Permanence values are defined to be
     between 0 and 1.

   * `ff-perm-init` - initial permanence values on new synapses.

   * `ff-stimulus-threshold` - minimum number of active input
     connections for a column to be _overlapping_ the input (i.e.
     active prior to inhibition). This parameter is tuned at run time
     to target `activation-level` when `global-inhibition?` is false.

   * `ff-grow-up-to-count` - target number of active synapses; active columns
     grow new synapses to inputs to reach this each time step.

   * `ff-max-synapse-count` - maximum number of synapses on the column.

   * `boost-overlap-duty-ratio` - when a column's overlap frequency is
     below this proportion of the _highest_ of its neighbours, its
     feed-forward synapses are boosted.

   * `boost-active-duty-ratio` - when a column's activation frequency is
     below this proportion of the _highest_ of its neighbours, its
     boost factor is increased.

   * `duty-cycle-period` - number of time steps to consider when
     updating column boosting measures. Also the period between such
     updates.

   * `max-boost` - ceiling on the column boosting factor used to
     increase activation frequency.

   * `lateral-synapses?` - whether distal synapses can connect
     laterally to other cells in this layer.

   * `use-feedback?` - whether distal synapses can connect to top-down
     feedback cells.

   * `distal-motor-dimensions` - defines bit field available for
     feed-forward motor input to distal synapses.

   * `distal-topdown-dimensions` - defines bit field available for
     top-down feedback to distal synapses.

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
     segments incorrectly predicting activation.

   * `activation-level` - fraction of columns that can be
     active (either locally or globally); inhibition kicks in to
     reduce it to this level.

   * `global-inhibition?` - whether to use the faster global algorithm
     for column inhibition (just keep those with highest overlap
     scores), or to apply local inhibition (only within a column's
     neighbours).

   * `inhibition-base-distance` - the distance in columns within which
     a cell *will always* inhibit neighbouring cells with lower
     excitation. Ignored if `global-inhibition?` is true.

   * `distal-vs-proximal-weight` - scaling to apply to the number of
     active distal synapses (on the winning segment) before adding to
     the number of active proximal synapses, when selecting active
     cells. Set to zero to disable ``prediction-assisted'' activation.

   * `spontaneous-activation?` - if true, cells may become active with
     sufficient distal synapse excitation, even in the absence of any
     proximal synapse excitation.

   * `temporal-pooling-decay` - multiplier on the continuing
     excitation score of temporal pooling cells; as this reduces a
     temporal pooling cell is more likely to be interrupted by
     competing cells.

   * `temporal-pooling-amp` - multiplier on the initial excitation
     score of temporal pooling cells; this increases the probability
     that TP cells will remain active."
  {:input-dimensions [:define-me!]
   :column-dimensions [2048]
   :ff-potential-radius 0.3
   :ff-init-frac 0.3
   :ff-perm-inc 0.05
   :ff-perm-dec 0.005
   :ff-perm-connected 0.2
   :ff-perm-init 0.16
   :ff-stimulus-threshold 3
   :ff-grow-and-die? false
   :ff-grow-up-to-count 15
   :ff-max-synapse-count 1000
   :boost-overlap-duty-ratio 0.001
   :boost-active-duty-ratio 0.001
   :duty-cycle-period 1000
   :max-boost 3.0
   :lateral-synapses? true
   :use-feedback? false
   :distal-motor-dimensions [0]
   :distal-topdown-dimensions [0]
   :depth 16
   :max-segments 5
   :seg-max-synapse-count 22
   :seg-new-synapse-count 15
   :seg-stimulus-threshold 12
   :seg-learn-threshold 8
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-connected 0.20
   :distal-perm-init 0.16
   :distal-punish? true
   :activation-level 0.02
   :global-inhibition? false
   :inhibition-base-distance 1
   :distal-vs-proximal-weight 0
   :spontaneous-activation? false
;   :temporal-pooling-decay 0.9
;   :temporal-pooling-amp 1.1
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
  "Returns the degree of cell depolarisation: the greatest number of
   active synapses on any one distal dendrite segment, as long as it
   is above the threshold `th`, in a map keyed by cell id."
  [seg-exc th]
  (->> seg-exc
       (reduce-kv (fn [m k n]
                    (if (< n th)
                      m                 ;; below threshold, ignore.
                      (let [id (pop k)] ;; seg-id to cell-id: [col ci _]
                        (assoc! m id (max n
                                          ;; also add 1 for every extra segment
                                          (inc (get m id 0)))))))
                  (transient {}))
       (persistent!)))

(defn total-excitations
  "Combine the proximal and distal excitations in a map of column id
   to excitation, being the sum of proximal and (weighted) distal
   values for the most active cell in the column. See
   `cell-depolarisation`. Normally only columns with proximal input
   are considered, but if `spontaneous-activation?` is true, this is
   not enforced."
  [prox-exc distal-exc-by-col distal-weight spontaneous-activation?]
  (if (zero? distal-weight)
    prox-exc
    (->> (if spontaneous-activation?
           (merge (zipmap (keys distal-exc-by-col) (repeat 0))
                  prox-exc)
           prox-exc)
         (reduce-kv (fn [m col pexc]
                      (if-let [dexcs (vals (distal-exc-by-col col))]
                        (let [dexc (* distal-weight (apply max dexcs))]
                          (assoc! m col (+ dexc pexc)))
                        ;; no distal excitation
                        (assoc! m col pexc)))
                    (transient {}))
         (persistent!))))

(defn select-active-cells
  "Finds the active cells grouped by their column id. Returns a map
   with keys `:active-cells-by-col` (a map from column id to cell ids)
   and `:burst-cols` (the set of bursting column ids)."
  [prox-exc distal-exc topo inh-radius spec]
  (let [distal-exc-by-col (util/group-by-maps (fn [[col _] _] col)
                                              distal-exc)
        exc (total-excitations prox-exc distal-exc-by-col
                               (:distal-vs-proximal-weight spec)
                               (:spontaneous-activation? spec))
        level (:activation-level spec)
        a-cols (if (:global-inhibition? spec)
                 (inh/inhibit-globally exc level (p/size topo))
                 (inh/inhibit-locally exc topo inh-radius
                                      (:inhibition-base-distance spec)))
        depth (:depth spec)]
    (loop [cols a-cols
           acbc (transient {})
           b-cols (transient #{})]
      (if-let [col (first cols)]
        (if-let [dm (distal-exc-by-col col)]
          (let [[cell dexc] (apply max-key val dm)]
            ;; TODO find multiple equal winners?
            (recur (next cols)
                   (assoc! acbc col [cell])
                   b-cols))
          ;; no distal excitation, so bursting
          (recur (next cols)
                 (assoc! acbc col (map vector (repeat col)
                                       (range depth)))
                 (conj! b-cols col)))
        ;; finished
        {:active-cells-by-col (persistent! acbc)
         :burst-cols (persistent! b-cols)}))))

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
   `:segment-idx` and `:cell-id`. If no such segments exist, returns a
   random cell, and `:segment-idx` nil. If there are no active cells,
   we can not learn but still want to present a consistent
   representation, so always return the first cell. This is a sequence
   reset mechanism."
  [distal-sg cell-ids ac spec]
  ;; special case if no activity at all previously: sequence reset.
  (if (empty? ac)
    {:cell-id (first cell-ids)}
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
        {:cell-id (util/rand-nth cell-ids)}))))

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
        segs (syn/cell-segments-raw distal-sg [col ci])]
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

(defn segment-new-synapse-cell-ids
  "Returns a collection of up to n cell ids chosen from the learnable
   cells `lc`. May be less than `n` if the random samples have
   duplicates or some already exist on the segment, or if there are
   fewer than `n` learnable cells. Connections to the host column are
   not allowed. However, connections can be made to a cell even if
   there is already a connection to another cell in the same column."
  [seg host-col lc-vec n]
  (when (seq lc-vec)
    (->> lc-vec
         (util/sample n)
         (distinct)
         (remove (fn [cell-id]
                   (or (seg cell-id)
                       (= host-col (first cell-id))))))))

(defn grow-new-segment
  "Adds a new segment on the cell with synapses to a selection of the
   learn cells from previous time step, unless there are too few to
   meet the minimum threshold."
  [distal-sg col ci lc-vec spec]
  (let [n (:seg-new-synapse-count spec)
        min-syns (:seg-learn-threshold spec)
        si (new-segment-id distal-sg col ci spec)
        syn-cell-ids (segment-new-synapse-cell-ids {} col lc-vec n)]
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
  [distal-sg seg-path ac lc-vec spec]
  (let [col (first seg-path)
        seg (p/in-synapses distal-sg seg-path)
        na (segment-activation seg ac 0.0) ;; include disconnected
        n (- (:seg-new-synapse-count spec) na)]
    (if (pos? n)
      (let [ids (segment-new-synapse-cell-ids seg col lc-vec n)]
        (p/conj-synapses distal-sg seg-path ids (:distal-perm-init spec)))
      distal-sg)))

(defn segment-reinforce
  [sg seg-path ac pinc pdec]
  (p/reinforce-in-synapses sg seg-path (constantly false) ac pinc pdec))

(defn segment-punish
  [sg seg-path ac pdec]
  (p/reinforce-in-synapses sg seg-path ac (constantly false) 0.0 pdec))

(defn learn-on-segment
  [distal-sg col ci si bursting? prev-lc-vec prev-ac spec]
  (let [pinc (:distal-perm-inc spec)
        pdec (:distal-perm-dec spec)]
    (if si
      ;; there is a matching segment, reinforce and/or extend it
      (cond-> distal-sg
              true (segment-reinforce [col ci si] prev-ac pinc pdec)
              bursting? (segment-extend [col ci si] prev-ac prev-lc-vec spec))
      ;; no matching segment, create a new one
      (grow-new-segment distal-sg col ci prev-lc-vec spec))))

(defn learn-distal
  [distal-sg lsegs b-cols prior-ac prior-lc spec]
  (let [prior-lc-vec (vec prior-lc)]
    (reduce-kv (fn [sg [col ci] si]
                 (learn-on-segment sg col ci si (b-cols col) prior-lc-vec
                                   prior-ac spec))
               distal-sg
               lsegs)))

(defn punish-cell
  [distal-sg col ci prev-ac th pcon pdec]
  (let [cell-segs (p/cell-segments distal-sg [col ci])
        asegs (cell-active-segments cell-segs prev-ac th pcon)]
    (reduce (fn [sg seg]
              (let [si (:segment-idx seg)]
                (segment-punish sg [col ci si] prev-ac pdec)))
            distal-sg asegs)))

(defn punish-distal
  "Punish segments which predicted activation on cells which did
   not become active. Ignore any which are still predictive."
  [distal-sg ac prev-ac pc prev-pc spec]
  (let [th (:seg-stimulus-threshold spec)
        pcon (:distal-perm-connected spec)
        pdec (:distal-perm-dec spec)
        bad-cells (set/difference prev-pc
                                  ac
                                  pc)]
    (reduce (fn [sg [col ci]]
              (punish-cell sg col ci prev-ac th pcon pdec))
            distal-sg
            bad-cells)))

(defn select-learning-segs
  [acbc b-cols distal-sg prior-ac spec]
  (let [pcon (:distal-perm-connected spec)]
    (loop [acbc (seq acbc)
           lc (transient #{})
           lsegs (transient {})]
      (if-let [x (first acbc)]
        (let [[col cells] x]
          (if (b-cols col)
            ;; bursting column - choose a learning segment and cell
            (let [sc (best-matching-segment-and-cell distal-sg cells prior-ac
                                                     spec)
                  cell (:cell-id sc)]
              (recur (next acbc)
                     (conj! lc cell)
                     (assoc! lsegs cell (:segment-idx sc))))
            ;; predicted column - the active cell is the learning cell
            (let [cell (first cells)
                  cell-segs (p/cell-segments distal-sg cell)
                  sc (most-active-segment cell-segs prior-ac pcon)
                  ;; if not depolarised by current active cells
                  ;; (temporal pooling?) then fall back to bursting-like
                  sc (if (:segment-idx sc)
                       sc
                       (most-active-segment cell-segs prior-ac 0))]
              (recur (next acbc)
                     (conj! lc cell)
                     (assoc! lsegs cell (:segment-idx sc))))))
        ;; finished
        {:learn-cells (persistent! lc)
         :learn-segs (persistent! lsegs)}))))

(defn tune-spec
  "Adjust stimulus threshold when using local inhibition to converge
   on the target level of activation."
  [spec actual-activation-level n-inbits]
  (if (or (:global-inhibition? spec false)
          (zero? n-inbits)) ;; ignore case of no input (a gap)
    spec
    (let [target-level (:activation-level spec 0.02)]
      (update-in spec [:ff-stimulus-threshold]
                 (fn [x]
                   ;; adjust threshold proportionally to error ratio
                   ;; but dampened to 10%
                   (let [scale (/ actual-activation-level target-level)
                         delta (* x (- scale 1) 0.10)]
                     (-> (+ x delta)
                         (max 1.0)
                         (min 1000.0))))))))

;;; ## Orchestration

(defn update-inhibition-radius
  [layer]
  (assoc layer :inh-radius
         (inh/inhibition-radius (:proximal-sg layer) (:topology layer)
                                (:input-topology layer))))

(defrecord LayerOfCells
    [spec topology input-topology inh-radius proximal-sg distal-sg
     overlaps proximal-exc proximal-sig-exc distal-exc
     active-cols burst-cols active-cells learn-cells signal-cells
     prior-active-cells prior-learn-cells pred-cells prior-pred-cells
     boosts active-duty-cycles overlap-duty-cycles]
  p/PLayerOfCells
  (layer-activate
    [this ff-bits signal-ff-bits]
    (let [om (syn/excitations proximal-sg ff-bits)
          prox-exc (columns/apply-overlap-boosting om boosts spec)
          sig-om (syn/excitations proximal-sg signal-ff-bits)
          {acbc :active-cells-by-col
           b-cols :burst-cols} (select-active-cells prox-exc distal-exc topology
                                                    inh-radius spec)
          a-cols (set (keys acbc))
          ac (set (apply concat (vals acbc)))
          sig-ac (set (apply concat (vals (apply dissoc acbc b-cols))))
          ]
      (assoc this
        :timestep (inc (:timestep this 0))
        :overlaps om
        :proximal-exc prox-exc
        :sig-overlaps sig-om
        :active-cells-by-col acbc ;; for convenience / efficiency in other steps
        :active-cells ac
        :active-cols a-cols
        :burst-cols b-cols
        :signal-cells sig-ac
        :prior-active-cells active-cells
        :prior-learn-cells learn-cells
        )))
  
  (layer-learn
    [this ff-bits]
    (let [dcp (:duty-cycle-period spec)
          t (:timestep this)
          boost? (zero? (mod t dcp))
          new-spec (tune-spec spec (/ (count active-cols) (p/size topology))
                              (count ff-bits))
          prior-ac prior-active-cells
          prior-lc prior-learn-cells
          acbc (:active-cells-by-col this)
          {lc :learn-cells
           lsegs :learn-segs} (select-learning-segs acbc burst-cols distal-sg
                                                    prior-active-cells spec)
          dsg (cond->
               (learn-distal distal-sg lsegs burst-cols prior-ac prior-lc spec)
               ;; allow this phase of learning as an option
               (:distal-punish? spec)
               (punish-distal active-cells prior-ac pred-cells prior-pred-cells
                              spec))
          psg (columns/learn-proximal proximal-sg input-topology topology
                                      active-cols ff-bits overlaps spec)]
      (cond->
       (assoc this
         :spec new-spec
         :learn-cells lc
         :learn-segments lsegs
         :distal-sg dsg
         :proximal-sg psg)
       true (update-in [:overlap-duty-cycles] columns/update-duty-cycles
                       (keys proximal-exc) dcp)
       true (update-in [:active-duty-cycles] columns/update-duty-cycles
                       active-cols dcp)
       boost? (columns/update-boosting)
       boost? (update-inhibition-radius))))
  
  (layer-depolarise
    [this distal-ff-bits distal-fb-bits]
    ;; TODO distal-bits
    (let [seg-exc (syn/excitations distal-sg active-cells)
          cell-exc (cell-depolarisation seg-exc (:seg-stimulus-threshold spec))
          pc (set (keys cell-exc))]
      (assoc this
        :pred-cells pc
        :prior-pred-cells pred-cells
        :distal-exc cell-exc)))
  
  (layer-depth [_]
    (:depth spec))
  (bursting-columns [this]
    burst-cols)
  (active-columns [_]
    active-cols)
  (active-cells [this]
    (:active-cells this))
  (learnable-cells [this]
    learn-cells)
  (signal-cells [this]
    (:signal-cells this))
  (temporal-pooling-cells [this]
    #{})
  (predictive-cells [this]
    pred-cells)
  (prior-predictive-cells [this]
    prior-pred-cells)
  (depolarisation [this]
    distal-exc)
  (column-excitation [_]
    proximal-exc)
  p/PTopological
  (topology [this]
    (:topology this))
  p/PTemporal
  (timestep [this]
    (:timestep this 0))
  p/PParameterised
  (params [_]
    spec))

(defn layer-of-cells
  [spec]
  (let [spec (merge parameter-defaults spec)
        input-topo (topology/make-topology (:input-dimensions spec))
        col-topo (topology/make-topology (:column-dimensions spec))
        n-cols (p/size col-topo)
        all-syns (columns/uniform-ff-synapses col-topo input-topo spec)
        proximal-sg (syn/synapse-graph all-syns (p/size input-topo)
                                       (:ff-perm-connected spec)
                                       (:ff-max-synapse-count spec)
                                       (:ff-grow-and-die? spec))
        distal-sg (syn/synapse-graph-by-segments n-cols (:depth spec)
                                                 (:max-segments spec)
                                                 (:distal-perm-connected spec)
                                                 (:seg-max-synapse-count spec)
                                                 true)]
    (->
     (map->LayerOfCells
      {:spec spec
       :topology col-topo
       :input-topology input-topo
       :inh-radius 1
       :proximal-sg proximal-sg
       :distal-sg distal-sg
       :prox-exc {}
       :overlaps {}
       :sig-overlaps {}
       :active-cols #{}
       :burst-cols #{}
       :active-cells #{}
       :learn-cells #{}
       :signal-cells #{}
       :pred-cells #{}
       :prior-pred-cells #{}
       :distal-exc {}
       :boosts (vec (repeat n-cols 1.0))
       :active-duty-cycles (vec (repeat n-cols 0.0))
       :overlap-duty-cycles (vec (repeat n-cols 0.0))
       })
     (update-inhibition-radius))))
