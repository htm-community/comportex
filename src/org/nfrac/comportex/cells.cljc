(ns org.nfrac.comportex.cells
  "Cell activation and sequence memory.

   **Argument name conventions:**

   * `col` -- a column id, an integer index in the region.
   * `ci` -- a cell id, an integer index in the column.
   * `si` -- a segment id, an integer index in the cell.
   * `cell-id` -- a vector `[col ci]`.
   * `seg-path` -- a vector `[col ci si]`.

   * `ff-bits` -- the set of indices of active bits/cells on proximal dendrites.
   * `aci` -- the set of indices of active bits/cells on distal dendrites.
   * `lci` -- the indices of learnable (winner) bits/cells on distal dendrites.
   * `ac` -- the set of ids of active cells.
   * `pc` -- the set of ids of predictive cells.
   * `tpc` -- the set of ids of temporal pooling cells.
   * `lc` -- the set of ids of learnable cells (winner cells).
   * `a-cols` -- the set of ids of active columns.
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

   * `ff-perm-init-hi` - highest initial permanence value on new synapses.

   * `ff-perm-init-lo` - lowest initial permanence value on new synapses.

   * `ff-stimulus-threshold` - minimum number of active input
     connections for a column to be _overlapping_ the input (i.e.
     active prior to inhibition). This parameter is tuned at run time
     to target `activation-level` when `global-inhibition?` is false.

   * `ff-max-synapse-count` - maximum number of synapses on the column.

   * `max-boost` - ceiling on the column boosting factor used to
     increase activation frequency.

   * `duty-cycle-period` - number of time steps to average over when
     updating duty cycles and (thereby) column boosting measures.

   * `boost-active-duty-ratio` - when a column's activation frequency is
     below this proportion of the _highest_ of its neighbours, its
     boost factor is increased.

   * `boost-overlap-duty-ratio` - when a column's overlap frequency is
     below this proportion of the _highest_ of its neighbours, its
     feed-forward synapses are boosted.

   * `boost-active-every` - number of time steps between recalculating
     column boosting factors.

   * `boost-overlap-every` - number of time steps between boosting
     column overlaps by increasing synapse permanences.

   * `inh-radius-every` - number of time steps between recalculating
     the effective inhibition radius.

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

   * `distal-vs-proximal-weight-cols` - scaling to apply to the number of
     active distal synapses (on the winning segment) before adding to
     the number of active proximal synapses, when selecting active
     columns. Set to zero to disable ``prediction-assisted'' activation.

   * `distal-vs-proximal-weight-cells` - same as above but applies
     when selecting active cells once columns have been chosen. This
     should usually be the same value as
     `distal-vs-proximal-weight-cols`, except when that is zero.

   * `spontaneous-activation?` - if true, cells may become active with
     sufficient distal synapse excitation, even in the absence of any
     proximal synapse excitation.

   * `dominance-margin` - an amount of excitation (generally measured
      in number of active synapses) by which one cell must exceed all
      others in the column to be considered dominant. And therefore to
      inhibit all other cells in the column.

  * `temporal-pooling-max-exc` - maximum continuing temporal pooling
     excitation level.

   * `temporal-pooling-amp` - multiplier on the initial excitation
     score of temporal pooling cells; this increases the probability
     that TP cells will remain active.

   * `temporal-pooling-fall` - amount by which a cell's continuing
     temporal pooling excitation falls each time step in the absence of
     further input.
"
  {:input-dimensions [:define-me!]
   :column-dimensions [2048]
   :ff-potential-radius 0.3
   :ff-init-frac 0.3
   :ff-perm-inc 0.05
   :ff-perm-dec 0.005
   :ff-perm-connected 0.20
   :ff-perm-init-hi 0.25
   :ff-perm-init-lo 0.10
   :ff-stimulus-threshold 1
   :ff-max-synapse-count 1000
   :max-boost 3.0
   :duty-cycle-period 1000
   :boost-active-duty-ratio 0.001
   :boost-overlap-duty-ratio 0.001
   :boost-active-every 1
   :boost-overlap-every 1000
   :inh-radius-every 1000
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
   :distal-vs-proximal-weight-cols 0
   :distal-vs-proximal-weight-cells 0.5
   :spontaneous-activation? false
   :dominance-margin 10
   :temporal-pooling-max-exc 50
   :temporal-pooling-amp 5.0
   :temporal-pooling-fall 10
   })

;;; ## Synapse tracing

(defn distal-sources-widths
  [spec]
  [(if (:lateral-synapses? spec)
     (reduce * (:depth spec) (:column-dimensions spec))
     0)
   (reduce * (:distal-motor-dimensions spec))
   (reduce * (:distal-topdown-dimensions spec))])

;; applies to cells in the current layer only
(defn cell->id
  [depth [col ci]]
  (+ (* col depth) ci))

(defn- cells->bits
  [depth cells]
  (map (partial cell->id depth) cells))

;; applies to cells in the current layer only
(defn id->cell
  [depth id]
  [(quot id depth)
   (rem id depth)])

(defn id->source
  "Returns a vector [k v] where k is one of :this, :ff, :fb. In the
   case of :this, v is [col ci], otherwise v gives the index in the
   feed-forward or feed-back input field."
  [spec id]
  (let [[this-w ff-w fb-w] (distal-sources-widths spec)]
    (cond
     (< id this-w) [:this (id->cell (:depth spec) id)]
     (< id (+ this-w ff-w)) [:ff (- id this-w)]
     (< id (+ this-w ff-w fb-w)) [:fb (- id this-w ff-w)])))

;;; ## Activation

(defn segment-activation
  "Returns the number of active cells to which the synapses are
  connected, i.e. where synapse permanence is equal to or greater than
  `pcon`."
  [syns aci pcon]
  (count-filter (fn [[id p]]
                  (and (>= p pcon)
                       (aci id)))
                syns))

(defn cell-active-segments
  "Returns a seq of the segment indexes in the cell with activation at
  or above the activation threshold `th`, only considering synapses
  with permanence values at or above `pcon`."
  [cell-segs aci th pcon]
  (keep-indexed (fn [si syns]
                  (let [act (segment-activation syns aci pcon)]
                    (when (>= act th) si)))
                cell-segs))

(defn best-matching-segment
  "Finds the segment in the cell having the most active synapses, as
  long as is above the activation threshold `th`, only considering
  synapses with permanence values at or above `pcon`.  Returns the
  segment index. If no such segments exist, returns nil."
  [cell-segs aci th pcon]
  (loop [segs cell-segs
         si 0
         best-si 0
         best-act 0]
    (if-let [syns (first segs)]
      (let [act (long (segment-activation syns aci pcon))]
        (recur (next segs)
               (inc si)
               (if (> act best-act) si best-si)
               (if (> act best-act) act best-act)))
      ;; finished
      (if (>= best-act th)
        best-si
        nil))))

(defn distal-excitations
  "Computes distal excitatation as a map from cell id to the greatest
  number of active synapses on any one distal dendrite segment. Note
  this is not filtered by a threshold for segment activation -- that
  has been deferred to a later stage so that a sub-threshold number of
  active synapses can contribute to picking a winner cell."
  [seg-exc]
  (->> seg-exc
       (reduce-kv (fn [m k n]
                    (if (zero? n)
                      m
                      (let [id (pop k)] ;; seg-id to cell-id: [col ci _]
                        (assoc! m id (max n (get m id 0))))))
                  (transient {}))
       (persistent!)))

(defn total-excitations
  "Combine the proximal and distal excitations in a map of cell id to
  excitation, as a weighted sum. Normally only cells with some proximal
  input are considered, but if `spontaneous-activation?` is true, this
  is not enforced (i.e. prediction alone could cause activation)."
  [prox-exc distal-exc distal-weight spontaneous-activation?]
  (if (zero? distal-weight)
    prox-exc
    (let [prox-exc (if (spontaneous-activation?)
                     (merge (zipmap (keys distal-exc) (repeat 0.0))
                            prox-exc)
                     prox-exc)]
      (persistent!
       (reduce-kv (fn [m id p-exc]
                    (let [d-exc (distal-exc id 0.0)]
                      (assoc! m id (+ p-exc (* distal-weight d-exc)))))
                  (transient {})
                  prox-exc)))))

(defn column-excitations
  "Returns a map of column ids to representative excitation values,
  being the greatest excitation of its constituent cells."
  [cell-exc]
  (persistent!
   (reduce-kv (fn [m [col ci] exc]
                (assoc! m col
                        (max exc (get m col 0.0))))
              (transient {})
              cell-exc)))

(defn select-active-columns
  "Returns a set of column ids to become active after lateral inhibition."
  [col-exc topo inh-radius spec]
  (let [level (:activation-level spec)
        n-on (max 1 (round (* level (p/size topo))))]
    (set
     (if (:global-inhibition? spec)
       (inh/inhibit-globally col-exc n-on)
       (inh/inhibit-locally col-exc topo inh-radius
                            (:inhibition-base-distance spec)
                            n-on)))))

(defn column-active-cells
  "Returns `[winner-cell-id active-cell-ids]`.
  The winner cell is the one with greatest excitation. The active
  cells are those within `dominance-margin` of the winner cell. The
  most common cases are (a) one cell is dominant in the column, or (b)
  no cells are dominant, therefore all cells become active."
  [col cell-exc depth dominance-margin]
  (let [cell-ids (for [ci (range depth)] [col ci])
        first-id (first cell-ids)
        first-exc (double (cell-exc first-id 0.0))]
    (loop [ids (next cell-ids)
           best-id first-id
           best-exc first-exc
           second-id nil
           second-exc -99999.0
           worst-id first-id
           worst-exc first-exc]
      (if-let [id (first ids)]
        (let [exc (double (cell-exc id 0))
              best? (> exc best-exc)
              worst? (< exc worst-exc)
              sec? (and (not best?) (> exc second-exc))]
          (recur (next ids)
                 (if best? id best-id)
                 (if best? exc best-exc)
                 (cond sec? id
                       best? best-id
                       :else second-id)
                 (double
                  (cond sec? exc
                        best? best-exc
                        :else second-exc))
                 (if worst? id worst-id)
                 (if worst? exc worst-exc)))
        ;; finished
        (cond
          ;; only one cell
          (== 1 depth)
          [best-id cell-ids]
          ;; none dominant
          (< (- best-exc (max 0.0 worst-exc)) dominance-margin)
          [best-id cell-ids]
          ;; one dominant
          (>= (- best-exc second-exc) dominance-margin)
          [best-id (list best-id)]
          ;; in general should scan back through...
          ;; for now just take the top 2 since we already have them.
          :else
          [best-id (list best-id second-id)])))))

(defn select-active-cells
  "Determines active cells in the given columns and whether they are bursting.
   Returns keys
  * `:active-cells` - the set of active cell ids.
  * `:stable-active-cells` - the set of non-bursting active cells.
  * `:burst-cols` - the set of bursting column ids.
  * `:winner-cells` - the set of winner cells, one from each active column."
  [a-cols cell-exc pred-cells spec]
  (let [depth (:depth spec)
        dominance-margin (:dominance-margin spec)]
    (loop [cols (seq a-cols)
           ac (transient #{})
           sac (transient #{}) ;; stable active cells
           b-cols (transient #{})
           lc (transient #{})]
      (if-let [col (first cols)]
        (let [[win-cell col-ac] (column-active-cells col cell-exc depth
                                                     dominance-margin)
              bursting? (not (pred-cells win-cell))
              next-ac (reduce conj! ac col-ac)
              next-sac (if bursting?
                         sac
                         (reduce conj! sac col-ac))]
          (recur (next cols)
                 next-ac
                 next-sac
                 (if bursting? (conj! b-cols col) b-cols)
                 (conj! lc win-cell)))
        ;; finished
        {:active-cells (persistent! ac)
         :stable-active-cells (persistent! sac)
         :burst-cols (persistent! b-cols)
         :winner-cells (persistent! lc)}
        ))))

(defn effects-of-inactive-segments
  "Calculates an excitation effect on cells caused by inactive distal
  segments. It looks for segments matching the input -- meeting a
  number `:seg-learn-threshold` of active synapses -- even if the
  synapses are not yet connected (below the permanence threshold). If
  a cell does have a segment that matches the input this way, its
  adjustment is positive. If a cell has segments but none match the
  input, its adjustment is negative and proportional to the number of
  segments. The adjustment unit amount is chosen to be lower than for
  a connected active segment: one less than `:seg-learn-threshold`,
  scaled by the distal-vs-proximal weight. Returns a map of cell ids
  to these excitation adjustment values."
  [a-cols cell-exc distal-sg aci spec]
  (let [depth (:depth spec)
        th (:seg-learn-threshold spec)
        w (:distal-vs-proximal-weight-cells spec)
        adj-unit (* w (dec th))]
    (->> (for [col a-cols
               ci (range depth)
               :let [cell-id [col ci]
                     cell-segs (->> (p/cell-segments distal-sg cell-id)
                                    (filter seq))
                     n-segs (count cell-segs)]
               :when (pos? n-segs)]
           (if (best-matching-segment cell-segs aci th 0.0)
             ;; some segment matches the input even if synapses disconnected
             [cell-id adj-unit]
             ;; there are segments but none match the input; apply penalty
             [cell-id (* -1 adj-unit n-segs)]))
         (into {}))))

;;; ## Proximal Learning

(defn learn-proximal
  "Adapt feed-forward synapses to focus on observed input patterns.
  Given the set of input bits `ff-bits`, adjusts the permanence
  values of all potential feed-forward synapses in the active cells
  `ac`."
  [sg ac ff-bits spec]
  (let [pinc (:ff-perm-inc spec)
        pdec (:ff-perm-dec spec)]
    (reduce (fn [sg cell-id]
              (p/reinforce-in-synapses sg cell-id (constantly false)
                                       ff-bits pinc pdec))
            sg ac)))

;;; ## Distal Learning

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

(defn segment-new-synapse-source-ids
  "Returns a collection of up to n ids chosen from the learnable cell
   bits `lci-vec`. May be less than `n` if the random samples have
   duplicates or some already exist on the segment, or if there are
   fewer than `n` learnable cells. Connections to the host column are
   not allowed. However, connections can be made to a cell even if
   there is already a connection to another cell in the same column."
  [seg lci-vec n exclude]
  (when (seq lci-vec)
    (->> lci-vec
         (util/sample n)
         (distinct)
         (remove (fn [id]
                   (or (seg id)
                       (exclude id)))))))

(defn grow-new-segment
  "Adds a new segment on the cell with synapses to a selection of the
   learn cells from previous time step, unless there are too few to
   meet the minimum threshold."
  [distal-sg col ci lci-vec spec]
  (let [n (:seg-new-synapse-count spec)
        min-syns (:seg-learn-threshold spec)
        si (new-segment-id distal-sg col ci spec)
        depth (:depth spec)
        exclude-col (if (:lateral-synapses? spec)
                      (fn [id] (= col (first (id->cell depth id))))
                      (constantly false))
        syn-cell-ids (segment-new-synapse-source-ids {} lci-vec n exclude-col)]
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
  [distal-sg seg-path aci lci-vec spec]
  (let [col (first seg-path)
        seg (p/in-synapses distal-sg seg-path)
        na (segment-activation seg aci 0.0) ;; include disconnected
        n (- (:seg-new-synapse-count spec) na)]
    (if (pos? n)
      (let [depth (:depth spec)
            exclude-col (if (:lateral-synapses? spec)
                          (fn [id] (= col (first (id->cell depth id))))
                          (constantly false))
            ids (segment-new-synapse-source-ids seg lci-vec n exclude-col)]
        (p/conj-synapses distal-sg seg-path ids (:distal-perm-init spec)))
      distal-sg)))

(defn segment-reinforce
  [sg seg-path aci pinc pdec]
  (p/reinforce-in-synapses sg seg-path (constantly false) aci pinc pdec))

(defn segment-punish
  [sg seg-path aci pdec]
  (p/reinforce-in-synapses sg seg-path aci (constantly false) 0.0 pdec))

(defn learn-on-segment
  [distal-sg col ci si bursting? lci-vec aci spec]
  (let [pinc (:distal-perm-inc spec)
        pdec (:distal-perm-dec spec)]
    (if si
      ;; there is a matching segment, reinforce and/or extend it
      (cond-> distal-sg
              true (segment-reinforce [col ci si] aci pinc pdec)
              bursting? (segment-extend [col ci si] aci lci-vec spec))
      ;; no matching segment, create a new one
      (grow-new-segment distal-sg col ci lci-vec spec))))

(defn learn-distal
  [distal-sg lsegs b-cols prior-aci prior-lci spec]
  (let [prior-lci-vec (vec prior-lci)]
    (reduce-kv (fn [sg [col ci] si]
                 (learn-on-segment sg col ci si (b-cols col) prior-lci-vec
                                   prior-aci spec))
               distal-sg
               lsegs)))

(defn punish-cell
  [distal-sg col ci prior-aci th pcon pdec]
  (let [cell-segs (p/cell-segments distal-sg [col ci])
        asegs (cell-active-segments cell-segs prior-aci th pcon)]
    (reduce (fn [sg si]
              (segment-punish sg [col ci si] prior-aci pdec))
            distal-sg asegs)))

(defn punish-distal
  "Punish segments which predicted activation on cells which did
   not become active. Ignore any which are still predictive."
  [distal-sg prior-pc pc ac prior-aci spec]
  (let [th (:seg-stimulus-threshold spec)
        pcon (:distal-perm-connected spec)
        pdec (:distal-perm-dec spec)
        bad-cells (set/difference prior-pc
                                  pc
                                  ac)]
    (reduce (fn [sg [col ci]]
              (punish-cell sg col ci prior-aci th pcon pdec))
            distal-sg
            bad-cells)))

(defn select-learning-segs
  "Returns a map from cell ids (the learning cells) to a segment index
  to learn on, being an adequately matching one, or nil, indicating
  that a new segment should be grown."
  [lc burst-cols distal-sg prior-aci spec]
  (let [pcon (:distal-perm-connected spec)
        ;; threshold to consider disconnected synapses
        pdisc 0.0]
    (persistent!
     (reduce (fn [m cell-id]
               (let [col (first cell-id)
                     cell-segs (p/cell-segments distal-sg cell-id)
                     ;; if bursting column - i.e. unpredicted - then consider
                     ;; unconnected synapses for best matching segment.
                     use-p (if (burst-cols col) pdisc pcon)
                     seg-idx (best-matching-segment cell-segs prior-aci
                                                    (:seg-learn-threshold spec)
                                                    use-p)]
                 (assoc! m cell-id seg-idx)))
             (transient {})
             lc))))

;;; ## Orchestration

(defn update-inhibition-radius
  [layer]
  (assoc layer :inh-radius
         (inh/inhibition-radius (:proximal-sg layer) (:topology layer)
                                (:input-topology layer))))

(defn apply-excitation
  [continuing-exc immed-exc max-exc amp]
  (persistent!
   (reduce-kv (fn [m id exc]
                (assoc! m id (-> (get m id 0.0)
                                 (+ (* exc amp))
                                 (min max-exc))))
              (transient continuing-exc)
              immed-exc)))

(defn apply-loss-fn
  [continuing-exc loss-func]
  (persistent!
   (reduce-kv (fn [m id exc]
                (let [e (loss-func exc)]
                  (if (pos? e)
                    (assoc! m id e)
                    m)))
              (transient {})
              continuing-exc)))

(defn exponential-loss-fn
  [decay epsilon]
  (fn [exc]
    (if (< exc epsilon)
      0.0
      (* exc decay))))

(defn linear-loss-fn
  [fall]
  (fn [exc]
    (if (> exc fall)
      (- exc fall)
      0.0)))

(defrecord LayerActiveState
    [in-ff-bits in-stable-ff-bits
     out-ff-bits out-stable-ff-bits
     overlaps temporal-pooling-exc
     active-cols burst-cols active-cells learn-cells tp-cells])

(defrecord LayerDistalState
    [distal-bits distal-lc-bits distal-exc pred-cells])

(defrecord LayerOfCells
    [spec topology input-topology inh-radius proximal-sg distal-sg
     state prior-state distal-state prior-distal-state
     boosts active-duty-cycles overlap-duty-cycles]
  p/PLayerOfCells
  (layer-activate
    [this ff-bits stable-ff-bits]
    (let [;; this is proximal excitation per cell [col ci]
          overlaps (syn/excitations proximal-sg ff-bits)
          immed-exc (columns/apply-overlap-boosting overlaps boosts spec)
          ;; temporal pooling:
          ;; stable inputs (from predicted cells) add to ongoing "tp" activation
          stable-overlaps (syn/excitations proximal-sg stable-ff-bits)
          immed-stable-exc (columns/apply-overlap-boosting stable-overlaps boosts spec)
          ;immed-burst-exc (merge-with - immed-exc immed-stable-exc)
          tp-exc (apply-loss-fn (:temporal-pooling-exc state)
                                 (linear-loss-fn (:temporal-pooling-fall spec)))
          ;; integrate excitation values to activate cells
          base-exc (merge-with + immed-exc tp-exc)
          cell-exc (total-excitations base-exc
                                      (:distal-exc distal-state)
                                      (:distal-vs-proximal-weight-cols spec)
                                      (:spontaneous-activation? spec))
          a-cols (select-active-columns (column-excitations cell-exc)
                                        topology inh-radius spec)
          ;; inactive distal segments induce _negative_ depolarisation of cells.
          ;; need this to encourage context-specific choice of cells in a column:
          ;; avoid a cell that is missing its learned context signal (segment).
          ;; (biology -- more segments = larger surface area to lose potential?).
          ;; also - matching segments below connected threshold get a bonus.
          adj-exc (effects-of-inactive-segments a-cols cell-exc distal-sg
                                                (:distal-bits distal-state) spec)
          within-col-cell-exc (let [w (:distal-vs-proximal-weight-cells spec)]
                                (if (== (:distal-vs-proximal-weight-cols spec) w)
                                  (merge-with + cell-exc adj-exc)
                                  (merge-with + base-exc
                                             (->> (:distal-exc distal-state)
                                                  (filter (comp a-cols first))
                                                  (remap #(* w %)))
                                             adj-exc)))
          ;; find active and winner cells in the columns
          {ac :active-cells
           lc :winner-cells
           b-cols :burst-cols
           stable-ac :stable-active-cells}
          (select-active-cells a-cols within-col-cell-exc (:pred-cells distal-state) spec)
          ;; update continuing TP activation
          next-tp-exc (-> (apply-excitation tp-exc immed-stable-exc
                                            (:temporal-pooling-max-exc spec)
                                            (:temporal-pooling-amp spec))
                          ;; clear TP excitation for inhibited cells
                          (select-keys ac))
          depth (:depth spec)]
      (assoc this
             :timestep (inc (:timestep this 0))
             :prior-state state
             :state (map->LayerActiveState
                     {:in-ff-bits ff-bits
                      :in-stable-ff-bits stable-ff-bits
                      :out-ff-bits (set (cells->bits depth ac))
                      :out-stable-ff-bits (set (cells->bits depth stable-ac))
                      :overlaps overlaps
                      :temporal-pooling-exc next-tp-exc
                      :active-cells ac
                      :active-cols a-cols
                      :burst-cols b-cols
                      :learn-cells lc
                      :tp-cells (keys next-tp-exc)}))))

  (layer-learn
    [this]
    (let [ff-bits (:in-ff-bits state)
          t (:timestep this)
          prior-aci (:distal-bits distal-state)
          prior-lci (:distal-lc-bits distal-state)
          burst-cols (:burst-cols state)
          lc (:learn-cells state)
          lsegs (select-learning-segs lc burst-cols distal-sg prior-aci spec)
          dsg (cond->
               (learn-distal distal-sg lsegs burst-cols prior-aci prior-lci spec)
               ;; allow this phase of learning as an option
               (:distal-punish? spec)
               (punish-distal (:pred-cells prior-distal-state)
                              (:pred-cells distal-state)
                              (:active-cells state)
                              (:distal-bits prior-distal-state)
                              spec))
          psg (learn-proximal proximal-sg (:active-cells state) ff-bits spec)
          overlap-cols (distinct (map first (keys (:overlaps state))))]
      (cond->
       (assoc this
              :state (assoc state :learn-segments lsegs)
              :distal-sg dsg
              :proximal-sg psg)
       true (update-in [:overlap-duty-cycles] columns/update-duty-cycles
                       overlap-cols (:duty-cycle-period spec))
       true (update-in [:active-duty-cycles] columns/update-duty-cycles
                       (:active-cols state) (:duty-cycle-period spec))
       (zero? (mod t (:boost-active-every spec))) (columns/boost-active)
       (zero? (mod t (:boost-overlap-every spec))) (columns/boost-overlap)
       (zero? (mod t (:inh-radius-every spec))) (update-inhibition-radius))))

  (layer-depolarise
    [this distal-ff-bits distal-fb-bits]
    (let [depth (:depth spec)
          widths (distal-sources-widths spec)
          aci (util/align-indices widths
                                  [(if (:lateral-synapses? spec)
                                     (:out-ff-bits state)
                                     [])
                                   distal-ff-bits
                                   (if (:use-feedback? spec) distal-fb-bits [])])
          ;; possibly should pass in separate lc sets as arguments
          lci (util/align-indices widths
                                  [(if (:lateral-synapses? spec)
                                     (cells->bits depth (:learn-cells state))
                                     [])
                                   distal-ff-bits
                                   (if (:use-feedback? spec) distal-fb-bits [])])
          seg-exc (syn/excitations distal-sg aci)
          ;; TODO - should reflect one seg or many? - more vs fewer synapses?
          distal-exc (distal-excitations seg-exc)
          th (:seg-stimulus-threshold spec)
          pc (set (keep (fn [[id v]] (when (>= v th) id)) distal-exc))]
      (assoc this
        :prior-distal-state distal-state
        :distal-state (map->LayerDistalState
                       {:distal-bits (set aci)
                        :distal-lc-bits lci
                        :distal-exc distal-exc
                        :pred-cells pc}))))

  (layer-depth [_]
    (:depth spec))
  (bursting-columns [_]
    (:burst-cols state))
  (active-columns [_]
    (:active-cols state))
  (active-cells [_]
    (:active-cells state))
  (learnable-cells [_]
    (:learn-cells state))
  (temporal-pooling-cells [_]
    (:tp-cells state))
  (predictive-cells [_]
    (:pred-cells distal-state))
  (prior-predictive-cells [_]
    (:pred-cells prior-distal-state))
  p/PTopological
  (topology [this]
    (:topology this))
  p/PFeedForward
  (ff-topology [this]
    (topology/make-topology (conj (p/dims-of this)
                                  (p/layer-depth this))))
  (bits-value [_]
    (:out-ff-bits state))
  (stable-bits-value [_]
    (:out-stable-ff-bits state))
  (source-of-bit
    [_ i]
    (id->cell (:depth spec) i))
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
        depth (:depth spec)
        n-distal (+ (if (:lateral-synapses? spec)
                      (* n-cols depth) 0)
                    (reduce * (:distal-motor-dimensions spec))
                    (reduce * (:distal-topdown-dimensions spec)))
        prox-syns (columns/uniform-ff-synapses col-topo input-topo spec)
        proximal-sg (syn/cell-synapse-graph prox-syns depth (p/size input-topo)
                                       (:ff-perm-connected spec)
                                       (:ff-max-synapse-count spec)
                                       false)
        distal-sg (syn/cell-segments-synapse-graph n-cols depth
                                                 (:max-segments spec)
                                                 n-distal
                                                 (:distal-perm-connected spec)
                                                 (:seg-max-synapse-count spec)
                                                 true)
        state (map->LayerActiveState
               {:learn-cells #{}
                :active-cells #{}
                :active-cols #{}
                :temporal-pooling-exc {}})
        distal-state (map->LayerDistalState
                      {:distal-bits #{}
                       :pred-cells #{}
                       :distal-exc {}})]
    (->
     (map->LayerOfCells
      {:spec spec
       :topology col-topo
       :input-topology input-topo
       :inh-radius 1
       :proximal-sg proximal-sg
       :distal-sg distal-sg
       :state state
       :prior-state state
       :distal-state distal-state
       :prior-distal-state distal-state
       :boosts (vec (repeat n-cols 1.0))
       :active-duty-cycles (vec (repeat n-cols 0.0))
       :overlap-duty-cycles (vec (repeat n-cols 0.0))
       })
     (update-inhibition-radius))))
