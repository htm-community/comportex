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
   * `lci` -- the set of indices of learnable bits/cells on distal dendrites.
   * `ac` -- the set of ids of active cells.
   * `pc` -- the set of ids of predictive cells.
   * `tpc` -- the set of ids of temporal pooling cells.
   * `lc` -- the set of ids of learning cells.
   * `wbc` -- the map of column ids to winner cell id.
   * `a-cols` -- the set of ids of active columns.
   * `seg` or `syns` -- incoming synapses as a map from source id to permanence.
"
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.columns :as columns]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util
             :refer [count-filter remap round]]
            [clojure.test.check.random :as random]
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

   * `ff-perm-stable-inc` - amount to increase a synapse's permanence
     value by when it is reinforced by a stable (predicted) input.

   * `ff-perm-connected` - permanence value at which a synapse is
     functionally connected. Permanence values are defined to be
     between 0 and 1.

   * `ff-perm-init-hi` - highest initial permanence value on new synapses.

   * `ff-perm-init-lo` - lowest initial permanence value on new synapses.

   * `ff-stimulus-threshold` - minimum number of active input
     connections for a column to be _overlapping_ the input (i.e.
     active prior to inhibition).

   * `ff-max-synapse-count` - maximum number of synapses on the column.

   * `max-boost` - ceiling on the column boosting factor used to
     increase activation frequency.

   * `duty-cycle-period` - number of time steps to average over when
     updating duty cycles and (thereby) column boosting measures.

   * `boost-active-duty-ratio` - when a column's activation frequency is
     below this proportion of the _highest_ of its neighbours, its
     boost factor is increased.

   * `boost-active-every` - number of time steps between recalculating
     column boosting factors.

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
     permanence when reinforcing distal dendrite segments.

   * `distal-perm-dec` - amount by which to decrease synapse permanence
     when reinforcing distal dendrite segments.

   * `distal-perm-punish` - amount by which to decrease synapse permanence
     when punishing distal dendrite segments in case of failed prediction.

   * `distal-perm-connected` - permanence value at which a synapse is
     functionally connected. Permanence values are defined to be
     between 0 and 1.

   * `distal-perm-init` - permanence value for new synapses on
     dendrite segments.

   * `distal-punish?` - whether to reduce synapse permanence on
     distal segments incorrectly predicting activation.

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
     columns. Set to zero to disable ``prediction-assisted'' activation.

   * `spontaneous-activation?` - if true, cells may become active with
     sufficient distal synapse excitation, even in the absence of any
     proximal synapse excitation.

   * `dominance-margin` - an amount of excitation (generally measured
      in number of active synapses) by which one cell must exceed all
      others in the column to be considered dominant. And therefore to
      inhibit all other cells in the column.

   * `stable-inbit-frac-threshold` - fraction of proximal input bits
     to a layer which must be from stable cells in order to start
     temporal pooling.

   * `temporal-pooling-max-exc` - maximum continuing temporal pooling
     excitation level.

  * `temporal-pooling-fall` - amount by which a cell's continuing
     temporal pooling excitation falls each time step in the absence of
     stable input.
"
  {:input-dimensions [:define-me!]
   :column-dimensions [1000]
   :ff-potential-radius 1.0
   :ff-init-frac 0.25
   :ff-perm-inc 0.04
   :ff-perm-dec 0.01
   :ff-perm-stable-inc 0.15
   :ff-perm-connected 0.20
   :ff-perm-init-hi 0.25
   :ff-perm-init-lo 0.10
   :ff-stimulus-threshold 2
   :ff-seg-max-synapse-count 300
   :ff-seg-new-synapse-count 12
   :ff-seg-learn-threshold 7
   :ff-max-segments 1
   :max-boost 1.5
   :duty-cycle-period 1000
   :boost-active-duty-ratio 0.001
   :boost-active-every 1000
   :inh-radius-every 1000
   :lateral-synapses? true
   :use-feedback? false
   :distal-motor-dimensions [0]
   :distal-topdown-dimensions [0]
   :depth 5
   :max-segments 5
   :seg-max-synapse-count 22
   :seg-new-synapse-count 12
   :seg-stimulus-threshold 9
   :seg-learn-threshold 7
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-punish 0.002
   :distal-perm-connected 0.20
   :distal-perm-init 0.16
   :distal-punish? true
   :activation-level 0.02
   :activation-level-max 0.10
   :global-inhibition? true
   :inhibition-base-distance 1
   :distal-vs-proximal-weight 0.0
   :spontaneous-activation? false
   :dominance-margin 4
   :stable-inbit-frac-threshold 0.5
   :temporal-pooling-max-exc 50.0
   :temporal-pooling-fall 5.0
   :random-seed 42
   })

;; TODO decide on defaults (reliability vs speed), provide alternatives?
(def better-parameter-defaults
  (assoc parameter-defaults
         :column-dimensions [2048]
         :depth 16
         :seg-stimulus-threshold 13
         :seg-learn-threshold 10
         :seg-new-synapse-count 20
         :seg-max-synapse-count 32
         :max-segments 8))

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
  long as is above the activation threshold `min-act`, only considering
  synapses with permanence values at or above `pcon`.  Returns
  `[seg-index activation synapses]`. If no such segments exist,
  returns `[nil 0 {}]`."
  [cell-segs aci min-act pcon]
  (loop [segs (seq cell-segs)
         si 0
         best-si 0
         best-act 0
         best-syns nil]
    (if-let [syns (first segs)]
      (let [act (long (segment-activation syns aci pcon))
            best? (> act best-act)]
        (recur (next segs)
               (inc si)
               (if best? si best-si)
               (if best? act best-act)
               (if best? syns best-syns)))
      ;; finished
      (if (>= best-act min-act)
        [best-si best-act best-syns]
        [nil 0 {}]))))

(defn best-segment-excitations-and-paths
  "Finds the most excited dendrite segment for each cell. Returns a
  tuple of 3 maps keyed by cell id, the first contains the segment
  excitation values, the second contains the segment paths, and the
  third the segment paths only for cells with excitation meeting
  min-threshold. In other words the latter paths identify the dominant
  segments on well-matching cells."
  [seg-exc min-threshold]
  (loop [seg-exc (seq seg-exc)
         excs (transient {})
         paths (transient {})
         good-paths (transient {})]
    (if-let [[k exc] (first seg-exc)]
      (let [id (pop k) ;; seg-id to cell-id: [col ci _]
            prev-exc (get excs id 0.0)]
        (if (> exc prev-exc)
          (recur (next seg-exc)
                 (assoc! excs id exc)
                 (assoc! paths id k)
                 (if (>= exc min-threshold)
                   (assoc! good-paths id k)
                   good-paths))
          (recur (next seg-exc)
                 excs
                 paths
                 good-paths)))
      ;; finished
      [(persistent! excs)
       (persistent! paths)
       (persistent! good-paths)])))

(defn best-segment-excitations
  "Computes excitatation as a map from cell id to the greatest
  number of active synapses on any one dendrite segment."
  [seg-exc]
  (persistent!
   (reduce-kv (fn [m k exc]
                (let [id (pop k)] ;; seg-id to cell-id: [col ci _]
                  (assoc! m id (max exc (get m id 0.0)))))
              (transient {})
              seg-exc)))

(defn best-by-column
  "Returns a map of column ids to representative excitation values,
  being the greatest excitation of its constituent cells or segments."
  [cell-exc]
  (persistent!
   (reduce-kv (fn [m id exc]
                (let [[col _] id] ;; cell-id / seg-id to col
                  (assoc! m col (max exc (get m col 0.0)))))
              (transient {})
              cell-exc)))

(defn total-excitations
  "Combine the proximal and distal excitations in a map of cell id to
  excitation, as a weighted sum. Temporal Pooling `tp-exc` is added to
  the proximal excitation but is given keyed by cell rather than by
  column. Normally only cells with some proximal input are included,
  but if `spontaneous-activation?` is true, this is relaxed
  (i.e. prediction alone could cause activation).

  * col-exc is keyed by column as [col 0].
  * tp-exc is keyed by cell as [col ci]."
  [col-exc tp-exc distal-exc distal-weight spontaneous-activation? depth]
  (let [has-tp? (seq tp-exc)
        ;; add TP columns to list of columns to consider (may have no proximal)
        basic-col-exc (if has-tp?
                        (merge-with + col-exc
                                    (into {} (map (fn [[col _]] [[col 0] 0]))
                                          (keys tp-exc)))
                        col-exc)
        ;; expand to all cells within columns, add TP values
        basic-exc (for [[[col _] exc] basic-col-exc
                        ci (range depth)
                        :let [cell-id [col ci]
                              tp (if has-tp? (get tp-exc cell-id 0.0) 0.0)]]
                    [cell-id (+ exc tp)])]
    (if (zero? distal-weight)
      (into {} basic-exc)
      (let [basic-exc (if spontaneous-activation?
                        (merge (zipmap (keys distal-exc) (repeat 0.0))
                               (into {} basic-exc))
                        basic-exc)]
        ;; add distal values
        (persistent!
         (reduce (fn [m [id p-exc]]
                   (let [d-exc (distal-exc id 0.0)]
                     (assoc! m id (+ p-exc (* distal-weight d-exc)))))
                 (transient {})
                 basic-exc))))))

(defn select-active-columns
  "Returns a set of column ids to become active after lateral inhibition."
  [col-exc topo activation-level inh-radius spec]
  (let [level activation-level
        n-on (max 1 (round (* level (p/size topo))))]
    (set
     (if (:global-inhibition? spec)
       (inh/inhibit-globally col-exc n-on)
       (inh/inhibit-locally col-exc topo inh-radius
                            (:inhibition-base-distance spec)
                            n-on)))))

(defn column-active-cells
  "Returns `[winner-cell-id active-cell-ids]`.
  The winner cell is one with greatest excitation, with ties broken
  randomly. If no cells exceed the threshold, then all become
  active (''bursting''). Otherwise, only cells above the threshold
  become active; but if the winner exceeds all others by at least
  `dominance-margin` then it is the only active cell."
  [col cell-exc prior-winner depth threshold dominance-margin rng]
  (let [cell-ids (for [ci (range depth)] [col ci])]
    (loop [ids cell-ids
           best-ids ()
           best-exc 0.0
           good-ids () ;; over threshold
           min-good-exc (double (* (inc threshold) 1000))]
      (if-let [id (first ids)]
        (let [exc (double (cell-exc id 0))
              equal-best? (== exc best-exc)
              new-best? (> exc best-exc)
              good? (>= exc threshold)]
          (recur (next ids)
                 (cond equal-best? (conj best-ids id)
                       new-best? (list id)
                       :else best-ids)
                 (if new-best? exc best-exc)
                 (if good? (conj good-ids id) good-ids)
                 (if (and good? (< exc min-good-exc)) exc min-good-exc)))
        ;; finished
        (let [winner (cond
                       (empty? best-ids)
                       (first cell-ids)
                       (== (count best-ids) 1)
                       (first best-ids)
                       (and prior-winner (some #(= % prior-winner) best-ids))
                       prior-winner
                       :else
                       (util/rand-nth rng best-ids))
              actives (cond
                        ;; stimulus threshold not reached
                        (< best-exc threshold)
                        cell-ids
                        ;; of cells over threshold, spread within dominance-margin
                        (< (- best-exc min-good-exc) dominance-margin)
                        good-ids
                        ;; otherwise best cell(s) exceed others by dominance-margin
                        :else
                        best-ids)]
          [winner actives])))))

(defn select-active-cells
  "Determines active cells in the given columns and whether they are bursting.
   Returns keys
  * `:active-cells` - the set of active cell ids.
  * `:stable-active-cells` - the set of non-bursting active cells.
  * `:burst-cols` - the set of bursting column ids.
  * `:winners-by-col` - the map of column id to winner cell id."
  [a-cols cell-exc prior-wbc spec rng]
  (let [depth (:depth spec)
        threshold (:seg-stimulus-threshold spec)
        dominance-margin (:dominance-margin spec)]
    (loop [cols (seq a-cols)
           ac (transient #{})
           sac (transient #{}) ;; stable active cells
           b-cols (transient #{})
           wbc (transient {}) ;; winning cell by column
           rng rng]
      (if-let [col (first cols)]
        (let [[rng rng*] (random/split rng)
              ;; carry forward learning cells for higher level sequences
              prior-winner (get prior-wbc col)
              [win-cell col-ac] (column-active-cells col cell-exc prior-winner
                                                     depth threshold dominance-margin
                                                     rng*)
              bursting? (== (count col-ac) depth)
              next-ac (reduce conj! ac col-ac)
              next-sac (if bursting?
                         sac
                         (reduce conj! sac col-ac))]
          (recur (next cols)
                 next-ac
                 next-sac
                 (if bursting? (conj! b-cols col) b-cols)
                 (assoc! wbc col win-cell)
                 rng))
        ;; finished
        {:active-cells (persistent! ac)
         :stable-active-cells (persistent! sac)
         :burst-cols (persistent! b-cols)
         :winners-by-col (persistent! wbc)}
        ))))

(defn within-column-cell-exc
  "Calculates cell excitation values to be used to select cells within
  columns `a-cols`; they are compared only within each column, not
  across columns. Where no segments exist, the value is
  zero. Otherwise three cases are possible:

  * For predicted cells, the distal excitation (number of active
  synapses on the most active segment) is returned.

  * A positive value is given for cells with partially-matching
  segments. This applies if any segments exist with at least
  `:seg-learn-threshold` active synapses -- even if the synapses are
  not yet connected (below the permanence threshold).  The value is
  chosen to be lower than any connected active segment: half the
  `:seg-learn-threshold`.

  * A negative value is given for cells with only inactive distal
  segments. We need this to encourage context-specific choice of cells
  in a column: avoid cells that are missing their learned context
  signal (segment). (As a biological justification, perhaps more
  segments create a larger cell surface area to lose potential?)

  The value is negatively proportional to the number of segments:
  again the unit amount is half the `:seg-learn-threshold`.

  Returns a map of cell ids to these relative excitation values."
  [a-cols prev-wbc distal-sg aci distal-exc min-act depth]
  (let [adj-base-amount (quot min-act 2)]
    (->> (for [col a-cols
               :let [prev-wc (get prev-wbc col)]
               ci (range depth)
               :let [cell-id [col ci]
                     cell-segs (->> (p/cell-segments distal-sg cell-id)
                                    (filter seq))
                     n-segs (count cell-segs)]
               :when (pos? n-segs)]
           (let [d-exc (distal-exc cell-id)]
             (cond
               ;; predicted cell, use distal excitation
               d-exc
               [cell-id d-exc]
               ;; continuing learning cell
               (= prev-wc cell-id)
               [cell-id adj-base-amount]
               ;; some segment matches the input even if synapses disconnected
               (first (best-matching-segment cell-segs aci min-act 0.0))
               [cell-id adj-base-amount]
               ;; there are segments but none match the input; apply penalty
               :else
               [cell-id (* -1 adj-base-amount n-segs)]
               )))
         (into {}))))

;;; ## Learning

(defn new-segment-id
  "Returns a segment index on the cell at which to grow a new segment,
  together with any existing synapses at that index. It may refer to
  the end of the existing vector to append to it, or it may refer to
  an existing segment that is to be culled before the new one
  grows. If the maximum number of segments has been reached, an
  existing one is chosen to be replaced based on having the fewest
  connected synapses, or fewest synapses to break ties."
  [segs pcon max-segs max-syns]
  (let [segs (take-while seq segs)]
    (if (>= (count segs) max-segs)
      ;; select the one with fewest connected, or fewest synapses, or first
      (let [si (apply min-key (fn [si]
                                (let [syns (nth segs si)
                                      n-conn (count-filter #(>= % pcon) (vals syns))]
                                  (+ (* n-conn max-syns)
                                     (count syns)
                                     (/ si (count segs)))))
                      (range (count segs)))]
        [si (nth segs si)])
      ;; have not reached limit; append
      [(count segs) nil])))

(defn segment-new-synapse-source-ids
  "Returns a collection of up to n ids chosen from the learnable cell
  bits `lci-vec`. May be less than `n` if the random samples have
  duplicates or some already exist on the segment, or if there are
  fewer than `n` learnable cells."
  [seg lci-vec n rng]
  (when (seq lci-vec)
    (->> lci-vec
         (util/sample rng n)
         (distinct)
         (remove seg))))

(defn segment-excess-synapse-source-ids
  "Given that an additional `grow-n` synapses will be added, checks if
  the segment will exceed the maximum allowed number of synapses, and
  if so, returns a list of synapse source ids to remove. These are the
  ones with lowest permanence."
  [syns grow-n max-syns]
  (let [total (+ (count syns) grow-n)
        excess (- total max-syns)]
    (if (pos? excess)
      (->> (sort-by val syns)
           (take excess)
           (map first))
      nil)))

(defn punish-failures
  "To punish segments which predicted activation on cells which did
  not become active. Ignores any which are still predictive.  Returns
  a sequence of SegUpdate records."
  [distal-sg prior-pc pc ac prior-aci pcon stimulus-th]
  (let [bad-cells (set/difference prior-pc
                                  pc
                                  ac)]
    (for [cell-id bad-cells
          :let [cell-segs (p/cell-segments distal-sg cell-id)]
          si (cell-active-segments cell-segs prior-aci stimulus-th pcon)
          :let [seg-path (conj cell-id si)]]
      (syn/seg-update seg-path :punish nil nil))))

(defn segment-learning-map
  "Takes the learning cells `lc` and maps each to a SegUpdate record,
  which includes the segment path to learn on, together with lists of
  any synapse sources to add or remove. The segment index is chosen as
  the best matching one, but if none match sufficiently then a new
  segment will be grown, perhaps replacing an existing one. `aci` is
  the set of active source indices, used to find a matching segment,
  while `lci` is the set of learnable source indices, used to grow new
  synapses. If `poor-match?` returns true for a cell id then
  unconnected synapses are used to find a matching segment. Otherwise
  only connected synapses are used.

  Note that ''cell-ids'' here may also refer to columns in a proximal
  synapse graph, where the convention is [col 0]. Everything else is
  the same since proximal synapses graphs can also have multiple
  segments [col 0 seg-idx]."
  [rng lc well-matching-paths sg aci lci {:keys [pcon
                                                 min-act
                                                 new-syns
                                                 max-syns
                                                 max-segs]}]
  (let [lci-vec (vec lci)] ;; for faster sampling
    (loop [cells (seq lc)
           m (transient {})
           rng rng]
      (if-let [cell-id (first cells)]
        (if-let [seg-path (well-matching-paths cell-id)]
          ;; choose the well matching segment
          (recur (next cells)
                 (assoc! m cell-id (syn/seg-update seg-path :learn nil nil))
                 rng)
          ;; otherwise - not well matching - check disconnected synapses
          (let [cell-segs (p/cell-segments sg cell-id)
                [match-si exc seg] (best-matching-segment cell-segs aci
                                                          min-act 0.0)
                new-segment? (not match-si)
                [seg-idx die-syns] (if match-si
                                     [match-si nil]
                                     (new-segment-id cell-segs pcon max-segs
                                                     max-syns))
                grow-n (- new-syns exc)
                [rng* rng] (if (pos? grow-n) (random/split rng) [rng rng])
                grow-source-ids (segment-new-synapse-source-ids seg lci-vec grow-n rng*)
                die-source-ids (if new-segment?
                                 (keys die-syns) ;; remove any existing (replaced)
                                 (segment-excess-synapse-source-ids seg grow-n
                                                                    max-syns))
                seg-path (conj cell-id seg-idx)]
            (recur (next cells)
                   ;; if not enough learnable sources to grow a new segment, skip it
                   (if (and new-segment?
                            (< (count grow-source-ids) min-act))
                     m ;; skip
                     (assoc! m cell-id (syn/seg-update seg-path :learn grow-source-ids
                                                       die-source-ids)))
                   rng)))
        ;; finished
        (persistent! m)))))

;;; ## Orchestration

(defn update-inhibition-radius
  [layer]
  (assoc layer :inh-radius
         (inh/inhibition-radius (:proximal-sg layer) (:topology layer)
                                (:input-topology layer))))

(defn decay-tp
  [tp-exc fall]
  (persistent!
   (reduce-kv (fn [m id exc]
                ;; constant fall amount
                (let [e (- exc fall)]
                  (if (pos? e)
                    (assoc! m id e)
                    m)))
              (transient {})
              tp-exc)))

(defrecord LayerActiveState
    [in-ff-bits in-stable-ff-bits
     out-ff-bits out-stable-ff-bits
     col-overlaps matching-ff-seg-paths well-matching-ff-seg-paths
     temporal-pooling-exc
     active-cols burst-cols active-cells winners-by-col timestep])

(defrecord LayerDistalState
    [distal-bits distal-lc-bits distal-exc pred-cells prior-active-cells
     matching-seg-paths well-matching-seg-paths timestep])

(def empty-active-state
  (map->LayerActiveState
   {:winners-by-col {}
    :active-cells #{}
    :active-cols #{}
    :temporal-pooling-exc {}
    :well-matching-ff-seg-paths {}}))

(def empty-distal-state
  (map->LayerDistalState
   {:distal-bits #{}
    :pred-cells #{}
    :distal-exc {}
    :well-matching-seg-paths {}}))

(defrecord LayerOfCells
    [spec rng topology input-topology inh-radius boosts active-duty-cycles
     proximal-sg distal-sg state distal-state prior-distal-state]
  p/PLayerOfCells
  (layer-activate
    [this ff-bits stable-ff-bits]
    (let [;; proximal excitation in number of active synapses, keyed by [col 0 seg-idx]
          col-seg-overlaps (p/excitations proximal-sg ff-bits
                                          (:ff-stimulus-threshold spec))
          ;; these all keyed by [col 0]
          [raw-col-exc ff-seg-paths ff-good-paths]
          (best-segment-excitations-and-paths col-seg-overlaps
                                              (:ff-seg-new-synapse-count spec))
          ;; stable inputs (from predicted source cells)
          stable-col-seg-overlaps (p/excitations proximal-sg stable-ff-bits
                                                 (:ff-stimulus-threshold spec))
          stable-col-exc (best-segment-excitations stable-col-seg-overlaps)
          ;; filter inputs depending on stability
          ;; also check for clear matches, these override pooling
          higher-level? (> (:ff-max-segments spec) 1)
          engaged? (or (not higher-level?)
                       (> (count stable-ff-bits)
                          (* (count ff-bits) (:stable-inbit-frac-threshold spec))))
          newly-engaged? (or (not higher-level?)
                             (and engaged? (not (:engaged? state))))
          tp-exc (cond-> (if newly-engaged?
                           {}
                           (:temporal-pooling-exc state))
                   true ;(not engaged?)
                   (decay-tp (:temporal-pooling-fall spec)))
          col-exc (cond-> raw-col-exc
                         (not engaged?)
                         (select-keys (keys ff-good-paths))
                         true
                         (columns/apply-overlap-boosting boosts))
          ;; combine excitation values for selecting columns
          abs-cell-exc (total-excitations col-exc tp-exc
                                          (:distal-exc distal-state)
                                          (:distal-vs-proximal-weight spec)
                                          (:spontaneous-activation? spec)
                                          (:depth spec))
          activation-level (let [base-level (:activation-level spec)
                                 prev-level (/ (count (:active-cols state))
                                               (p/size-of this))]
                             (if (or newly-engaged? (not engaged?))
                               base-level
                               (min (:activation-level-max spec)
                                    (+ prev-level (* 0.5 base-level)))))
          a-cols (select-active-columns (best-by-column abs-cell-exc)
                                        topology activation-level
                                        inh-radius spec)
          ;; calculate relative excitations for cells within each active column:
          ;; * include distal excitation on predicted cells.
          ;; * matching segments below connected threshold get a bonus.
          ;; * cells with inactive segments get a penalty.
          rel-cell-exc (->> (within-column-cell-exc a-cols
                                                    (:winners-by-col state)
                                                    distal-sg
                                                    (:distal-bits distal-state)
                                                    (:distal-exc distal-state)
                                                    (:seg-learn-threshold spec)
                                                    (:depth spec))
                            (merge-with + tp-exc))
          ;; find active and winner cells in the columns
          [rng* rng] (random/split rng)
          {ac :active-cells
           wbc :winners-by-col
           b-cols :burst-cols
           stable-ac :stable-active-cells}
          (select-active-cells a-cols rel-cell-exc
                               (:winners-by-col state) ;; keep winners stable
                               spec rng*)
          ;; learning cells are the winning cells, but excluding any
          ;; continuing winners when temporal pooling
          old-winners (vals (:winners-by-col state))
          new-winners (vals wbc)
          learning (if newly-engaged? ;; always true at first level
                     new-winners
                     (remove (set old-winners) new-winners))
          ;; update continuing TP activation
          next-tp-exc (if higher-level?
                        (let [new-ac (if newly-engaged?
                                       ac
                                       (set/difference ac (:active-cells state)))]
                          (into tp-exc
                               (map vector new-ac
                                    (repeat (:temporal-pooling-max-exc spec)))))
                        {})
          depth (:depth spec)]
      (assoc this
             :rng rng
             :state (map->LayerActiveState
                     {:in-ff-bits ff-bits
                      :in-stable-ff-bits stable-ff-bits
                      :out-ff-bits (set (cells->bits depth ac))
                      :out-stable-ff-bits (set (cells->bits depth stable-ac))
                      :engaged? engaged?
                      :newly-engaged? newly-engaged?
                      :col-overlaps raw-col-exc
                      :matching-ff-seg-paths ff-seg-paths
                      :well-matching-ff-seg-paths ff-good-paths
                      :temporal-pooling-exc next-tp-exc
                      :active-cells ac
                      :active-cols a-cols
                      :burst-cols b-cols
                      :winners-by-col wbc
                      :learning-cells learning
                      :timestep (inc (:timestep state))
                      }))))

  (layer-learn
    [this]
    (let [prior-aci (:distal-bits distal-state)
          prior-lci (:distal-lc-bits distal-state)
          lc (:learning-cells state)
          [rng* rng] (random/split rng)
          distal-learning (segment-learning-map rng* lc
                                                (:well-matching-seg-paths distal-state)
                                                distal-sg prior-aci prior-lci
                                                {:pcon (:distal-perm-connected spec)
                                                 :min-act (:seg-learn-threshold spec)
                                                 :new-syns (:seg-new-synapse-count spec)
                                                 :max-syns (:seg-max-synapse-count spec)
                                                 :max-segs (:max-segments spec)})
          distal-punishments (if (:distal-punish? spec)
                               (punish-failures distal-sg
                                                (:pred-cells prior-distal-state)
                                                (:pred-cells distal-state)
                                                (:prior-active-cells distal-state)
                                                (:distal-bits prior-distal-state)
                                                (:distal-perm-connected spec)
                                                (:seg-stimulus-threshold spec))
                               nil)
          dsg (cond-> distal-sg
                (seq distal-learning)
                (p/bulk-learn (vals distal-learning) prior-lci
                                (:distal-perm-inc spec) (:distal-perm-dec spec)
                                (:distal-perm-init spec))
                distal-punishments
                (p/bulk-learn distal-punishments (:distal-bits prior-distal-state)
                              (:distal-perm-inc spec) (:distal-perm-punish spec)
                              (:distal-perm-init spec)))
          higher-level? (> (:ff-max-segments spec) 1)
          a-cols (:active-cols state)
          [rng* rng] (random/split rng)
          prox-learning (when (:engaged? state)
                          (segment-learning-map rng* (map vector a-cols (repeat 0))
                                                (:well-matching-ff-seg-paths state)
                                                proximal-sg
                                                (:in-ff-bits state)
                                                (if higher-level?
                                                  (:in-stable-ff-bits state)
                                                  (:in-ff-bits state))
                                                {:pcon (:ff-perm-connected spec)
                                                 :min-act (:ff-seg-learn-threshold spec)
                                                 :new-syns (:ff-seg-new-synapse-count spec)
                                                 :max-syns (:ff-seg-max-synapse-count spec)
                                                 :max-segs (:ff-max-segments spec)}))
          psg (cond-> proximal-sg
                prox-learning
                (p/bulk-learn (vals prox-learning)
                                (:in-ff-bits state)
                                (:ff-perm-inc spec) (:ff-perm-dec spec)
                                (:ff-perm-init-hi spec))
                ;; positive learning rate is higher for stable (predicted) inputs
                (and prox-learning
                     (seq (:in-stable-ff-bits state))
                     (> (:ff-perm-stable-inc spec) (:ff-perm-inc spec)))
                (p/bulk-learn (map #(syn/seg-update (:target-id %) :reinforce nil nil)
                                   (vals prox-learning))
                              (:in-stable-ff-bits state)
                              (- (:ff-perm-stable-inc spec) (:ff-perm-inc spec))
                              (:ff-perm-dec spec)
                              (:ff-perm-init-hi spec)))
          timestep (:timestep state)]
      (cond->
       (assoc this
              :rng rng
              :state (assoc state
                            :distal-learning distal-learning
                            :distal-punishments distal-punishments
                            :proximal-learning prox-learning)
              :distal-sg dsg
              :proximal-sg psg)
       true (update-in [:active-duty-cycles] columns/update-duty-cycles
                       (:active-cols state) (:duty-cycle-period spec))
       (zero? (mod timestep (:boost-active-every spec))) (columns/boost-active)
       (zero? (mod timestep (:inh-radius-every spec))) (update-inhibition-radius))))

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
          wc (vals (:winners-by-col state))
          ;; possibly should pass in separate learnable bit sets as arguments
          lci (util/align-indices widths
                                  [(if (:lateral-synapses? spec)
                                     (cells->bits depth wc)
                                     [])
                                   distal-ff-bits
                                   (if (:use-feedback? spec) distal-fb-bits [])])
          seg-exc (p/excitations distal-sg aci (:seg-stimulus-threshold spec))
          [distal-exc seg-paths good-paths] (best-segment-excitations-and-paths
                                             seg-exc (:seg-new-synapse-count spec))
          pc (set (keys distal-exc))]
      (assoc this
        :prior-distal-state distal-state
        :distal-state (map->LayerDistalState
                       {:distal-bits (set aci)
                        :distal-lc-bits (set lci)
                        :matching-seg-paths seg-paths
                        :well-matching-seg-paths good-paths
                        :distal-exc distal-exc
                        :prior-active-cells (:active-cells state)
                        :pred-cells pc
                        :timestep (:timestep state)}))))

  (layer-depth [_]
    (:depth spec))
  (bursting-columns [_]
    (:burst-cols state))
  (active-columns [_]
    (:active-cols state))
  (active-cells [_]
    (:active-cells state))
  (winner-cells [_]
    (set (vals (:winners-by-col state))))
  (temporal-pooling-cells [_]
    (when (:engaged? state)
      (keys (:temporal-pooling-exc state))))
  (predictive-cells [_]
    (when (== (:timestep state)
              (:timestep distal-state))
      (:pred-cells distal-state)))
  (prior-predictive-cells [_]
    (let [t-1 (dec (:timestep state))]
      (cond
        (== t-1 (:timestep prior-distal-state)) (:pred-cells prior-distal-state)
        (== t-1 (:timestep distal-state)) (:pred-cells distal-state))))

  p/PInterruptable
  (break [this mode]
    (case mode
      :tm (assoc this :distal-state
                 (assoc empty-distal-state :timestep (:timestep state)))
      :tp (update-in this [:state :temporal-pooling-exc] empty)))

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
  (timestep [_]
    (:timestep state))
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
        [rng rng*] (-> (random/make-random (:random-seed spec))
                       (random/split))
        col-prox-syns (columns/uniform-ff-synapses col-topo input-topo
                                                   spec rng*)
        proximal-sg (syn/col-segs-synapse-graph col-prox-syns n-cols
                                                (:ff-max-segments spec)
                                                (p/size input-topo)
                                                (:ff-perm-connected spec)
                                                false)
        distal-sg (syn/cell-segs-synapse-graph n-cols depth
                                               (:max-segments spec)
                                               n-distal
                                               (:distal-perm-connected spec)
                                               true)
        state (assoc empty-active-state :timestep 0)
        distal-state (assoc empty-distal-state :timestep 0)]
    (->
     (map->LayerOfCells
      {:spec spec
       :rng rng
       :topology col-topo
       :input-topology input-topo
       :inh-radius 1
       :proximal-sg proximal-sg
       :distal-sg distal-sg
       :state state
       :distal-state distal-state
       :prior-distal-state distal-state
       :boosts (vec (repeat n-cols 1.0))
       :active-duty-cycles (vec (repeat n-cols 0.0))
       })
     (update-inhibition-radius))))
