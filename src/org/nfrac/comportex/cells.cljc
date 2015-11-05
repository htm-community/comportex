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

(def dendrite-parameter-defaults
  "Default parameters for distal dendrite segments. The
  same parameters are also used for proximal segments, but with
  different default values.

  * `max-segments` - maximum number of dendrites segments per cell (or
  column for proximal dendrites).

  * `max-synapse-count` - maximum number of synapses per segment.

  * `new-synapse-count` - number of synapses on a new dendrite
  segment.

  * `stimulus-threshold` - minimum number of active synapses on a
  segment for it to become active.

  * `learn-threshold` - minimum number of active synapses on a segment
  for it to be reinforced and extended if it is the best matching.

  * `perm-inc` - amount by which to increase synapse permanence to
  active sources when reinforcing a segment.

  * `perm-stable-inc` - amount by which to increase a synapse
  permanence to stable (predicted) sources.

  * `perm-dec` - amount by which to decrease synapse permanence to
  inactive sources when reinforcing a segment.

  * `perm-punish` - amount by which to decrease synapse permanence
  when punishing segments in case of failed prediction.

  * `perm-connected` - permanence value at which a synapse is
  functionally connected. Permanence values are defined to be between
  0 and 1.

  * `perm-init` - permanence value for new synapses on segments.

  * `punish?` - whether to reduce synapse permanence on segments
  incorrectly predicting activation.

  * `learn?` - whether to reinforce and grow synapses.
"
  {:max-segments 5
   :max-synapse-count 22
   :new-synapse-count 12
   :stimulus-threshold 9
   :learn-threshold 7
   :perm-inc 0.05
   :perm-stable-inc 0.05
   :perm-dec 0.01
   :perm-punish 0.002
   :perm-connected 0.20
   :perm-init 0.16
   :punish? true
   :learn? true
   })

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

  * `ff-perm-init-hi` - highest initial permanence value on new synapses.

  * `ff-perm-init-lo` - lowest initial permanence value on new synapses.

  * `proximal` - map of parameters for proximal dendrite segments,
  see `dendrite-parameter-defaults`.

  *  `distal` - map of parameters for distal dendrite segments,
  see `dendrite-parameter-defaults`.

  *  `apical` - map of parameters for apical dendrite segments,
  see `dendrite-parameter-defaults`. Ignored unless :use-feedback?

  * `max-boost` - ceiling on the column boosting factor used to
  increase activation frequency.

  * `duty-cycle-period` - number of time steps to average over when
  updating duty cycles and (thereby) column boosting measures.

  * `boost-active-duty-ratio` - when a column's activation frequency
  is below this proportion of the _highest_ of its neighbours, its
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

  * `activation-level` - fraction of columns that can be
  active (either locally or globally); inhibition kicks in to reduce
  it to this level. Does not apply to temporal pooling.

  * `activation-level-max` - maximum fraction of columns that can be
  active as temporal pooling progresses. Each step of continued
  pooling allocates an extra 50% of `activation-level` until this
  maximum is reached.

  * `global-inhibition?` - whether to use the faster global algorithm
  for column inhibition (just keep those with highest overlap scores),
  or to apply local inhibition (only within a column's neighbours).

  * `inhibition-base-distance` - the distance in columns within which
  a cell *will always* inhibit neighbouring cells with lower
  excitation. Ignored if `global-inhibition?` is true.

  * `distal-vs-proximal-weight` - scaling to apply to the number of
  active distal synapses (on the winning segment) before adding to the
  number of active proximal synapses, when selecting active
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
  temporal pooling excitation falls each time step.

  * `random-seed` - the random seed (for reproducible results).
"
  {:input-dimensions [:define-me!]
   :column-dimensions [1000]
   :depth 5
   :ff-potential-radius 1.0
   :ff-init-frac 0.25
   :ff-perm-init-hi 0.25
   :ff-perm-init-lo 0.10
   :proximal {:max-segments 1
              :max-synapse-count 300
              :new-synapse-count 12
              :stimulus-threshold 2
              :learn-threshold 7
              :perm-inc 0.04
              :perm-stable-inc 0.15
              :perm-dec 0.01
              :perm-connected 0.20
              :perm-init 0.25
              :learn? true
              }
   :distal (assoc dendrite-parameter-defaults
                  :learn? true)
   :apical (assoc dendrite-parameter-defaults
                  :learn? false)
   :max-boost 1.5
   :duty-cycle-period 1000
   :boost-active-duty-ratio 0.001
   :boost-active-every 1000
   :inh-radius-every 1000
   :lateral-synapses? true
   :distal-motor-dimensions [0]
   :distal-topdown-dimensions [0]
   :use-feedback? false
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
         :distal (assoc dendrite-parameter-defaults
                        :max-segments 8
                        :max-synapse-count 32
                        :new-synapse-count 20
                        :stimulus-threshold 13
                        :learn-threshold 10
                        )))

;;; ## Synapse tracing

(defn distal-sources-widths
  [spec]
  [(if (:lateral-synapses? spec)
     (reduce * (:depth spec) (:column-dimensions spec))
     0)
   (reduce * (:distal-motor-dimensions spec))])

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
  "Returns a vector [k v] where k is one of :this or :ff. In the
   case of :this, v is [col ci], otherwise v gives the index in the
   feed-forward distal input field."
  [spec id]
  (let [[this-w ff-w] (distal-sources-widths spec)]
    (cond
     (< id this-w) [:this (id->cell (:depth spec) id)]
     (< id (+ this-w ff-w)) [:ff (- id this-w)])))

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
  synapses with permanence values at or above `pcon`. `aci` are active bits.
  Returns
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
                        (into (zipmap (keys distal-exc) (repeat 0.0))
                              basic-exc)
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
  [col cell-exc prior-winner depth threshold dominance-margin rng reset?]
  (let [cell-ids (for [ci (range depth)] [col ci])]
    (loop [ids cell-ids
           best-ids ()
           best-exc -99999.9
           good-ids () ;; over threshold
           second-exc (double threshold)]
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
                 (if new-best?
                   (max best-exc second-exc) ;; think best-exc (second-exc just for init)
                   (if (< second-exc exc best-exc) ;; between second & best
                     exc
                     second-exc))))
        ;; finished
        (let [winner (cond
                       (== (count best-ids) 1)
                       (first best-ids)
                       (and prior-winner (some #(= % prior-winner) best-ids))
                       prior-winner
                       reset?
                       (first cell-ids)
                       :else
                       (util/rand-nth rng best-ids))
              actives (cond
                        ;; stimulus threshold not reached
                        (< best-exc threshold)
                        cell-ids
                        ;; best cells exceed all others by dominance-margin
                        (>= (- best-exc second-exc) dominance-margin)
                        best-ids
                        ;; otherwise, all cells over threshold become active
                        :else
                        good-ids)]
          [winner actives])))))

(defn select-active-cells
  "Determines active cells in the given columns and whether they are bursting.
   Returns keys
  * `:active-cells` - the set of active cell ids.
  * `:stable-active-cells` - the set of non-bursting active cells.
  * `:burst-cols` - the set of bursting column ids.
  * `:col-winners` - the map of column id to winner cell id."
  [a-cols cell-exc bursting? prior-col-winners reset?
   depth threshold dominance-margin rng]
  (loop [cols (seq a-cols)
         ac (transient #{}) ;; active cells
         sac (transient #{}) ;; stable active cells
         b-cols (transient #{}) ;; bursting columns
         col-winners (transient {})
         rng rng]
    (if-let [col (first cols)]
      (let [[rng rng*] (random/split rng)
            ;; carry forward learning cells for higher level sequences
            prior-winner (get prior-col-winners col)
            [win-cell col-ac] (column-active-cells col cell-exc prior-winner
                                                   depth threshold dominance-margin
                                                   rng* reset?)
            b-col? (bursting? col win-cell col-ac)
            next-ac (reduce conj! ac col-ac)
            next-sac (if b-col?
                       sac
                       (reduce conj! sac col-ac))]
        (recur (next cols)
               next-ac
               next-sac
               (if b-col? (conj! b-cols col) b-cols)
               (assoc! col-winners col win-cell)
               rng))
      ;; finished
      {:active-cells (persistent! ac)
       :stable-active-cells (persistent! sac)
       :burst-cols (persistent! b-cols)
       :col-winners (persistent! col-winners)}
      )))

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
  [a-cols prior-col-winners distal-sg apical-sg distal-bits apical-bits
   distal-apical-exc distal-min-act apical-min-act depth]
  (let [adj-base-amount (quot distal-min-act 2)]
    (->> (for [col a-cols
               :let [prior-wc (get prior-col-winners col)]
               ci (range depth)
               :let [cell-id [col ci]
                     d-cell-segs (->> (p/cell-segments distal-sg cell-id)
                                      (filter seq))
                     a-cell-segs (->> (p/cell-segments apical-sg cell-id)
                                      (filter seq))
                     n-segs (+ (count d-cell-segs) (count a-cell-segs))]
               :when (pos? n-segs)]
           (let [d-exc (distal-apical-exc cell-id)]
             (cond
               ;; predicted cell, use distal excitation
               d-exc
               [cell-id d-exc]
               ;; continuing winner cell
               (= prior-wc cell-id)
               [cell-id adj-base-amount]
               ;; some segment matches the input even if synapses disconnected
               (or
                (first (best-matching-segment d-cell-segs distal-bits distal-min-act 0.0))
                (first (best-matching-segment a-cell-segs apical-bits apical-min-act 0.0)))
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

(defn segment-punishments
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
  [rng lc well-matching-paths sg aci lci {pcon :perm-connected
                                          min-act :learn-threshold
                                          new-syns :new-synapse-count
                                          max-syns :max-synapse-count
                                          max-segs :max-segments}]
  (let [lci-vec (vec lci)] ;; for faster sampling
    (loop [cells (seq lc)
           m (transient {})
           rng rng]
      (if-let [cell-id (first cells)]
        (if-let [seg-path (get well-matching-paths cell-id)]
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

(defn learn-distal
  [sg distal-state lc dspec rng]
  (let [learning (segment-learning-map rng lc
                                       (:well-matching-seg-paths distal-state)
                                       sg (:on-bits distal-state)
                                       (:on-lc-bits distal-state)
                                       dspec)
        new-sg (if (seq learning)
                 (p/bulk-learn sg (vals learning) (:on-bits distal-state)
                               (:perm-inc dspec) (:perm-dec dspec)
                               (:perm-init dspec))
                 sg)]
    [new-sg
     learning]))

(defn punish-distal
  [sg distal-state prior-distal-state dspec]
  (let [punishments (segment-punishments sg
                                         (:pred-cells prior-distal-state)
                                         (:pred-cells distal-state)
                                         (:prior-active-cells distal-state)
                                         (:on-bits prior-distal-state)
                                         (:perm-connected dspec)
                                         (:stimulus-threshold dspec))
        new-sg (if punishments
                 (p/bulk-learn sg punishments (:on-bits prior-distal-state)
                               (:perm-inc dspec) (:perm-punish dspec)
                               (:perm-init dspec))
                 sg)]
    [new-sg
     punishments]))

(defn layer-learn-lateral
  [this lc]
  (let [sg (:distal-sg this)
        dstate (:distal-state this)
        dspec (:distal (:spec this))
        [rng* rng] (random/split (:rng this))
        [new-sg learning] (learn-distal sg dstate lc dspec rng*)]
    (assoc this
           :rng rng
           :state (assoc (:state this) :distal-learning learning)
           :distal-sg new-sg)))

(defn layer-learn-apical
  [this lc]
  (let [sg (:apical-sg this)
        dstate (:apical-state this)
        dspec (:apical (:spec this))
        [rng* rng] (random/split (:rng this))
        [new-sg learning] (learn-distal sg dstate lc dspec rng*)]
    (assoc this
           :rng rng
           :state (assoc (:state this) :apical-learning learning)
           :apical-sg new-sg)))

(defn layer-punish-lateral
  [this]
  (let [sg (:distal-sg this)
        dstate (:distal-state this)
        pdstate (:prior-distal-state this)
        dspec (:distal (:spec this))
        [new-sg punishments] (punish-distal sg dstate pdstate dspec)]
    (assoc this
           :state (assoc (:state this) :distal-punishments punishments)
           :distal-sg new-sg)))

(defn layer-punish-apical
  [this]
  (let [sg (:apical-sg this)
        dstate (:apical-state this)
        pdstate (:prior-apical-state this)
        dspec (:apical (:spec this))
        [new-sg punishments] (punish-distal sg dstate pdstate dspec)]
    (assoc this
           :state (assoc (:state this) :apical-punishments punishments)
           :apical-sg new-sg)))

(defn layer-learn-proximal
  [this cols]
  (let [sg (:proximal-sg this)
        state (:state this)
        pspec (:proximal (:spec this))
        higher-level? (> (:max-segments pspec) 1)
        [rng* rng] (random/split (:rng this))
        prox-learning (segment-learning-map rng*
                                            (map vector cols (repeat 0))
                                            (:well-matching-ff-seg-paths state)
                                            sg
                                            (:in-ff-bits state)
                                            (if higher-level?
                                              (:in-stable-ff-bits state)
                                              (:in-ff-bits state))
                                            pspec)
        psg (cond-> sg
              (seq prox-learning)
              (p/bulk-learn (vals prox-learning) (:in-ff-bits state)
                            (:perm-inc pspec) (:perm-dec pspec)
                            (:perm-init pspec))
              ;; positive learning rate is higher for stable (predicted) inputs
              (and (seq prox-learning)
                   (seq (:in-stable-ff-bits state))
                   (> (:perm-stable-inc pspec) (:perm-inc pspec)))
              (p/bulk-learn (map #(syn/seg-update (:target-id %) :reinforce nil nil)
                                 (vals prox-learning))
                            (:in-stable-ff-bits state)
                            (- (:perm-stable-inc pspec) (:perm-inc pspec))
                            (:perm-dec pspec)
                            (:perm-init pspec)))]
    (assoc this
           :rng rng
           :state (assoc state :proximal-learning prox-learning)
           :proximal-sg psg)))

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
     active-cols burst-cols active-cells col-winners timestep])

(defrecord LayerDistalState
    [on-bits on-lc-bits cell-exc pred-cells
     matching-seg-paths well-matching-seg-paths
     prior-active-cells timestep])

(def empty-active-state
  (map->LayerActiveState
   {:col-winners {}
    :active-cells #{}
    :active-cols #{}
    :temporal-pooling-exc {}
    :well-matching-ff-seg-paths {}}))

(def empty-distal-state
  (map->LayerDistalState
   {:on-bits #{}
    :cell-exc {}
    :pred-cells #{}
    :well-matching-seg-paths {}}))

(defn compute-distal-state
  [sg aci lci dspec t]
  (let [seg-exc (p/excitations sg aci (:stimulus-threshold dspec))
        [cell-exc seg-paths good-paths] (best-segment-excitations-and-paths
                                         seg-exc (:new-synapse-count dspec))
        pc (set (keys cell-exc))]
    (map->LayerDistalState
     {:on-bits (set aci)
      :on-lc-bits (set lci)
      :cell-exc cell-exc
      :matching-seg-paths seg-paths
      :well-matching-seg-paths good-paths
      :pred-cells pc
      :timestep t})))

(defrecord LayerOfCells
    [spec rng topology input-topology inh-radius boosts active-duty-cycles
     proximal-sg distal-sg apical-sg state distal-state prior-distal-state
     apical-state prior-apical-state]
  p/PLayerOfCells
  (layer-activate
    [this ff-bits stable-ff-bits]
    (let [pspec (:proximal spec)
          ;; proximal excitation in number of active synapses, keyed by [col 0 seg-idx]
          col-seg-overlaps (p/excitations proximal-sg ff-bits
                                          (:stimulus-threshold pspec))
          ;; these all keyed by [col 0]
          [raw-col-exc ff-seg-paths ff-good-paths]
          (best-segment-excitations-and-paths col-seg-overlaps
                                              (:new-synapse-count pspec))
          ;; temporal pooling, depending on stability of input bits.
          ;; also check for clear matches, these override pooling
          higher-level? (> (:max-segments pspec) 1)
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
          ;; unlike other segments, allow apical excitation to add to distal
          d-a-cell-exc (if (:use-feedback? spec)
                         (merge-with + (:cell-exc distal-state)
                                     (:cell-exc apical-state))
                         (:cell-exc distal-state))
          ;; combine excitation values for selecting columns
          abs-cell-exc (total-excitations col-exc tp-exc d-a-cell-exc
                                          (:distal-vs-proximal-weight spec)
                                          (:spontaneous-activation? spec)
                                          (:depth spec))
          ;; union temporal pooling: accrete more columns as pooling continues
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
          ;; keep winners stable only at higher levels (first level needs to see repeats)
          prior-col-winners (if higher-level? (:col-winners state))
          ;; calculate relative excitations for cells within each active column:
          ;; * include distal excitation on predicted cells.
          ;; * matching segments below connected threshold get a bonus.
          ;; * cells with inactive segments get a penalty.
          rel-cell-exc (->> (within-column-cell-exc a-cols
                                                    prior-col-winners
                                                    distal-sg apical-sg
                                                    (:on-bits distal-state)
                                                    (:on-bits apical-state)
                                                    d-a-cell-exc
                                                    (:learn-threshold (:distal spec))
                                                    (:learn-threshold (:apical spec))
                                                    (:depth spec))
                            (merge-with + tp-exc))
          ;; find active and winner cells in the columns
          pc (set/union (:pred-cells distal-state) (:pred-cells apical-state))
          depth (:depth spec)
          [rng* rng] (random/split rng)
          {ac :active-cells
           col-winners :col-winners
           burst-cols :burst-cols
           stable-ac :stable-active-cells}
          (select-active-cells a-cols rel-cell-exc
                               ;; definition of bursting for a column
                               (fn [col win-cell col-ac]
                                 (if (and (not newly-engaged?)
                                          (= win-cell (get prior-col-winners col)))
                                   ;; for continuing temporal pooling
                                   (== depth (count col-ac))
                                   ;; otherwise: for discrete transitions
                                   (not (or (pc win-cell) (tp-exc win-cell)))))
                               prior-col-winners
                               (empty? (:on-bits distal-state)) ;; reset?
                               depth (:stimulus-threshold (:distal spec))
                               (:dominance-margin spec) rng*)
          ;; learning cells are the winning cells, but excluding any
          ;; continuing winners when temporal pooling
          old-winners (vals (:col-winners state))
          new-winners (vals col-winners)
          learning (if newly-engaged? ;; always true at first level
                     new-winners
                     (remove (set old-winners) new-winners))
          ;; update continuing TP activation
          next-tp-exc (if higher-level?
                        (let [new-ac (if newly-engaged?
                                       ac
                                       (set/difference ac (:active-cells state)))]
                          (into (select-keys tp-exc ac) ;; only keep TP for active cells
                               (map vector new-ac
                                    (repeat (:temporal-pooling-max-exc spec)))))
                        {})]
      (assoc this
             :rng rng
             :state (map->LayerActiveState
                     {:in-ff-bits ff-bits
                      :in-stable-ff-bits stable-ff-bits
                      :out-ff-bits (set (cells->bits depth ac))
                      :out-stable-ff-bits (set (cells->bits depth stable-ac))
                      :out-wc-bits (set (cells->bits depth (vals col-winners)))
                      :engaged? engaged?
                      :newly-engaged? newly-engaged?
                      :col-overlaps raw-col-exc
                      :matching-ff-seg-paths ff-seg-paths
                      :well-matching-ff-seg-paths ff-good-paths
                      :temporal-pooling-exc next-tp-exc
                      :active-cells ac
                      :active-cols a-cols
                      :burst-cols burst-cols
                      :col-winners col-winners
                      :learning-cells learning
                      :timestep (inc (:timestep state))
                      }))))

  (layer-learn
    [this]
    (let [lc (:learning-cells state)
          a-cols (:active-cols state)
          timestep (:timestep state)]
      (cond->
          (update this :state assoc
                  :proximal-learning {}
                  :distal-learning {}
                  :apical-learning {}
                  :distal-punishments ()
                  :apical-punishments ())
        (:learn? (:distal spec)) (layer-learn-lateral lc)
        (:learn? (:apical spec)) (layer-learn-apical lc)
        (:punish? (:distal spec)) (layer-punish-lateral)
        (:punish? (:apical spec)) (layer-punish-apical)
        (and (:engaged? state)
             (:learn? (:proximal spec))) (layer-learn-proximal a-cols)
        true (update-in [:active-duty-cycles] columns/update-duty-cycles
                        (:active-cols state) (:duty-cycle-period spec))
        (zero? (mod timestep (:boost-active-every spec))) (columns/boost-active)
        (zero? (mod timestep (:inh-radius-every spec))) (update-inhibition-radius))))

  (layer-depolarise
    [this distal-ff-bits apical-fb-bits apical-fb-wc-bits]
    (let [depth (:depth spec)
          widths (distal-sources-widths spec)
          distal-aci (util/align-indices widths
                                  [(if (:lateral-synapses? spec)
                                     (:out-ff-bits state)
                                     [])
                                   distal-ff-bits])
          wc (vals (:col-winners state))
          distal-lci (util/align-indices widths
                                  [(if (:lateral-synapses? spec)
                                     (cells->bits depth wc)
                                     [])
                                   distal-ff-bits])
          apical-aci (if (:use-feedback? spec) apical-fb-bits [])
          apical-lci (if (:use-feedback? spec) apical-fb-wc-bits [])]
      (assoc this
        :prior-distal-state distal-state
        :prior-apical-state apical-state
        :distal-state (->
                       (compute-distal-state distal-sg distal-aci distal-lci
                                             (:distal spec) (:timestep state))
                       (assoc :prior-active-cells (:active-cells state)))
        :apical-state (->
                       (compute-distal-state apical-sg apical-aci apical-lci
                                             (:apical spec) (:timestep state))
                       (assoc :prior-active-cells (:active-cells state))))))

  (layer-depth [_]
    (:depth spec))
  (bursting-columns [_]
    (:burst-cols state))
  (active-columns [_]
    (:active-cols state))
  (active-cells [_]
    (:active-cells state))
  (winner-cells [_]
    (set (vals (:col-winners state))))
  (temporal-pooling-cells [_]
    (when (:engaged? state)
      (keys (:temporal-pooling-exc state))))
  (predictive-cells [_]
    (when (== (:timestep state)
              (:timestep distal-state))
      (set/union (:pred-cells distal-state)
                 (:pred-cells apical-state))))
  (prior-predictive-cells [_]
    (let [t-1 (dec (:timestep state))]
      (cond
        ;; after depolarise phase has run
        (== t-1 (:timestep prior-distal-state))
        (set/union (:pred-cells prior-distal-state)
                   (:pred-cells prior-apical-state))
        ;; before depolarise phase has run
        (== t-1 (:timestep distal-state))
        (set/union (:pred-cells distal-state)
                   (:pred-cells apical-state)))))

  p/PInterruptable
  (break [this mode]
    (case mode
      :tm (assoc this :distal-state
                 (assoc empty-distal-state :timestep (:timestep state)))
      :fb (assoc this :apical-state
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
  p/PFeedBack
  (wc-bits-value [_]
    (:out-wc-bits state))
  p/PTemporal
  (timestep [_]
    (:timestep state))
  p/PParameterised
  (params [_]
    spec))

(defn layer-of-cells
  [spec]
  (let [spec (util/deep-merge parameter-defaults spec)
        input-topo (topology/make-topology (:input-dimensions spec))
        col-topo (topology/make-topology (:column-dimensions spec))
        n-cols (p/size col-topo)
        depth (:depth spec)
        n-distal (+ (if (:lateral-synapses? spec)
                      (* n-cols depth) 0)
                    (reduce * (:distal-motor-dimensions spec)))
        n-apical (reduce * (:distal-topdown-dimensions spec))
        [rng rng*] (-> (random/make-random (:random-seed spec))
                       (random/split))
        col-prox-syns (columns/uniform-ff-synapses col-topo input-topo
                                                   spec rng*)
        proximal-sg (syn/col-segs-synapse-graph col-prox-syns n-cols
                                                (:max-segments (:proximal spec))
                                                (p/size input-topo)
                                                (:perm-connected (:proximal spec))
                                                false)
        distal-sg (syn/cell-segs-synapse-graph n-cols depth
                                               (:max-segments (:distal spec))
                                               n-distal
                                               (:perm-connected (:distal spec))
                                               true)
        apical-sg (syn/cell-segs-synapse-graph n-cols depth
                                               (:max-segments (:apical spec))
                                               n-apical
                                               (:perm-connected (:apical spec))
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
       :apical-sg apical-sg
       :state state
       :distal-state distal-state
       :prior-distal-state distal-state
       :boosts (vec (repeat n-cols 1.0))
       :active-duty-cycles (vec (repeat n-cols 0.0))
       })
     (update-inhibition-radius))))
