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

  * `temporal-pooling-amp` - multiplier on cell excitation to become
  persistent temporal pooling.

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
   :temporal-pooling-amp 3.0
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
  "Finds the most excited dendrite segment for each cell. Returns
  `[cell-exc cell-seg-exc]` where

  * cell-exc is a map from cell-id to best excitation value.
  * cell-seg-exc is a map from cell-id to best [seg-path exc]."
  [seg-exc]
  (loop [seg-exc (seq seg-exc)
         excs (transient {})
         paths (transient {})]
    (if-let [[path exc] (first seg-exc)]
      (let [id (pop path) ;; seg-id to cell-id: [col ci _]
            prev-exc (get excs id 0.0)]
        (if (> exc prev-exc)
          (recur (next seg-exc)
                 (assoc! excs id exc)
                 (assoc! paths id [path exc]))
          (recur (next seg-exc)
                 excs
                 paths)))
      ;; finished
      [(persistent! excs)
       (persistent! paths)])))

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
  "Returns a sequence of cell ids to become active in the column.
  If no cells have excitation over the threshold, then all become
  active (bursting). Otherwise, only cells above the threshold become
  active; but if the top excitation exceeds all others by at least
  `dominance-margin` then the others are inhibited even if they are
  over the threshold."
  [col cell-exc depth threshold dominance-margin]
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
        (cond
          ;; stimulus threshold not reached
          (< best-exc threshold)
          cell-ids
          ;; best cells exceed all others by dominance-margin
          (>= (- best-exc second-exc) dominance-margin)
          best-ids
          ;; otherwise, all cells over threshold become active
          :else
          good-ids)))))

(defn select-active-cells
  "Determines active cells in the given columns and whether they are bursting.
  Returns keys
  * `:by-column` - map of column id to seq of active cell ids.
  * `:active-cells` - the set of active cell ids.
  * `:stable-active-cells` - the set of non-bursting active cells.
  * `:burst-cols` - the set of bursting column ids."
  [a-cols cell-exc depth threshold dominance-margin]
  (loop [cols (seq a-cols)
         col-ac (transient {}) ;; active cells by column
         ac (transient #{}) ;; active cells
         sac (transient #{}) ;; stable active cells
         b-cols (transient #{}) ;; bursting columns
         ]
    (if-let [col (first cols)]
      (let [this-ac (column-active-cells col cell-exc depth
                                         threshold dominance-margin)
            bursting? (== depth (count this-ac))]
        (recur (next cols)
               (assoc! col-ac col this-ac)
               (reduce conj! ac this-ac)
               (if bursting? sac (reduce conj! sac this-ac))
               (if bursting? (conj! b-cols col) b-cols)))
      ;; finished
      {:by-column (persistent! col-ac)
       :active-cells (persistent! ac)
       :stable-active-cells (persistent! sac)
       :burst-cols (persistent! b-cols)}
      )))

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

(defn learning-updates
  "Takes the learning `cells` and maps each to a SegUpdate record,
  which includes the segment path to learn on, together with lists of
  any synapse sources to add or remove. Any matching segments to learn
  on are given as `matching-segs`, mapping cell ids to `[seg-path
  exc]`. If this is missing for a cell then a new segment will be
  grown, perhaps replacing an existing one.

  `aci` is the set of active source indices, used to find a matching
  segment, while `lci` is the set of learnable source indices, used to
  grow new synapses.

  Note that ''cell-ids'' here may also refer to columns in a proximal
  synapse graph, where the convention is [col 0]. Everything else is
  the same since proximal synapses graphs can also have multiple
  segments [col 0 seg-idx]."
  [cells matching-segs sg aci lci rng
   {pcon :perm-connected
    min-act :learn-threshold
    new-syns :new-synapse-count
    max-syns :max-synapse-count
    max-segs :max-segments}]
  (let [lci-vec (vec lci)] ;; for faster sampling
    (loop [cells (seq cells)
           m (transient {})
           rng rng]
      (if-let [cell-id (first cells)]
        (let [[matching-path exc] (get matching-segs cell-id)
              new-segment? (not matching-path)
              cell-segs (p/cell-segments sg cell-id)
              [new-si replaced-syns] (when new-segment?
                                       (new-segment-id cell-segs pcon max-segs
                                                       max-syns))
              seg (if new-segment?
                    {}
                    (let [[_ _ si] matching-path]
                      (nth cell-segs si)))
              grow-n (-> (- new-syns (or exc 0)) (max 0))
              [rng* rng] (random/split rng)
              grow-source-ids (segment-new-synapse-source-ids seg lci-vec grow-n rng*)
              die-source-ids (if new-segment?
                               (keys replaced-syns)
                               (segment-excess-synapse-source-ids seg grow-n
                                                                  max-syns))
              seg-path (if new-segment? (conj cell-id new-si) matching-path)]
          (recur (next cells)
                 ;; if not enough learnable sources to grow a new segment, skip it
                 (if (and new-segment?
                          (< (count grow-source-ids) min-act))
                   m ;; skip
                   (assoc! m cell-id (syn/seg-update seg-path :learn grow-source-ids
                                                     die-source-ids)))
                 rng))
        ;; finished
        (persistent! m)))))

(defn learn-distal
  [sg distal-state cells matching-segs dspec rng]
  (let [learning (learning-updates cells matching-segs
                                   sg (:on-bits distal-state)
                                   (:on-lc-bits distal-state)
                                   rng dspec)
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
  [this cells matching-segs]
  (let [sg (:distal-sg this)
        dstate (:distal-state this)
        dspec (:distal (:spec this))
        [rng* rng] (random/split (:rng this))
        [new-sg learning] (learn-distal sg dstate cells matching-segs dspec rng*)]
    (assoc this
           :rng rng
           :learn-state (assoc-in (:learn-state this)
                                  [:learning :distal] learning)
           :distal-sg new-sg)))

(defn layer-learn-apical
  [this cells matching-segs]
  (let [sg (:apical-sg this)
        dstate (:apical-state this)
        dspec (:apical (:spec this))
        [rng* rng] (random/split (:rng this))
        [new-sg learning] (learn-distal sg dstate cells matching-segs dspec rng*)]
    (assoc this
           :rng rng
           :learn-state (assoc-in (:learn-state this)
                                  [:learning :apical] learning)
           :apical-sg new-sg)))

(defn layer-punish-lateral
  [this]
  (let [sg (:distal-sg this)
        dstate (:distal-state this)
        pdstate (:prior-distal-state this)
        dspec (:distal (:spec this))
        [new-sg punishments] (punish-distal sg dstate pdstate dspec)]
    (assoc this
           :learn-state (assoc-in (:learn-state this)
                                  [:punishments :distal] punishments)
           :distal-sg new-sg)))

(defn layer-punish-apical
  [this]
  (let [sg (:apical-sg this)
        dstate (:apical-state this)
        pdstate (:prior-apical-state this)
        dspec (:apical (:spec this))
        [new-sg punishments] (punish-distal sg dstate pdstate dspec)]
    (assoc this
           :learn-state (assoc-in (:learn-state this)
                                  [:punishments :apical] punishments)
           :apical-sg new-sg)))

(defn layer-learn-proximal
  [this cols]
  (let [sg (:proximal-sg this)
        state (:state this)
        pspec (:proximal (:spec this))
        higher-level? (> (:max-segments pspec) 1)
        [rng* rng] (random/split (:rng this))
        prox-learning (learning-updates (map vector cols (repeat 0))
                                        (:matching-ff-seg-paths state)
                                        sg
                                        (:in-ff-bits state)
                                        (if higher-level?
                                          (:in-stable-ff-bits state)
                                          (:in-ff-bits state))
                                        rng* pspec)
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
           :learn-state (assoc-in (:learn-state this)
                                  [:learning :proximal] prox-learning)
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
     col-overlaps matching-ff-seg-paths
     temporal-pooling-exc
     active-cols burst-cols col-active-cells active-cells timestep])

(defrecord LayerLearnState
    [col-winners winner-seg learning-cells learning punishments timestep])

(defrecord LayerDistalState
    [on-bits on-lc-bits cell-exc pred-cells
     matching-seg-paths prior-active-cells timestep])

(def empty-active-state
  (map->LayerActiveState
   {:active-cells #{}
    :active-cols #{}
    :temporal-pooling-exc {}
    :matching-ff-seg-paths {}}))

(def empty-learn-state
  (map->LayerLearnState
   {:col-winners {}
    :learning-cells #{}
    :learning {}
    :punishments {}}))

(def empty-distal-state
  (map->LayerDistalState
   {:on-bits #{}
    :cell-exc {}
    :pred-cells #{}
    :matching-seg-paths {}}))

(defn compute-active-state
  [state ff-bits stable-ff-bits proximal-sg distal-state apical-state
   boosts topology inh-radius spec]
  (let [pspec (:proximal spec)
        ;; proximal excitation as number of active synapses, keyed by [col 0 seg-idx]
        col-seg-overlaps (p/excitations proximal-sg ff-bits
                                        (:stimulus-threshold pspec))
        ;; these both keyed by [col 0]
        [raw-col-exc ff-seg-paths]
        (best-segment-excitations-and-paths col-seg-overlaps)
        ;; temporal pooling, depending on stability of input bits.
        ;; also check for clear matches, these override pooling
        higher-level? (> (:max-segments pspec) 1)
        engaged? (or (not higher-level?)
                     (>= (count stable-ff-bits)
                         (* (count ff-bits) (:stable-inbit-frac-threshold spec))))
        newly-engaged? (or (not higher-level?)
                           (and engaged?
                                (or (not (:engaged? state))
                                    ;; check for manual resets (break :tp)
                                    (empty? (:temporal-pooling-exc state)))))
        tp-exc (cond-> (if newly-engaged?
                         {}
                         (:temporal-pooling-exc state))
                 true ;(not engaged?)
                 (decay-tp (:temporal-pooling-fall spec)))
        col-exc (cond-> raw-col-exc
                  (not engaged?)
                  (select-keys (keys ff-seg-paths))
                  true
                  (columns/apply-overlap-boosting boosts))
        ;; ignore apical excitation unless there is matching distal.
        ;; unlike other segments, allow apical excitation to add to distal
        d-a-cell-exc (if (:use-feedback? spec)
                       (merge-with + (:cell-exc distal-state)
                                   (select-keys (:cell-exc apical-state)
                                                (keys (:cell-exc distal-state))))
                       (:cell-exc distal-state))
        ;; combine excitation values for selecting columns
        abs-cell-exc (total-excitations col-exc tp-exc d-a-cell-exc
                                        (:distal-vs-proximal-weight spec)
                                        (:spontaneous-activation? spec)
                                        (:depth spec))
        ;; union temporal pooling: accrete more columns as pooling continues
        activation-level (let [base-level (:activation-level spec)
                               prev-ncols (->> (keys (:temporal-pooling-exc state))
                                               (map first) (distinct) (count))
                               prev-level (/ prev-ncols
                                             (p/size topology))]
                           (if (or newly-engaged? (not engaged?))
                             base-level
                             (min (:activation-level-max spec)
                                  (+ prev-level base-level))))
        a-cols (select-active-columns (best-by-column abs-cell-exc)
                                      topology activation-level
                                      inh-radius spec)
        ;; find active cells in the columns
        depth (:depth spec)
        {col-ac :by-column
         ac :active-cells
         burst-cols :burst-cols
         stable-ac :stable-active-cells}
        (select-active-cells a-cols (merge-with + d-a-cell-exc tp-exc)
                             depth (:stimulus-threshold (:distal spec))
                             (:dominance-margin spec))
        ;; update continuing TP activation
        next-tp-exc (if higher-level?
                      (let [new-ac (if newly-engaged?
                                     ac
                                     (set/difference ac (:active-cells state)))
                            amp (:temporal-pooling-amp spec)
                            max-exc (:temporal-pooling-max-exc spec)]
                        (into (select-keys tp-exc ac) ;; only keep TP for active cells
                              (map (fn [[cell exc]]
                                     [cell (-> exc (* amp) (min max-exc))]))
                              (select-keys abs-cell-exc new-ac)))
                      {})]
    (map->LayerActiveState
     {:in-ff-bits ff-bits
      :in-stable-ff-bits stable-ff-bits
      :out-ff-bits (set (cells->bits depth ac))
      :out-stable-ff-bits (set (cells->bits depth stable-ac))
      :engaged? engaged?
      :newly-engaged? newly-engaged?
      :col-overlaps raw-col-exc
      :matching-ff-seg-paths ff-seg-paths
      :temporal-pooling-exc next-tp-exc
      :col-active-cells col-ac
      :active-cells ac
      :active-cols a-cols
      :burst-cols burst-cols
      :timestep (inc (:timestep state))
      })))

(defn select-winner-cell
  "Returns [winner-cell [distal-seg-path exc] [apical-seg-path exc]]
  giving the best matching existing segments to learn on, if any."
  [ac distal-state apical-state distal-sg apical-sg spec rng]
  (let [full-matching-distal (:matching-seg-paths distal-state)
        full-matching-apical (:matching-seg-paths apical-state)
        d-full-matches (keep full-matching-distal ac)
        a-full-matches (keep full-matching-apical ac)
        distal-bits (:on-bits distal-state)
        apical-bits (:on-bits apical-state)
        min-distal (:learn-threshold (:distal spec))
        min-apical (:learn-threshold (:apical spec))
        ;; TODO: perf - maintain index of targets-by-source with pcon=0
        best-partial-distal-segment
        (fn [cell-id]
          (let [cell-segs (p/cell-segments distal-sg cell-id)
                [match-si exc seg] (best-matching-segment
                                    cell-segs distal-bits min-distal 0.0)]
            (when match-si
              [(conj cell-id match-si) exc])))
        best-partial-apical-segment
        (fn [cell-id]
          (let [cell-segs (p/cell-segments apical-sg cell-id)
                [match-si exc seg] (best-matching-segment
                                    cell-segs apical-bits min-apical 0.0)]
            (when match-si
              [(conj cell-id match-si) exc])))
        distal-match*
        (cond
          ;; * if one matching distal segment
          ;; ==> select it
          (== (count d-full-matches) 1)
          (first d-full-matches)
          ;; * if multiple matching distal segments
          ;; ==> these are the active cells; fall through to apical selection
          ;; (and finally select distal segment afterwards)
          (> (count d-full-matches) 1)
          nil
          ;; * if some partial matching distal segments
          ;; ==> select best of those
          :else
          (let [partial-matches (keep best-partial-distal-segment ac)]
            (if (seq partial-matches)
              (apply max-key second partial-matches)
              ;; * otherwise - no distal matches
              nil
              )))
        apical-match
        (cond
          ;; * if there is already a distal match
          ;; ==> select best apical segment on that cell
          distal-match*
          (let [cell-id (pop (first distal-match*))]
            (if-let [full-match (full-matching-apical cell-id)]
              full-match
              (best-partial-apical-segment cell-id)))
          ;; * if multiple matching distal segments / cells
          ;; ==> select best by apical, fall back to random
          (> (count d-full-matches) 1)
          (if (seq a-full-matches)
            (util/rand-nth rng a-full-matches)
            (let [partial-matches (keep best-partial-apical-segment ac)]
              (if (seq partial-matches)
                (apply max-key second partial-matches)
                ;; * otherwise - no apical matches
                nil
                )))
          :else
          nil
          )
        distal-match
        (cond
          ;; * if already have it, nothing to do
          distal-match*
          distal-match*
          ;; * if multiple matching distal segments
          ;;   (deferred case from above)
          ;; ==> select best segment on chosen cell (by apical or randomly)
          (> (count d-full-matches) 1)
          (let [cell-id (if apical-match
                          (pop (first (apical-match)))
                          (util/rand-nth rng ac))
                match (full-matching-distal cell-id)]
            (assert match "fully active distal, if any, should equal active cells")
            match)
          :else
          nil)
        winner-cell
        (cond
          distal-match
          (pop (first distal-match))
          :else
          (util/rand-nth rng ac))]
    [winner-cell distal-match apical-match]))

(defn select-winner-cells
  "Returns keys / nested keys

  * `:col-winners` - maps column id to winning cell id;
  * `:winner-seg :distal` - maps cell id to [seg-path exc] for a lateral segment;
  * `:winner-seg :apical` - maps cell id to [seg-path exc] for an apical segment;

  These :winner-seg maps contain only the winning cell ids for which an
  existing segment matches sufficiently to be learning. Otherwise, a
  new (effectively new) segment will be grown."
  [col-ac distal-state apical-state learn-state distal-sg apical-sg spec rng
   newly-engaged?]
  (let [reset? (empty? (:on-bits distal-state))
        ;; keep winners stable only at higher levels (first level needs to see repeats)
        prior-col-winners (when-not newly-engaged? (:col-winners learn-state))]
    (loop [col-ac col-ac
           col-winners (transient {})
           winning-distal (transient {})
           winning-apical (transient {})
           rng rng]
      (if-let [[col ac] (first col-ac)]
        (if reset?
          (recur (next col-ac)
                 (assoc! col-winners col (first ac))
                 winning-distal winning-apical rng)
          (let [;; carry forward learning cells for higher level sequences
                prior-winner (get prior-col-winners col)
                ac (if (and prior-winner (some #(= % prior-winner) ac))
                     [prior-winner]
                     ac)
                [rng* rng] (random/split rng)
                [winner dmatch amatch]
                (select-winner-cell ac distal-state apical-state
                                    distal-sg apical-sg spec rng*)]
            (recur (next col-ac)
                   (assoc! col-winners col winner)
                   (if dmatch
                     (assoc! winning-distal winner dmatch)
                     winning-distal)
                   (if amatch
                     (assoc! winning-apical winner amatch)
                     winning-apical)
                   rng)))
        ;; finished
        {:col-winners (persistent! col-winners)
         :winner-seg {:distal (persistent! winning-distal)
                      :apical (persistent! winning-apical)}}))))

(defn compute-distal-state
  [sg aci lci dspec t]
  (let [seg-exc (p/excitations sg aci (:stimulus-threshold dspec))
        [cell-exc seg-paths] (best-segment-excitations-and-paths seg-exc)
        pc (set (keys cell-exc))]
    (map->LayerDistalState
     {:on-bits (set aci)
      :on-lc-bits (set lci)
      :cell-exc cell-exc
      :matching-seg-paths seg-paths
      :pred-cells pc
      :timestep t})))

(defrecord LayerOfCells
    [spec rng topology input-topology inh-radius boosts active-duty-cycles
     proximal-sg distal-sg apical-sg state distal-state prior-distal-state
     apical-state prior-apical-state learn-state]

  p/PLayerOfCells

  (layer-activate
    [this ff-bits stable-ff-bits]
    (let [new-state (compute-active-state state ff-bits stable-ff-bits
                                          proximal-sg distal-state apical-state
                                          boosts topology inh-radius spec)]
      (assoc this :state new-state)))

  (layer-learn
    [this]
    (let [a-cols (:active-cols state)
          col-ac (:col-active-cells state)
          newly-engaged? (:newly-engaged? state)
          [rng* rng] (random/split rng)
          {:keys [col-winners winner-seg]}
          (select-winner-cells col-ac distal-state apical-state learn-state
                               distal-sg apical-sg spec rng* newly-engaged?)
          ;; learning cells are the winning cells, but excluding any
          ;; continuing winners when temporal pooling
          old-winners (vals (:col-winners learn-state))
          new-winners (vals col-winners)
          lc (if newly-engaged? ;; always true at first level
               new-winners
               (remove (set old-winners) new-winners))
          depth (:depth spec)
          out-wc-bits (set (cells->bits depth (vals col-winners)))
          timestep (:timestep state)]
      (cond->
          this
        true (assoc :learn-state (assoc empty-learn-state
                                        :col-winners col-winners
                                        :winner-seg winner-seg
                                        :learning-cells lc
                                        :out-wc-bits out-wc-bits
                                        :timestep timestep)
                    :rng rng)
        (:learn? (:distal spec)) (layer-learn-lateral lc (:distal winner-seg))
        (:learn? (:apical spec)) (layer-learn-apical lc (:apical winner-seg))
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
          wc (vals (:col-winners learn-state))
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
    (set (vals (:col-winners learn-state))))
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
        ;; after depolarise phase has run
        (== t-1 (:timestep prior-distal-state))
        (:pred-cells prior-distal-state)
        ;; before depolarise phase has run
        (== t-1 (:timestep distal-state))
        (:pred-cells distal-state))))

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
    (:out-wc-bits learn-state))
  p/PTemporal
  (timestep [_]
    (:timestep state))
  p/PParameterised
  (params [_]
    spec))

(defn init-layer-state
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
        learn-state (assoc empty-learn-state :timestep 0)
        distal-state (assoc empty-distal-state :timestep 0)]
    {:spec spec
     :rng rng
     :topology col-topo
     :input-topology input-topo
     :inh-radius 1
     :proximal-sg proximal-sg
     :distal-sg distal-sg
     :apical-sg apical-sg
     :state state
     :learn-state learn-state
     :distal-state distal-state
     :prior-distal-state distal-state
     :boosts (vec (repeat n-cols 1.0))
     :active-duty-cycles (vec (repeat n-cols 0.0))
     }))

(defn layer-of-cells
  [spec]
  (->
   (init-layer-state spec)
   (map->LayerOfCells)
   (update-inhibition-radius)))
