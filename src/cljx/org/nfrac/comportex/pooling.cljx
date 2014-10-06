(ns org.nfrac.comportex.pooling
  "Selection of the set of columns in a region to become active, by
   excitation and inhibition. This process produces spatial
   pooling (pattern memory) over feed-forward inputs, and temporal
   pooling over recognised sequences.

   **Argument name conventions:**

   * `col` -- a column id, an integer index in the region.
   * `a-cols` -- the set of ids of active columns.
   * `om` -- overlap scores in a map keyed by column id.
   * `ff-bits` -- the set of indexes of any active feed-forward input bits.
   * `cf` -- a ColumnField.
"
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util
             :refer [abs round mean count-filter remap]]))

(def parameter-defaults
  "Default parameter specification map.

   * `input-dimensions` - size of input bit array, either one
     dimensional `[size]` or two dimensional `[width height]`.

   * `column-dimensions` - size of column field, either one
     dimensional `[size]` or two dimensional `[width height]`.

   * `global-inhibition` - whether to use the faster global algorithm
     for column inhibition (just keep those with highest overlap
     scores), or to apply inhibition only within a column's
     neighbours.

   * `activation-level` - fraction of columns that can be
     active (either locally or globally); inhibition kicks in to
     reduce it to this level.

   * `ff-potential-radius` - range of potential feed-forward synapse
     connections; a distance in input bits. See also `core/tree`.

   * `ff-potential-frac` - fraction of inputs within range that will be
     part of the potentially connected set.

   * `ff-perm-inc` - amount to increase a synapse's permanence value
     by when it is reinforced.

   * `ff-perm-dec` - amount to decrease a synapse's permanence value
     by when it is not reinforced.

   * `ff-perm-connected` - permanence value at which a synapse is
     functionally connected. Permanence values are defined to be
     between 0 and 1.

   * `ff-stimulus-threshold` - minimum number of active input connections
     for a column to be _overlapping_ the input (i.e. active prior to
     inhibition).

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

   * `temporal-pooling-decay` - multiplier on the continuing overlap
     score of temporal pooling columns; as this reduces a temporal
     pooling cell is more likely to be interrupted by competing
     columns.

   * `temporal-pooling-amp` - multiplier on the initial overlap score
     of temporal pooling columns; this increases the probability that
     TP cells will remain active."
  {:input-dimensions [:define-me!]
   :column-dimensions [2048]
   :ff-potential-radius 256
   :ff-potential-frac 0.5
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.1
   :ff-stimulus-threshold 3
   :global-inhibition false
   :activation-level 0.02
   :boost-overlap-duty-ratio 0.001
   :boost-active-duty-ratio 0.001
   :duty-cycle-period 1000
   :max-boost 3.0
   :temporal-pooling-decay 0.9
   :temporal-pooling-amp 1.1
   })

(defn n-columns
  [cf]
  (p/size (p/topology cf)))

(defn uniform-ff-synapses
  "Generates feed-forward synapses connecting to the given column
   from the input array.

   Connections are made locally by scaling the input space to the
   column space. Potential synapses are chosen within a radius
   `ff-potential-radius` of input bits, and of those, a fraction
   `ff-potential-frac` are chosen from a uniform random distribution.

   Initial permanence values are uniformly distributed between one
   increment above the connected threshold, down to two increments
   below. So about one third will be initially connected."
  [col n-cols itopo spec]
  (let [pcon (:ff-perm-connected spec)
        pinc (:ff-perm-inc spec)
        radius (:ff-potential-radius spec) ;; in input space
        frac (:ff-potential-frac spec)
        input-size (p/size itopo)
        focus-i (round (* input-size (/ col n-cols)))
        all-ids (p/neighbours-indices itopo focus-i radius)
        n (round (* frac (count all-ids)))
        ids (take n (util/shuffle all-ids))
        p-hi (-> (+ pcon (* 1.0 pinc)) (min 1.0))
        p-lo (-> (- pcon (* 2.0 pinc)) (max 0.0))
        perms (repeatedly n #(util/rand p-lo p-hi))]
    (zipmap ids perms)))

(defn triangular-ff-synapses
  "Generates feed-forward synapses connecting to the given column
   from the input array.

   Connections are made locally by scaling the input space to the
   column space. Potential synapses are chosen within a radius
   `ff-potential-radius` of input bits, and of those, a fraction
   `ff-potential-frac` are chosen from a uniform random distribution.

   Initial permanence values are triangular distributed between one
   increment above the connected threshold, down to two increments
   below. So about one third will be initially connected."
  [col n-cols itopo spec]
  (let [pcon (:ff-perm-connected spec)
        pinc (:ff-perm-inc spec)
        radius (:ff-potential-radius spec) ;; in input space
        frac (:ff-potential-frac spec)
        input-size (p/size itopo)
        focus-i (round (* input-size (/ col n-cols)))
        focus-c (p/coordinates-of-index itopo focus-i)
        all-coords (p/neighbours itopo focus-c radius)
        n (round (* frac (count all-coords)))
        coords (take n (util/shuffle all-coords))
        ids (map (partial p/index-of-coordinates itopo) coords)
        p-hi (-> (+ pcon (* 1.0 pinc)) (min 1.0))
        p-lo (-> (- pcon (* 2.0 pinc)) (max 0.0))
        ;; triangular:
        perms (for [c coords]
                ;; z is 1 at input focus, down to 0 linearly at radius
                (let [z (- 1.0 (/ (p/coord-distance itopo focus-c c)
                                  radius))]
                  (+ p-lo (* z (- p-hi p-lo)))))]
    (zipmap ids perms)))

;;; ## Neighbouring columns

(defn numeric-span
  [xs]
  (- (apply max xs) (apply min xs)))

(defn column-receptive-field-size
  "Returns the span over the input bit array to which this column has
   connected synapses. Takes the maximum span in any one dimension."
  [ff-sg itopo col]
  (let [ids (p/sources-connected-to ff-sg col)
        coords (map (partial p/coordinates-of-index itopo) ids)]
    (if (seq coords)
      (if (number? (first coords))
        (numeric-span coords)
        (let [m (count (p/dimensions itopo))]
          (->> (for [j (range m)]
                 (numeric-span (map #(nth % j) coords)))
               (apply max))))
      0)))

(defn avg-receptive-field-size
  [cf]
  (let [n-cols (n-columns cf)
        itopo (:input-topology cf)
        ff-sg (:ff-sg cf)]
    (-> (map (partial column-receptive-field-size ff-sg itopo)
             (range n-cols))
        (mean))))

(defn inhibition-radius
  "The radius in column space defining neighbouring columns, based on
   the average receptive field size. Specifically, neighbouring
   columns are defined by sharing at least 30% of their receptive
   fields, on average."
  [cf]
  (let [shared-frac 0.3
        n-cols (n-columns cf)
        n-inbits (p/size (:input-topology cf))
        arfs (avg-receptive-field-size cf)
        ;; columns in this range will have some overlap of inputs
        cols-diameter (* n-cols (/ arfs n-inbits))
        cols-radius (quot cols-diameter 2)]
    ;; to share a given fraction of receptive fields
    (-> (* cols-radius (- 1.0 shared-frac))
        (round)
        (max 1))))

;;; ## Overlaps

(defn compute-overlaps
  "Given a column field `cf` and the set of active input bits
   `ff-bits`, finds the columns with an overlap count above parameter
   `ff-stimulus-threshold`, and returns a map of their column ids to
   overlap scores. That is, the number of input bits connected to,
   multiplied by the column boosting factor."
  [cf ff-bits]
  (let [om-raw (syn/excitations (:ff-sg cf) ff-bits)
        th (:ff-stimulus-threshold (:spec cf))]
    (->> om-raw
         (reduce-kv (fn [om col x]
                      (if (< x th)
                        om
                        (let [boost (get-in cf [:boosts col])]
                          (assoc! om col (* x boost)))))
                    (transient {}))
         (persistent!))))

;;; ## Activation

(defn active-columns-with-global-inhibition
  "Returns the set of column ids which should become active given the
   map of column overlap scores `om`, and the target activation rate
   `level`. Global inhibition is applied, i.e. the top N columns by
   overlap score are selected."
  [om level n-cols]
  (let [n-on (max 1 (round (* level n-cols)))]
    (loop [oms (seq om)
           am (sorted-map-by #(compare (om %1) (om %2)))
           curr-min 1000000.0]
      (if (empty? oms)
        (set (keys am))
        (let [[col o] (first oms)]
          (cond
           ;; just initialising the set
           (< (count am) n-on)
           (recur (next oms)
                  (assoc am col o)
                  (double (min curr-min o)))
           ;; include this one, dominates previous min
           (> o curr-min)
           (let [new-am (-> (dissoc am (first (keys am)))
                            (assoc col o))]
             (recur (next oms)
                    new-am
                    (double (first (vals new-am)))))
           ;; exclude this one
           :else
           (recur (next oms) am curr-min)))))))

(defn dominant-overlap-diff
  [dist]
  (-> dist
      (- 4) ; within this radius, there can be only one
      (max 0)
      (/ 2.0))) ; for every 2 columns away, need one extra overlap to dominate

(defn map->vec
  [n m]
  (mapv m (range n)))

(defn vec->map
  [v]
  (persistent!
   (reduce-kv (fn [m i x]
                (if x
                  (assoc! m i x)
                  m))
              (transient {}) v)))

(defn apply-local-inhibition
  [om cf outer-radius inner-radius]
  (let [topo (p/topology cf)]
    (loop [cols (keys om)
           mask (transient (map->vec (n-columns cf) om))]
      (if-let [col (first cols)]
        (if-let [o (mask col)]
          (recur
           (next cols)
           ;; loop through neighbours and mask out any dominated
           (let [coord (p/coordinates-of-index topo col)]
             (loop [nbs (p/neighbours topo coord outer-radius inner-radius)
                    mask mask]
               (if-let [nb-coord (first nbs)]
                 (let [nb-col (p/index-of-coordinates topo nb-coord)]
                   (if-let [nb-o (mask nb-col)]
                     (let [dist (p/coord-distance topo coord nb-coord)
                           odom (dominant-overlap-diff dist)]
                       (cond
                        ;; neighbour is dominated
                        (>= o (+ nb-o odom))
                        (recur (next nbs)
                               (assoc! mask nb-col nil))
                        ;; we are dominated by neighbour; abort
                        (<= o (- nb-o odom))
                        (assoc! mask col nil)
                        ;; neither dominates
                        :else
                        (recur (next nbs) mask)))
                     ;; neighbour has no overlap or was eliminated
                     (recur (next nbs) mask)))
                 ;; finished with neighbours
                 mask))))
          ;; already eliminated, skip
          (recur (next cols) mask))
        ;; finished
        (vec->map (persistent! mask))))))

(defn perturb-overlaps
  [om]
  (remap #(+ % (util/rand 0 0.5)) om))

(defn select-active-columns
  "Returns the set of column ids which should become active given the
   map of column overlap scores `om`. The method used depends on the
   specification key `:global-inhibition`."
  [cf om]
  (let [spec (:spec cf)
        level (:activation-level spec)
        do-local? (not (:global-inhibition spec))
        radius (:inh-radius cf)
        omi (cond-> (perturb-overlaps om)
                    do-local? (apply-local-inhibition cf (quot radius 3) 0)
                    do-local? (apply-local-inhibition cf radius (quot radius 3)))]
    (active-columns-with-global-inhibition omi level (n-columns cf))))

;;; ## Learning

(defn learn
  "Adapt feed-forward synapses to focus on observed input patterns.
   Given the set of input bits `ff-bits`, adjusts the permanence
   values of all potential feed-forward synapses in the active columns
   `a-cols`."
  [cf a-cols ff-bits signal-ff-bits]
  (let [pinc (:ff-perm-inc (:spec cf))
        pdec (:ff-perm-dec (:spec cf))]
    (update-in cf [:ff-sg]
               (fn [ff-sg]
                 (reduce (fn [ff col]
                           (p/reinforce-in-synapses ff col (constantly false)
                                                    ff-bits pinc pdec))
                         ff-sg a-cols)))))

;;; ## Boosting

(defn update-column-boosting
  "Recalculates the column's boost factor and possibly applies an
   increase to all feedforward synapse permanences. This is based on
   comparing the number of activations or overlaps in recent history
   to the _maximum_ such value from its neighbours."
  [cf col]
  (let [spec (:spec cf)
        o-th (:boost-overlap-duty-ratio spec)
        a-th (:boost-active-duty-ratio spec)
        maxb (:max-boost spec)
        pinc (:ff-perm-inc spec)
        ods (:overlap-duty-cycles cf)
        ads (:active-duty-cycles cf)
        radius (:inh-radius cf)
        ncols (p/neighbours-indices (p/topology cf) col radius)
        max-od (apply max 1 (vals (select-keys ods ncols)))
        max-ad (apply max 1 (vals (select-keys ads ncols)))
        crit-od (* o-th max-od)
        crit-ad (* a-th max-ad)
        od (get ods col)
        ad (get ads col)
        nboost (-> (- maxb (* (- maxb 1)
                              (/ ad crit-ad)))
                   (max 1.0)
                   (double))]
    (if (< od crit-od)
      (update-in cf [:ff-sg] p/reinforce-in-synapses col (constantly false)
                 (constantly true) pinc 0)
      (assoc-in cf [:boosts col] nboost))))

(defn update-boosting
  "For each column, determines whether it has had too few activations
  -- relative to its neighbours -- in recent history. Boosting may be
  applied to boost either a column's input overlap (by increasing
  connections) or its share of activations after inhibition (by
  increasing its boost factor)."
  [cf]
  (reduce update-column-boosting
          cf (range (n-columns cf))))

(defn update-duty-cycles
  "Records a set of events with indices `is` in the vector `v`
   according to duty cycle period `period`. As in NuPIC, the formula
   is

<pre>
y[t] = (period-1) * y[t-1]  +  1
       --------------------------
         period
</pre>"
  [v is period]
  (let [d (/ 1.0 period)
        decay (* d (dec period))]
    (-> (mapv #(* % decay) v)
        (util/update-each is #(+ % d)))))

;; ## Temporal Pooling

(defn temporal-pooling-scores
  "Selects a subset of active columns to be in a temporal pooling state,
   returning a map from their column ids to an (adjusted) overlap
   score -- which is used to compete for activation next time step.

   `a-cols` - set of active column ids.

   `signal-ff-bits` - input set from predicted cells.

   `prev-tpm` - temporal pooling overlap scores from previous step.

   `curr-om` - overlap scores from current feedforward input."
  [cf a-cols signal-ff-bits prev-tpm curr-om]
  (let [amp (:temporal-pooling-amp (:spec cf))
        decay (:temporal-pooling-decay (:spec cf))
        sig-om (compute-overlaps cf signal-ff-bits)
        new-tpm (->> (select-keys sig-om a-cols)
                     (remap (partial * amp)))
        ;; if a previous TP column received a new dominant input
        ;; then its TP status is interrupted (unless in new-tpm)
        stopped-tps (keep (fn [[k v]] (when (> (curr-om k 0) v) k))
                          prev-tpm)
        ;; which of the previous TP columns became (remained) active?
        ;; their TP status continues, and overlap scores decay.
        kept-tpm (->> (apply disj a-cols stopped-tps)
                      (select-keys prev-tpm)
                      (remap (partial * decay)))]
    (merge-with max new-tpm kept-tpm)))

;;; ## Orchestration

(defn update-inhibition-radius
  [cf]
  (assoc cf :inh-radius (inhibition-radius cf)))

(defrecord ColumnField
    [spec ff-sg topology input-topology inh-radius
     overlaps active-cols tp-scores
     boosts active-duty-cycles overlap-duty-cycles]
  p/PColumnField
  (columns-step
    [this ff-bits signal-ff-bits cell-depolarisation learn?]
    (let [curr-om (compute-overlaps this ff-bits)
          om (merge-with max curr-om tp-scores)
          a-cols (select-active-columns this om)
          tpm (temporal-pooling-scores this a-cols signal-ff-bits tp-scores curr-om)
          dcp (:duty-cycle-period spec)
          t (inc (:timestep this 0))
          boost? (and learn? (zero? (mod t dcp)))]
      (cond->
       (assoc this
         :timestep t
         :overlaps om
         :active-cols a-cols
         :tp-scores tpm)
       learn? (update-in [:overlap-duty-cycles] update-duty-cycles (keys om) dcp)
       learn? (update-in [:active-duty-cycles] update-duty-cycles a-cols dcp)
       learn? (learn (filter curr-om a-cols) ;; only learn on columns with current input
                     ff-bits signal-ff-bits)
       boost? (update-boosting)
       boost? (update-inhibition-radius))))
  (active-columns [_]
    active-cols)
  (temporal-pooling-columns [_]
    (set (keys tp-scores)))
  (column-overlaps [_]
    overlaps)
  p/PTemporal
  (timestep [this]
    (:timestep this 0))
  p/PTopological
  (topology [this]
    (:topology this))
  p/PParameterised
  (params [_]
    spec))

(defn column-field
  [spec]
  (let [spec (merge parameter-defaults spec)
        input-dim (:input-dimensions spec)
        col-dim (:column-dimensions spec)
        input-topo (topology/make-topology input-dim)
        col-topo (topology/make-topology col-dim)
        n-inbits (p/size input-topo)
        n-cols (p/size col-topo)
        all-syns (mapv #(uniform-ff-synapses % n-cols input-topo spec)
                       (range n-cols))]
    (->
     (map->ColumnField
      {:spec spec
       :ff-sg (syn/synapse-graph all-syns n-inbits
                                 (:ff-perm-connected spec)
                                 1000000 false)
       :topology col-topo
       :input-topology input-topo
       :inh-radius 1
       :overlaps {}
       :active-cols #{}
       :tp-scores {}
       :boosts (vec (repeat n-cols 1.0))
       :active-duty-cycles (vec (repeat n-cols 0.0))
       :overlap-duty-cycles (vec (repeat n-cols 0.0))
       })
     (update-inhibition-radius))))
