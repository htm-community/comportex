(ns org.nfrac.comportex.columns
  "Handling of column excitation through proximal dendrite segment
   synapses. This process produces spatial pooling (pattern memory)
   over feed-forward inputs.

   **Argument name conventions:**

   * `col` -- a column id, an integer index in the region.
   * `a-cols` -- the set of ids of active columns.
   * `om` -- overlap scores in a map keyed by column id.
   * `ff-bits` -- the set of indexes of any active feed-forward input bits.
   * `cf` -- a ColumnField.
"
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util
             :refer [abs round mean count-filter remap]]))

(def columns-parameter-defaults
  "Default parameter specification map.

   * `input-dimensions` - size of input bit grid as a vector, one
     dimensional `[size]`, two dimensional `[width height]`, etc.

   * `column-dimensions` - size of column field as a vector, one
     dimensional `[size]` or two dimensional `[width height]`.

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
     increase activation frequency."
  {:input-dimensions [:define-me!]
   :column-dimensions [2048]
   :ff-potential-radius 256
   :ff-potential-frac 0.5
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.1
   :ff-stimulus-threshold 3
   :boost-overlap-duty-ratio 0.001
   :boost-active-duty-ratio 0.001
   :duty-cycle-period 1000
   :max-boost 3.0
   })

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
          cf (range (p/size-of cf))))

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
  (assoc cf :inh-radius (inh/inhibition-radius (:ff-sg cf) (:topology cf)
                                               (:input-topology cf))))

(defrecord ColumnField
    [spec ff-sg topology input-topology overlaps sig-overlaps
     inh-radius boosts active-duty-cycles overlap-duty-cycles]
  p/PColumnField
  (columns-step
    [this ff-bits signal-ff-bits]
    (let [om (compute-overlaps this ff-bits)
          sig-om (compute-overlaps this signal-ff-bits)]
      (cond->
       (assoc this
         :timestep (inc (:timestep this 0))
         :overlaps om
         :sig-overlaps sig-om))))
  (columns-learn
    [this ff-bits signal-ff-bits a-cols]
    (let [dcp (:duty-cycle-period spec)
          t (:timestep this)
          boost? (zero? (mod t dcp))]
      ;; TODO: only learn on columns with current input?
      (cond-> (learn this a-cols ff-bits signal-ff-bits)
              true (update-in [:overlap-duty-cycles] update-duty-cycles
                              (keys overlaps) dcp)
              true (update-in [:active-duty-cycles] update-duty-cycles
                              a-cols dcp)
              boost? (update-boosting)
              boost? (update-inhibition-radius))))
  (inhibition-radius [_]
    inh-radius)
  (column-overlaps [_]
    overlaps)
  (column-signal-overlaps [_]
    sig-overlaps)
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
  (let [spec (merge columns-parameter-defaults spec)
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
       :sig-overlaps {}
       :boosts (vec (repeat n-cols 1.0))
       :active-duty-cycles (vec (repeat n-cols 0.0))
       :overlap-duty-cycles (vec (repeat n-cols 0.0))
       })
     (update-inhibition-radius))))
