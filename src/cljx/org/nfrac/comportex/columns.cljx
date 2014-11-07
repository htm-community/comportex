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

   * `ff-stimulus-threshold` - minimum number of active input connections
     for a column to be _overlapping_ the input (i.e. active prior to
     inhibition).

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
     increase activation frequency."
  {:input-dimensions [:define-me!]
   :column-dimensions [2048]
   :ff-potential-radius 0.3
   ;:ff-potential-frac 0.5
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
   })

(defn uniform-ff-synapses
  "Generates feed-forward synapses connecting columns to the input bit
   array.

   Connections are made locally by scaling the input space to the
   column space. Potential synapses are chosen within a radius in
   input space of `ff-potential-radius` fraction of the longest single
   dimension, and of those, `ff-init-frac` are chosen from a
   uniform random distribution.

   Initial permanence values are uniformly distributed between one
   increment above the connected threshold, down to two increments
   below. So about one third will be initially connected."
  [topo itopo spec]
  (let [pcon (:ff-perm-connected spec)
        pinc (:ff-perm-inc spec)
        p-hi (-> (+ pcon (* 1.0 pinc)) (min 1.0))
        p-lo (-> (- pcon (* 2.0 pinc)) (max 0.0))
        global? (>= (:ff-potential-radius spec) 1.0)
        ;; radius in input space, fraction of longest dimension
        radius (long (* (:ff-potential-radius spec)
                        (apply max (p/dimensions itopo))))
        frac (:ff-init-frac spec)
        input-size (p/size itopo)
        n-cols (p/size topo)
        one-d? (or (== 1 (count (p/dimensions topo)))
                   (== 1 (count (p/dimensions itopo))))
        [cw ch] (p/dimensions topo)
        [iw ih] (p/dimensions itopo)]
    (if global?
      (->> (range n-cols)
           (mapv (fn [col]
                   (let [n (round (* frac input-size))
                         ids (repeatedly n #(util/rand-int (dec input-size))) ;; ignore dups
                         perms (repeatedly n #(util/rand p-lo p-hi))]
                     (zipmap ids perms)))))
      (->> (range n-cols)
           (mapv (fn [col]
                   (let [focus-i (if one-d?
                                   (round (* input-size (/ col n-cols)))
                                   (let [[cx cy] (p/coordinates-of-index topo col)]
                                     (p/index-of-coordinates itopo [(round (* iw (/ cx cw)))
                                                                    (round (* ih (/ cy ch)))])))
                         all-ids (vec (p/neighbours-indices itopo focus-i radius))
                         n (round (* frac (count all-ids)))
                         ids (if (< frac 0.4) ;; for performance:
                               (util/sample n all-ids)
                               (util/reservoir-sample n all-ids))
                         perms (repeatedly n #(util/rand p-lo p-hi))]
                     (zipmap ids perms))))))))

;;; ## Overlaps

(defn apply-overlap-boosting
  "Given a map `om` of the raw overlap counts, finds the columns with
  an overlap count above parameter `ff-stimulus-threshold`, and
  returns a map of their column ids to excitations. That is, the
  number of input bits connected to, multiplied by the column boosting
  factor."
  [om-raw boosts spec]
  (let [th (:ff-stimulus-threshold spec)]
    (->> om-raw
         (reduce-kv (fn [om col x]
                      (if (< x th)
                        om
                        (let [b (get boosts col)]
                          (assoc! om col (* x b)))))
                    (transient {}))
         (persistent!))))

;;; ## Learning

(defn ff-new-synapse-ids
  [ff-bits curr-ids-set col itopo focus-coord radius n-grow]
  (loop [ids ()
         on-bits (util/shuffle ff-bits)]
    (if (or (empty? on-bits)
            (>= (count ids) n-grow))
      ids
      (let [id (first ff-bits)]
        (if (curr-ids-set id)
          ;; already have this synapse
          (recur ids (next on-bits))
          ;; check distance
          (let [coord (p/coordinates-of-index itopo id)
                dist (p/coord-distance itopo coord focus-coord)]
            (if (< dist radius)
              ;; ok, choose this for a new synapse
              ;; TODO - ff-potential-frac
              (recur (conj ids id) (next on-bits))
              ;; out of radius
              (recur ids (next on-bits)))))))))

(defn grow-new-synapses
  [ff-sg col ff-bits itopo radius n-cols n-grow pinit]
  (let [input-size (p/size itopo)
        focus-i (round (* input-size (/ col n-cols)))
        focus-coord (p/coordinates-of-index itopo focus-i)
        new-ids (ff-new-synapse-ids ff-bits
                                    (p/in-synapses ff-sg col)
                                    col itopo
                                    focus-coord
                                    radius n-grow)]
    (p/conj-synapses ff-sg col new-ids pinit)))

(defn learn
  "Adapt feed-forward synapses to focus on observed input patterns.
   Given the set of input bits `ff-bits`, adjusts the permanence
   values of all potential feed-forward synapses in the active columns
   `a-cols`."
  [cf a-cols ff-bits om]
  (let [itopo (:input-topology cf)
        spec (:spec cf)
        pinc (:ff-perm-inc spec)
        pdec (:ff-perm-dec spec)
        pinit (:ff-perm-init spec)
        grow-and-die? (:ff-grow-and-die? spec)
        grow-up-to (:ff-grow-up-to-count spec)
        max-syns (:ff-max-synapse-count spec)
        ;; radius in input space, fraction of longest dimension
        radius (long (* (:ff-potential-radius spec)
                        (apply max (p/dimensions itopo))))
        n-cols (p/size-of cf)]
    (update-in cf [:ff-sg]
               (fn [ff-sg]
                 (reduce (fn [sg col]
                           (let [n-on (om col)
                                 n-grow (max 0 (- grow-up-to n-on))]
                             (cond->
                              (p/reinforce-in-synapses sg col (constantly false)
                                                       ff-bits pinc pdec)
                              (and grow-and-die? (pos? n-grow))
                              (grow-new-synapses col ff-bits itopo radius n-cols
                                                 n-grow pinit))))
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

;;; ## Orchestration

(defn update-inhibition-radius
  [cf]
  (assoc cf :inh-radius (inh/inhibition-radius (:ff-sg cf) (:topology cf)
                                               (:input-topology cf))))

(defrecord ColumnField
    [spec ff-sg topology input-topology overlaps sig-overlaps prox-exc
     inh-radius boosts active-duty-cycles overlap-duty-cycles]
  p/PColumnField
  (columns-step
    [this ff-bits signal-ff-bits]
    (let [om (syn/excitations ff-sg ff-bits)
          exc (apply-overlap-boosting om boosts spec)
          sig-om (syn/excitations ff-sg signal-ff-bits)]
      (cond->
       (assoc this
         :timestep (inc (:timestep this 0))
         :prox-exc exc
         :overlaps om
         :sig-overlaps sig-om))))
  (columns-learn
    [this ff-bits a-cols]
    (let [dcp (:duty-cycle-period spec)
          t (:timestep this)
          boost? (zero? (mod t dcp))]
      (cond-> (learn this a-cols ff-bits overlaps)
              true (update-in [:overlap-duty-cycles] update-duty-cycles
                              (keys prox-exc) dcp)
              true (update-in [:active-duty-cycles] update-duty-cycles
                              a-cols dcp)
              boost? (update-boosting)
              boost? (update-inhibition-radius))))
  (inhibition-radius [_]
    inh-radius)
  (column-excitation [_]
    prox-exc)
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
        all-syns (uniform-ff-synapses col-topo input-topo spec)]
    (->
     (map->ColumnField
      {:spec spec
       :ff-sg (syn/synapse-graph all-syns n-inbits
                                 (:ff-perm-connected spec)
                                 (:ff-max-synapse-count spec)
                                 (:ff-grow-and-die? spec))
       :topology col-topo
       :input-topology input-topo
       :inh-radius 1
       :prox-exc {}
       :overlaps {}
       :sig-overlaps {}
       :boosts (vec (repeat n-cols 1.0))
       :active-duty-cycles (vec (repeat n-cols 0.0))
       :overlap-duty-cycles (vec (repeat n-cols 0.0))
       })
     (update-inhibition-radius))))
