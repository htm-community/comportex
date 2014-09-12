(ns org.nfrac.comportex.pooling
  "Currently this implements Spatial Pooling as in the CLA."
  (:require [org.nfrac.comportex.util :as util
             :refer [round mean count-filter]]))

(def spatial-pooler-defaults
  "Default parameter specification map for spatial pooling. Mostly
   based on values from NuPIC.

   * `ncol` - number of columns. Currently only one-dimensional.

   * `input-size` - number of input bits. Currently only one-dimensional.

   * `potential-radius` - range of potential feed-forward synapse
     connections; a distance in input bits.

   * `potential-frac` - fraction of inputs within range that will be
     part of the potentially connected set.

   * `global-inhibition` - whether to use the faster global algorithm
     for column inhibition (just keep those with highest overlap
     scores), or to apply inhibition only within a column's
     neighbours.

   * `activation-level` - fraction of columns that can be
     active (either locally or globally); inhibition kicks in to
     reduce it to this level.

   * `sp-perm-inc` - amount to increase a synapse's permanence value
     by when it is reinforced.

   * `sp-perm-dec` - amount to decrease a synapse's permanence value
     by when it is not reinforced.

   * `sp-perm-connected` - permanence value at which a synapse is
     functionally connected. Permanence values are defined to be
     between 0 and 1.

   * `stimulus-threshold` - minimum number of active input connections
     for a column to be _overlapping_ the input (i.e. active prior to
     inhibition).

   * `boost-overlap-duty-ratio` - when a column's overlap frequency is
     below this proportion of the _highest_ of its neighbours, its
     input synapses are boosted.

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
  {:input-size :define-me!
   :ncol 2048
   :potential-radius 128
   :potential-frac 0.5
   :global-inhibition false
   :activation-level 0.02
   :sp-perm-inc 0.05
   :sp-perm-dec 0.01
   :sp-perm-connected 0.1
   :stimulus-threshold 3
   :boost-overlap-duty-ratio 0.001
   :boost-active-duty-ratio 0.001
   :duty-cycle-period 1000
   :max-boost 3.0
   :temporal-pooling-decay 0.9
   :temporal-pooling-amp 1.1
   })

;; ## Construction

(defn into-connected-disconnected
  "Takes a collection of synapse connections and partitions them into
   connected and disconnected ones.

   * `pcon` - the permanence value at (or above) which synapses are
     connected.

   * `synapses` - a collection of `[input-id permanence]` tuples.

   Returns a map with keys `:connected` and `:disconnected`, each a
   map of the corresponding tuples."
  [pcon synapses]
  (util/group-by-maps (fn [id perm]
                        (if (>= perm pcon) :connected :disconnected))
                synapses))

(defn in-synapses
  "Generates a random selection of input (feed-forward) synapse
   connections to the given column. Both the input and columns are
   assumed to be indexed in one dimension.

   Connections are made locally by scaling the input space to the
   column space. Potential synapses are chosen within a radius
   `potential-radius` of input bits, and of those, a fraction
   `potential-frac` are chosen randomly.

   Initial permanence values are randomly selected from a uniform
   distribution around the connected threshold, plus-or-minus the
   increment amount. So about half will be initially connected."
  [column-id
   {:as spec
    :keys [ncol
           input-size
           potential-radius
           potential-frac
           sp-perm-connected
           sp-perm-inc]}]
  (let [input-focus (round (* input-size (/ column-id ncol)))
        idseq (range (max 0 (- input-focus potential-radius))
                     (min input-size (+ input-focus potential-radius)))
        n (* potential-frac (count idseq))
        ids (take n (util/shuffle idseq))
        perms (repeatedly n #(util/rand (- sp-perm-connected sp-perm-inc)
                                        (+ sp-perm-connected sp-perm-inc)))]
    (->> (map vector ids perms)
         (into-connected-disconnected sp-perm-connected))))

(defn column
  "Constructs a column with the given `id` index and with a randomised
   set of input synapses."
  [id spec]
  {:id id
   :in-synapses (in-synapses id spec)
   :neighbours #{} ;; cache
   :boost 1.0})

(declare update-neighbours)

(defn region
  "Constructs a region (as in the CLA) with the given specification
   map. See documentation on `spatial-pooler-defaults` and
   `sequence-memory-defaults` for possible keys. Any keys given here
   will override those default values.

   Initially this region has only columns, not individual cells, so can
   run spatial pooling but not sequence memory."
  [spec]
  (let [full-spec (merge spatial-pooler-defaults spec)
        ncol (:ncol full-spec)]
    (-> {:columns (mapv column (range ncol) (repeat full-spec))
         :spec full-spec
         :active-columns #{}
         :active-duty-cycles (vec (repeat (:ncol spec) 0))
         :overlap-duty-cycles (vec (repeat (:ncol spec) 0))}
        (update-neighbours))))

;; ## Neighbouring columns

(defn column-receptive-field-size
  "Returns the span over the input bit array to which this column has
   connected synapses."
  [col]
  (let [idxs (keys (:connected (:in-synapses col)))]
    (if (seq idxs)
      (- (apply max idxs) (apply min idxs))
      0)))

(defn avg-receptive-field-size
  [rgn]
  (mean (map column-receptive-field-size (:columns rgn))))

(defn neighbours
  "Returns the set of column ids within radius `r` in column space of
   this column `col-id`, not including itself."
  [col-id ncol r]
  (disj (set (range (max 0 (- col-id r))
                    (min ncol (+ col-id r 1))))
        col-id))

(defn update-neighbours
  "Recalculates the `:neighbours` of each column in the region, by
   first recalculating the average receptive field size."
  [rgn]
  (let [spec (:spec rgn)
        nin (:input-size spec)
        ncol (:ncol spec)
        arfs (avg-receptive-field-size rgn)
        diameter (round (* ncol (/ arfs nin)))
        radius (quot diameter 2)
        cols (mapv (fn [col]
                     (assoc col :neighbours
                            (neighbours (:id col) ncol radius)))
                   (:columns rgn))]
    (assoc rgn :columns cols
           :avg-receptive-field-size radius)))

;; ## Overlaps

(defn column-overlap
  "Given the set of active input bits `in-set`, returns the overlap
   score of column `col`. That is the number of synapses connected to
   active input bits, multiplied by the column's boosting factor. If
   the number of connections is below `stimulus-threshold` the result
   is nil."
  [col in-set stimulus-threshold]
  (let [syns (:connected (:in-synapses col))
        ov (count-filter in-set (keys syns))]
    (when (>= ov stimulus-threshold)
      (* ov (:boost col)))))

(defn overlaps
  "Given the set of active input bits `in-set`, finds the columns with
   an overlap score above parameter `stimulus-threshold`, and returns
   a map of their column ids to overlap scores."
  [columns in-set th]
  (into {} (keep (fn [col]
                   (when-let [v (column-overlap col in-set th)]
                     [(:id col) v]))
                 columns)))

(defn all-overlaps
  [rgn in-set]
  (let [th (:stimulus-threshold (:spec rgn))]
    (overlaps (:columns rgn) in-set th)))

;; ## Activation

(defn column-active-with-local-inhibition?
  "Given the map of column overlap scores `om`, and the target
   activation rate `level`, decides whether the column should become
   active. A false value indicates that the column is supressed by
   local inhibition from its neighbours."
  [col om level]
  (when-let [o-val (om (:id col))]
    (let [ns (:neighbours col)
          activity-limit (-> (* level (inc (count ns)))
                             (round)
                             (max 1))]
      (if (< (count-filter om ns) activity-limit)
        ;; no inhibition within neighbourhood
        true
        ;; inhibition within neighbourhood;
        ;; check number of neighbouring columns dominating col
        (let [o-val* (+ o-val (util/rand -0.1 0.1)) ;; break ties
              n-over (count-filter #(> (om % 0) o-val*) ns)]
          (< n-over activity-limit))))))

(defn active-columns-with-local-inhibition
  "Returns the set of column ids which should become active given the
   map of column overlap scores `om`, and the target activation rate
   `level`. Local inhibition is applied."
  [rgn om level]
  (->> (:columns rgn)
       (keep (fn [col]
               (when (column-active-with-local-inhibition? col om level)
                 (:id col))))
       (into #{})))

(defn active-columns-with-global-inhibition
  "Returns the set of column ids which should become active given the
   map of column overlap scores `om`, and the target activation rate
   `level`. Global inhibition is applied, i.e. the top N columns by
   overlap score are selected."
  [rgn om level]
  (let [activity-limit (-> (* level (count (:columns rgn)))
                           (round)
                           (max 1))]
    (if (<= (count om) activity-limit)
      ;; no inhibition
      (set (keys om))
      ;; inhibition
      (->> om
           (sort-by val >)
           (take activity-limit)
           (map first)
           (set)))))

(defn active-columns
  "Returns the set of column ids which should become active given the
   map of column overlap scores `om`. The method used depends on the
   specification key `:global-inhibition`."
  [rgn om]
  (let [spec (:spec rgn)
        level (:activation-level spec)]
    (if (:global-inhibition spec)
      (active-columns-with-global-inhibition rgn om level)
      (active-columns-with-local-inhibition rgn om level))))

;; ## Learning

(defn column-update-in-synapses
  "Given the set of input bits `in-set`, adjusts the permanence values
   of all potential feed-forward synapses on the column. Those linked
   to active inputs have their permanences increased by `pinc`, and
   the rest have their permanences decreased by `pdec`. They may
   become newly connected or disconnected according to the threshold
   `pcon`."
  [col in-set pinc pdec pcon]
  (let [syns (:in-synapses col)
        nsyns (->> (concat (:connected syns)
                           (:disconnected syns))
                   (mapv (fn [[id perm]]
                           (let [newp (if (in-set id)
                                        (min 1.0 (+ perm pinc))
                                        (max 0.0 (- perm pdec)))]
                             [id newp])))
                   (into-connected-disconnected pcon))]
    (assoc col :in-synapses nsyns)))

(defn learn
  "Adapt feed-forward connections to focus on consistent input
   patterns. Given the set of input bits `in-set`, adjusts the
   permanence values of all potential feed-forward synapses in each
   column in the region."
  [rgn as in-set]
  (let [pinc (:sp-perm-inc (:spec rgn))
        pdec (:sp-perm-dec (:spec rgn))
        pcon (:sp-perm-connected (:spec rgn))
        cols (reduce (fn [cols i]
                       (update-in cols [i]
                                  (fn [col]
                                    (column-update-in-synapses col in-set pinc pdec pcon))))
                     (:columns rgn) as)]
    (assoc rgn :columns cols)))

;; ## Boosting

(defn column-increase-permanences
  "Used to increase a column's feed-forward connections within its
   potential pool. All input synapses have their permanences
   increased, perhaps passing the connection threshold `pcon`."
  [col pcon]
  (let [syns (:in-synapses col)
        nsyns (->> (concat (:connected syns)
                           (:disconnected syns))
                   (mapv (fn [[id perm]]
                           (let [newp (min 1.0 (+ perm (* pcon 0.1)))]
                             [id newp])))
                   (into-connected-disconnected pcon))]
    (assoc col :in-synapses nsyns)))

(defn- column-update-boosting
  "Recalculates the column's `:boost` factor and possibly applies an
   increase to all input synapse permanences. These are based on
   comparing the number of activations or overlaps in recent history
   to the _maximum_ such value from its neighbours."
  [col ods ads o-th a-th maxb pcon]
  (let [ns (:neighbours col)
        max-od (apply max 1 (vals (select-keys ods ns)))
        max-ad (apply max 1 (vals (select-keys ads ns)))
        crit-od (* o-th max-od)
        crit-ad (* a-th max-ad)
        od (get ods (:id col))
        ad (get ads (:id col))
        nboost (-> (- maxb (* (- maxb 1)
                              (/ ad crit-ad)))
                   (max 1.0)
                   (double))]
    (if (< od crit-od)
      (column-increase-permanences col pcon)
      (assoc col :boost nboost))))

(defn update-boosting
  "For each column, determines whether it has had too few activations
  -- relative to its neighbours -- in recent history. Boosting may be
  applied to boost either a column's input overlap (by increasing
  connections) or its share of activations after inhibition (by
  increasing its boost factor)."
  [rgn t]
  (let [o-th (:boost-overlap-duty-ratio (:spec rgn))
        a-th (:boost-active-duty-ratio (:spec rgn))
        maxb (:max-boost (:spec rgn))
        pcon (:sp-perm-connected (:spec rgn))
        ods (:overlap-duty-cycles rgn)
        ads (:active-duty-cycles rgn)
        cols (mapv (fn [col]
                     (column-update-boosting col ods ads o-th a-th maxb pcon))
                   (:columns rgn))]
    (assoc rgn :columns cols)))

(defn update-duty-cycles
  "Records a set of events with indices `is` in the vector `v`
   according to duty cycle period `period`. As in NuPIC, the formula
   is

`y[t] = (period-1) * y[t-1]  +  1
       --------------------------
         period`"
  [v is period]
  (let [d (/ 1.0 period)
        decay (* d (dec period))]
    (-> (mapv #(* % decay) v)
        (util/update-each is #(+ % d)))))

;; ## Temporal Pooling

(defn signal-overlaps
  "Finds columns with input from the `signal-in-set`, returning the
   overlap scores in a map. As with normal overlaps they must meet
   `stimulus-threshold` to be included. Also, only active
   columns (column ids `as`) are considered."
  [rgn as signal-in-set]
  (let [th (:stimulus-threshold (:spec rgn))
        o-cols (map (partial get (:columns rgn)) as)]
    (overlaps o-cols signal-in-set th)))

(defn temporal-pooling-scores
  "Selects a set of columns to be in a temporal pooling state,
   returning a map from their column ids to an (adjusted) overlap
   score -- which is used to compete for activation next time step.

   `as` - set of active column ids.

   `signal-in-set` - input set from predicted cells.

   `prev-tpm` - temporal pooling overlap scores from previous step.

   `om` - overlap scores from current feedforward input."
  [rgn as signal-in-set prev-tpm om]
  (let [amp (:temporal-pooling-amp (:spec rgn))
        new-tpm (->> (signal-overlaps rgn as signal-in-set)
                     (util/remap (partial * amp)))
        ;; if a previous TP column received a new dominant input
        ;; then its TP status is interrupted (unless in new-tpm)
        stopped-tps (keep (fn [[k v]] (when (> (om k 0) v) k))
                          prev-tpm)
        ;; which of the previous TP columns became active?
        ;; their TP status continues but overlap scores decay.
        decay (:temporal-pooling-decay (:spec rgn))
        kept-tpm (->> (apply disj as stopped-tps)
                      (select-keys prev-tpm)
                      (util/remap (partial * decay)))]
    (merge-with max new-tpm kept-tpm)))

;; ## Orchestration

(defn pooling-step
  "Given a set of active input bits `in-set` and a region `rgn`,
   performs an iteration of the CLA spatial pooling algorithm:

   * increments the `:timestep`.
   * maps column ids to overlap scores in `:overlaps`.
   * calculates the set of active column ids `:active-columns`.
   * updates TP columns mapped to overlap scores in `:temporal-pooling-scores`.
   * performs the learning step by updating input synapse permanences.
   * after every `duty-cycle-period` duration,
     * applies column boosting as needed
     * recalculates the neighbours of each column according to the
       average receptive field size.

   The argument `signal-in-set` gives the subset of `in-set` that came
   from cells that correctly predicted their activation in lower
   layers. This is used for temporal pooling. Set to `#{}` to disable."
  ([rgn in-set learn?]
     (pooling-step rgn in-set #{} learn?))
  ([rgn in-set signal-in-set learn?]
   (let [t (inc (:timestep rgn 0))
         curr-om (all-overlaps rgn in-set)
         prev-tpm (:temporal-pooling-scores rgn {})
         om (merge-with max curr-om prev-tpm)
         as (active-columns rgn om)
         tpm (if (seq signal-in-set)
               (temporal-pooling-scores rgn as signal-in-set prev-tpm curr-om)
               {})
         dcp (:duty-cycle-period (:spec rgn))
         boost? (and learn? (zero? (mod t dcp)))]
     (cond->
      (assoc rgn
        :timestep t
        :overlaps om
        :active-columns as
        :temporal-pooling-scores tpm)
      learn? (update-in [:overlap-duty-cycles] update-duty-cycles (keys om) dcp)
      learn? (update-in [:active-duty-cycles] update-duty-cycles as dcp)
      learn? (learn (filter curr-om as) in-set) ;; only learn if current feedforward input
      boost? (update-boosting t)
      boost? (update-neighbours)))))
