(ns org.nfrac.comportex.pooling
  "Currently this implements Spatial Pooling as in the CLA."
  (:require [org.nfrac.comportex.util :as util
             :refer [abs round mean count-filter remap]]
            [cljs-uuid.core :as uuid]))

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

   * `sp-perm-signal-inc` - amount to increase a synapse's permanence
     value by when it is reinforced by input from a correctly
     predicted cell.

   * `sp-perm-connected` - permanence value at which a synapse is
     functionally connected. Permanence values are defined to be
     between 0 and 1.

   * `stimulus-threshold` - minimum number of active input connections
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
  {:input-size :define-me!
   :ncol 2048
   :potential-radius 256
   :potential-frac 0.5
   :global-inhibition false
   :activation-level 0.02
   :sp-perm-inc 0.05
   :sp-perm-dec 0.01
   :sp-perm-signal-inc 0.50
   :sp-perm-connected 0.1
   :stimulus-threshold 3
   :boost-overlap-duty-ratio 0.001
   :boost-active-duty-ratio 0.001
   :duty-cycle-period 1000
   :max-boost 3.0
   :temporal-pooling-decay 0.9
   :temporal-pooling-amp 1.1
   })

;;; ## Synapse indexing

(defn init-ff-index
  "Builds an internal index from each input bit to the set of columns
   connected to it. This speeds up overlaps calculations."
  [rgn]
  (let [n-in (:input-size (:spec rgn))]
    (->>
     (reduce (fn [ffi col]
               (let [cid (:id col)
                     is (keys (:connected (:ff-synapses col)))]
                 (util/update-each ffi is #(conj % cid))))
             (vec (repeat n-in #{}))
             (:columns rgn))
     (assoc rgn ::ff-index))))

(defn update-ff-synapses
  "Updates the permanences of all potential feed-forward synapses on
   the column, either incrementing or decrementing according to
   `reinforce?`, which is a function of the input id. Synapses may
   become newly connected or disconnected, in which case the internal
   feed-forward index is updated."
  [rgn cid reinforce? signal?]
  (let [pcon (:sp-perm-connected (:spec rgn))
        pinc (:sp-perm-inc (:spec rgn))
        pdec (:sp-perm-dec (:spec rgn))
        psiginc (:sp-perm-signal-inc (:spec rgn))
        syns (get-in rgn [:columns cid :ff-synapses])
        g-conn (util/group-by-maps (fn [i p]
                                     (if (reinforce? i)
                                       (if (signal? i)
                                         :signal-up
                                         :up)
                                       (if (< p (+ pcon pdec))
                                         :demote :down)))
                                   (:connected syns))
        g-disc (util/group-by-maps (fn [i p]
                                     (if (reinforce? i)
                                       (if (signal? i)
                                         (if (>= p (- pcon psiginc))
                                           :signal-promote :signal-up)
                                         (if (>= p (- pcon pinc))
                                           :promote :up))
                                       :down))
                                   (:disconnected syns))
        new-syns {:connected
                  (merge (remap #(min (+ % pinc) 1.0)
                                (merge (:up g-conn)
                                       (:promote g-disc)))
                         (remap #(min (+ % psiginc) 1.0)
                                (merge (:signal-up g-conn)
                                       (:signal-promote g-disc)))
                         (remap #(- % pdec) (:down g-conn)))
                  :disconnected
                  (merge (remap #(max (- % pdec) 0.0)
                                (merge (:down g-disc)
                                       (:demote g-conn)))
                         (remap #(+ % pinc) (:up g-disc))
                         (remap #(+ % psiginc) (:signal-up g-disc)))}
        to-promote (concat (keys (:promote g-disc))
                           (keys (:signal-promote g-disc)))
        to-demote (keys (:demote g-conn))
        new-ffi (-> (::ff-index rgn)
                    (util/update-each to-promote #(conj % cid))
                    (util/update-each to-demote #(disj % cid)))]
    (-> rgn
        (assoc-in [:columns cid :ff-synapses] new-syns)
        (assoc ::ff-index new-ffi))))

;;; ## Construction

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

(defn init-ff-synapses
  "Generates feed-forward synapses connecting to the given column.
   Both the input and columns are indexed in one dimension.

   Connections are made locally by scaling the input space to the
   column space. Potential synapses are chosen within a radius
   `potential-radius` of input bits, and of those, a fraction
   `potential-frac` are chosen randomly.

   Initial permanence values are randomly distributed between one
   increment above the connected threshold, down to two increments
   below. So about one third will be initially connected."
  [cid
   {:as spec
    :keys [ncol
           input-size
           potential-radius
           potential-frac
           sp-perm-connected
           sp-perm-inc]}]
  (let [input-focus (round (* input-size (/ cid ncol)))
        idseq (range (max 0 (- input-focus potential-radius))
                     (min input-size (+ input-focus potential-radius)))
        n (* potential-frac (count idseq))
        ids (take n (util/shuffle idseq))
        p-hi (-> (+ sp-perm-connected (* 1.0 sp-perm-inc)) (min 1.0))
        p-lo (-> (- sp-perm-connected (* 2.0 sp-perm-inc)) (max 0.0))
        perms (repeatedly n #(util/rand p-lo p-hi))
        ;; triangular:
        #_(for [i ids]
                ;; z is 1 at input focus, down to 0 linearly at radius
                (let [z (- 1.0 (/ (util/abs (- i input-focus))
                                  potential-radius))]
                  (+ p-lo (* z (- p-hi p-lo)))))]
    (->> (map vector ids perms)
         (into-connected-disconnected sp-perm-connected))))

(defn column
  "Constructs a column with the given `id` index and with a randomised
   set of feed-forward synapses."
  [id spec]
  {:id id
   :ff-synapses (init-ff-synapses id spec)
   :boost 1.0})

(declare update-neighbour-radius)

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
    (-> {:uuid (uuid/make-random)
         :columns (mapv column (range ncol) (repeat full-spec))
         :spec full-spec
         :active-columns #{}
         :active-duty-cycles (vec (repeat (:ncol spec) 0))
         :overlap-duty-cycles (vec (repeat (:ncol spec) 0))}
        (init-ff-index)
        (update-neighbour-radius))))

;;; ## Neighbouring columns

(defn neighbours
  "Returns the column ids within `outer-r`adius in column space of
   this column `cid`, but excluding any within `inner-r`adius."
  [cid outer-r inner-r ncol]
  (concat (range (min (+ cid inner-r 1) ncol)
                 (min (+ cid outer-r 1) ncol))
          (range (max (- cid outer-r) 0)
                 (max (- cid inner-r) 0))))

(defn column-receptive-field-size
  "Returns the span over the input bit array to which this column has
   connected synapses."
  [col]
  (let [idxs (keys (:connected (:ff-synapses col)))]
    (if (seq idxs)
      (- (apply max idxs) (apply min idxs))
      0)))

(defn avg-receptive-field-size
  [rgn]
  (mean (map column-receptive-field-size (:columns rgn))))

(defn neighbour-radius
  "The radius in column space defining neighbouring columns, based on
   the average receptive field size. Specifically, neighbouring
   columns are defined by sharing at least 30% of their receptive
   fields, on average."
  [rgn]
  (let [spec (:spec rgn)
        nin (:input-size spec)
        ncol (:ncol spec)
        shared-frac 0.3
        arfs (avg-receptive-field-size rgn)
        ;; columns in this range will have some overlap of inputs
        cols-diameter (* ncol (/ arfs nin))
        cols-radius (quot cols-diameter 2)]
    ;; to share a given fraction of receptive fields
    (-> (* cols-radius (- 1.0 shared-frac))
        (round)
        (max 1))))

(defn update-neighbour-radius
  [rgn]
  (assoc rgn :neighbour-radius (neighbour-radius rgn)))

;;; ## Overlaps

(defn all-overlaps-raw
  "Computes a map of column ids to the number of bits in `in-set`
   they are connected to."
  [rgn in-set]
  (let [ffi (::ff-index rgn)]
    (-> (reduce (fn [om i]
                  (reduce (fn [om cid]
                            (assoc! om cid (inc (get om cid 0))))
                          om (ffi i)))
                (transient {})
                in-set)
        (persistent!))))

(defn all-overlaps
  "Given the set of active input bits `in-set`, finds the columns with
   an overlap count above parameter `stimulus-threshold`, and returns
   a map of their column ids to overlap scores. That is, the number of
   input bits connected to multiplied by the column boosting factor."
  [rgn in-set]
  (let [omr (all-overlaps-raw rgn in-set)
        th (:stimulus-threshold (:spec rgn))]
    (->> omr
         (reduce-kv (fn [om cid v]
                      (if (< v th)
                        om
                        (let [boost (get-in rgn [:columns cid :boost])]
                          (assoc! om cid (* v boost)))))
                    (transient {}))
         (persistent!))))

;;; ## Activation

(defn active-columns-with-global-inhibition
  "Returns the set of column ids which should become active given the
   map of column overlap scores `om`, and the target activation rate
   `level`. Global inhibition is applied, i.e. the top N columns by
   overlap score are selected."
  [rgn om level]
  (let [n-on (-> (* level (count (:columns rgn)))
                 (round)
                 (max 1))]
    (loop [oms (seq om)
           am (sorted-map-by #(compare (om %1) (om %2)))
           curr-min 1000000.0]
      (if (empty? oms)
        (set (keys am))
        (let [[cid o] (first oms)]
          (cond
           ;; just initialising the set
           (< (count am) n-on)
           (recur (next oms)
                  (assoc am cid o)
                  (double (min curr-min o)))
           ;; include this one, dominates previous min
           (> o curr-min)
           (let [new-am (-> (dissoc am (first (keys am)))
                            (assoc cid o))]
             (recur (next oms)
                    new-am
                    (double (first (vals new-am)))))
           ;; exclude this one
           :else
           (recur (next oms) am curr-min)))))))

(defn dominant-overlap-diff
  [cid n-cid]
  (let [away (util/abs (- cid n-cid))]
   (-> away
       (- 4) ; within this radius, there can be only one
       (max 0)
       (/ 2.0)))) ; for every 2 columns away, need one extra overlap to dominate

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
  [om rgn outer-radius inner-radius]
  (let [cols (:columns rgn)
        ncol (count cols)]
    (loop [cids (keys om)
           mask (transient (map->vec ncol om))]
      (if-let [cid (first cids)]
        (if-let [o (mask cid)]
          (recur
           (next cids)
           ;; loop through neighbours and mask out any dominated
           (loop [ns (neighbours cid outer-radius inner-radius ncol)
                  mask mask]
             (if-let [n-cid (first ns)]
               (if-let [no (mask n-cid)]
                 (let [odom (dominant-overlap-diff cid n-cid)]
                   (cond
                    ;; neighbour is dominated
                    (>= o (+ no odom))
                    (recur (next ns)
                           (assoc! mask n-cid nil))
                    ;; we are dominated by neighbour; abort
                    (<= o (- no odom))
                    (assoc! mask cid nil)
                    ;; neither dominates
                    :else
                    (recur (next ns) mask)))
                 ;; neighbour has no overlap or was eliminated
                 (recur (next ns) mask))
               ;; finished with neighbours
               mask)))
          ;; already eliminated, skip
          (recur (next cids) mask))
        ;; finished
        (vec->map (persistent! mask))))))

(defn perturb-overlaps
  [om]
  (remap #(+ % (util/rand 0 0.5)) om))

(defn active-columns
  "Returns the set of column ids which should become active given the
   map of column overlap scores `om`. The method used depends on the
   specification key `:global-inhibition`."
  [rgn om]
  (let [spec (:spec rgn)
        level (:activation-level spec)
        do-local? (not (:global-inhibition spec))
        radius (:neighbour-radius rgn)
        omi (cond-> (perturb-overlaps om)
                    do-local? (apply-local-inhibition rgn (quot radius 3) 0)
                    do-local? (apply-local-inhibition rgn radius (quot radius 3)))]
    (active-columns-with-global-inhibition rgn omi level)))

;;; ## Learning

(defn learn
  "Adapt feed-forward connections to focus on consistent input
   patterns. Given the set of input bits `in-set`, adjusts the
   permanence values of all potential feed-forward synapses in each
   column in the region."
  [rgn as in-set signal-in-set]
  (reduce (fn [rgn cid]
            (update-ff-synapses rgn cid in-set signal-in-set))
          rgn as))

;;; ## Boosting

(defn update-column-boosting
  "Recalculates the column's `:boost` factor and possibly applies an
   increase to all feedforward synapse permanences. This is based on
   comparing the number of activations or overlaps in recent history
   to the _maximum_ such value from its neighbours."
  [rgn cid]
  (let [o-th (:boost-overlap-duty-ratio (:spec rgn))
        a-th (:boost-active-duty-ratio (:spec rgn))
        maxb (:max-boost (:spec rgn))
        ods (:overlap-duty-cycles rgn)
        ads (:active-duty-cycles rgn)
        ncol (count (:columns rgn))
        ns (neighbours cid (:neighbour-radius rgn) 0 ncol)
        max-od (apply max 1 (vals (select-keys ods ns)))
        max-ad (apply max 1 (vals (select-keys ads ns)))
        crit-od (* o-th max-od)
        crit-ad (* a-th max-ad)
        od (get ods cid)
        ad (get ads cid)
        nboost (-> (- maxb (* (- maxb 1)
                              (/ ad crit-ad)))
                   (max 1.0)
                   (double))]
    (if (< od crit-od)
      (update-ff-synapses rgn cid (constantly true) (constantly false))
      (assoc-in rgn [:columns cid :boost] nboost))))

(defn update-boosting
  "For each column, determines whether it has had too few activations
  -- relative to its neighbours -- in recent history. Boosting may be
  applied to boost either a column's input overlap (by increasing
  connections) or its share of activations after inhibition (by
  increasing its boost factor)."
  [rgn]
  (reduce update-column-boosting
          rgn (range (count (:columns rgn)))))

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

(defn temporal-pooling-scores
  "Selects a subset of active columns to be in a temporal pooling state,
   returning a map from their column ids to an (adjusted) overlap
   score -- which is used to compete for activation next time step.

   `as` - set of active column ids.

   `signal-in-set` - input set from predicted cells.

   `prev-tpm` - temporal pooling overlap scores from previous step.

   `curr-om` - overlap scores from current feedforward input."
  [rgn as signal-in-set prev-tpm curr-om]
  (let [amp (:temporal-pooling-amp (:spec rgn))
        sig-om (all-overlaps rgn signal-in-set)
        new-tpm (->> (select-keys sig-om as)
                     (remap (partial * amp)))
        ;; if a previous TP column received a new dominant input
        ;; then its TP status is interrupted (unless in new-tpm)
        stopped-tps (keep (fn [[k v]] (when (> (curr-om k 0) v) k))
                          prev-tpm)
        ;; which of the previous TP columns became (remained) active?
        ;; their TP status continues, and overlap scores decay.
        decay (:temporal-pooling-decay (:spec rgn))
        kept-tpm (->> (apply disj as stopped-tps)
                      (select-keys prev-tpm)
                      (remap (partial * decay)))]
    (merge-with max new-tpm kept-tpm)))

;;; ## Orchestration

(defn pooling-step
  "Given a set of active input bits `in-set` and a region `rgn`,
   performs an iteration of the CLA spatial pooling algorithm:

   * increments the `:timestep`.
   * maps column ids to overlap scores in `:overlaps`.
   * calculates the set of active column ids `:active-columns`.
   * updates TP columns mapped to overlap scores in `:temporal-pooling-scores`.
   * performs the learning step by updating feed-forward synapse permanences.
   * after every `duty-cycle-period` duration,
     * applies column boosting as needed
     * recalculates the neighbours of each column according to the
       average receptive field size.

   The argument `signal-in-set` gives the subset of `in-set` that came
   from cells that correctly predicted their activation in lower
   layers. This is used for temporal pooling."
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
      learn? (learn (filter curr-om as) ;; only learn on columns with current input
                    in-set signal-in-set)
      boost? (update-boosting)
      boost? (update-neighbour-radius)))))
