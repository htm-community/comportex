(ns org.nfrac.comportex.core
  "Familiarity with the Numenta Cortical Learning Algorithm is assumed.

   A _region_ is the main composable unit in this library. It
   represents an array of neurons arranged in columns, responding to
   an input bit array.

### Regions

   A region data structure is a map with keys:

   * `:columns` a vector of columns described below;

   * `:spec` the specification map giving parameter values. For the
     basic spatial pooling functionality it should have the keys in
     `p/spatial-pooler-defaults`; for sequence memory additional keys
     are used as in `sm/sequence-memory-defaults`.

   * (others updated by the orchestration functions:)
     * `:timestep` the simulation time step, an iteration number.
     * `:overlaps` a map from column id to overlap score.
     * `:active-columns` a set of the active column ids.
     * `:active-cells` a set of the active cell ids.
     * `:bursting-columns` a set of the bursting column ids.
     * `:predictive-cells` a set of the predictive cell ids.

### Columns

   A column data structure represents a minicolumn of neurons. For
   spatial pooling a column is represented without any individual
   neuron cells. It is a map with keys:

   * `:id` the column id, currently an integer index.

   * `:in-synapses` storing the _feed-forward_ connections to this
     column, from the input source. Each connection has an associated
     _permanence_ value (between 0 and 1). For convenience the
     synapses are kept split between sub-keys `:connected` and
     `:disconnected`, each storing a map from input index to a
     permanence value.

   * `:boost` the column boosting factor, initially 1.0, used to
     increase a column's activation frequency.

   * `:overlap-history` a sorted set of the timesteps (integers) when
     this column was significantly overlapping with feedforward input.
     May be truncated to only the most recent.

   * `:active-history` a sorted set of the timesteps (integers) when
     this column was active. This is a subset of `:overlap-history`
     with the difference due to inhibition. May be truncated to only
     the most recent.

### Cells

   A region may optionally include individual neuron cells within each
   column. These enable the sequence memory properties of the Cortical
   Learning Algorithm. In this case each column has a key `:cells`, a
   vector of cells.

   A cell data structure is a map with keys:

   * `:id` the cell id as a tuple of `[column-index cell-index]`.

   * `:segments` a vector of dendrite segments. See below.

### Segments

   Dendrite segments consist of a number of synapses connecting a cell
   with other cells in the region. Each synapse has an associated
   permanence value.

   A segment data structure is a map with keys:

   * `:synapses` a map from cell ids to permanence values (between 0
     and 1). Note that unlike the feed-forward synapses these are not
     stored as connected vs disconnected groups."
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.sequence-memory :as sm]
            [org.nfrac.comportex.encoders :as enc]
            [clojure.set :as set]))

(defprotocol PFeedForward
  "A feedforward input source with a bit set representation. Could be
   sensory input or a lower level region."
  (bit-width [this])
  (bits-value* [this offset])
  (signal-bits-value* [this offset])
  (feed-forward-step* [this learn?]))

(defn bits-value
  ([this] (bits-value* this 0))
  ([this offset] (bits-value* this offset)))

(defn signal-bits-value
  ([this] (signal-bits-value* this 0))
  ([this offset] (signal-bits-value* this offset)))

(defn feed-forward-step
  ([this] (feed-forward-step* this true))
  ([this learn?] (feed-forward-step* this learn?)))

(defprotocol PInputGenerator
  "Maintains an input stream."
  (domain-value [this])
  (input-reset [this]))

(defprotocol PRegion
  (n-columns [this])
  (n-cells-per-column [this])
  (region-step [this in-bits signal-in-bits learn?])
  (active-cells [this])
  (signal-cells [this]))

(extend-protocol PRegion
  ;; default implementation - for hashmaps
  #+cljs object
  #+clj java.lang.Object
  (n-columns [this]
    (:ncol (:spec this)))
  (n-cells-per-column [this]
    (:depth (:spec this)))
  (region-step [this in-bits signal-in-bits learn?]
    (let [r-sp (p/pooling-step this in-bits signal-in-bits learn?)]
      (sm/sequence-memory-step r-sp (:active-columns r-sp) learn?)))
  (active-cells [this]
    (:active-cells this))
  (signal-cells [this]
    (:signal-cells this)))

(defn cla-region
  [spec]
  (-> (p/region spec)
      (sm/with-sequence-memory)))

(defrecord InputGenerator [init-value value transform encoder]
  PFeedForward
  (bit-width [_] (enc/encoder-bit-width encoder))
  (bits-value* [_ offset] (enc/encode encoder offset value))
  (signal-bits-value* [_ offset] #{})
  (feed-forward-step* [this _] (assoc this :value (transform value)))
  PInputGenerator
  (domain-value [_] value)
  (input-reset [this] (assoc this :value init-value)))

(defn input-generator
  "Creates an input stream generator from an initial value, a function
   to transform the input to the next time step, and an encoder."
  [init-value transform encoder]
  (->InputGenerator init-value init-value transform encoder))

(defn combined-bit-width
  "Returns the total bit width from a collection of
   sources (satisfying PFeedForward)."
  [ffs]
  (reduce + (map bit-width ffs)))

(defn combined-bits-value
  "Returns the total bit set from a collection of sources (satisfying
   PFeedForward). `bits-fn` should be `bits-value*` or
   `signal-bits-value*`."
  [ffs bits-fn]
  (let [ws (map bit-width ffs)
        os (list* 0 (reductions + ws))]
    (->> (map bits-fn ffs os)
         (apply set/union))))

(defn cell-id->inbit
  [offset depth [cid i]]
  (+ offset (* depth cid) i))

(defrecord RegionTree [region subs]
  PFeedForward
  (bit-width [_]
    (* (n-cells-per-column region)
       (n-columns region)))
  (bits-value* [_ offset]
    (let [depth (n-cells-per-column region)]
      (->> (active-cells region)
           (mapv (partial cell-id->inbit offset depth))
           (into #{}))))
  (signal-bits-value* [_ offset]
    (let [depth (n-cells-per-column region)]
      (->> (signal-cells region)
           (mapv (partial cell-id->inbit offset depth))
           (into #{}))))
  (feed-forward-step* [this learn?]
    (let [new-subs (map #(feed-forward-step* % learn?) subs)
          new-rgn (region-step region
                               (combined-bits-value new-subs bits-value*)
                               (combined-bits-value new-subs signal-bits-value*)
                               learn?)]
      (assoc this :region new-rgn :subs new-subs))))

(defn region-tree
  [rgn subs]
  (->RegionTree rgn subs))

(defn tree
  [build-region spec subs]
  (-> (assoc spec :input-size (combined-bit-width subs)
             :potential-radius (quot (combined-bit-width subs) 4)) ; TODO
      (build-region)
      (region-tree subs)))

(defn tree-reset-inputs
  [this]
  (if (satisfies? PInputGenerator this)
    (input-reset this)
    (update-in this [:subs] #(map tree-reset-inputs %))))

(defn column-state-freqs
  "Returns a map with the frequencies of columns in states `:active`,
  `:predicted`, `:active-predicted`."
  [rgn]
  (let [pred-cids (set (keys (:prev-predictive-cells-by-column rgn)))
        active-cids (:active-columns rgn)
        hit-cids (set/intersection pred-cids active-cids)
        col-states (merge (zipmap pred-cids (repeat :predicted))
                          (zipmap active-cids (repeat :active))
                          (zipmap hit-cids (repeat :active-predicted)))]
    (-> {:active 0, :predicted 0, :active-predicted 0}
        (merge (frequencies (vals col-states)))
        (assoc :timestep (:timestep rgn)
               :ncol (count (:columns rgn))))))
