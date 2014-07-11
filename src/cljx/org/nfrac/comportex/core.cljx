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
            [clojure.set :as set]))

(defn cla-region
  [spec]
  (-> (p/region spec)
      (sm/with-sequence-memory)))

(defn cla-step
  [rgn in-bits learn?]
  (let [r-sp (p/pooling-step rgn in-bits learn?)]
    (sm/sequence-memory-step r-sp (:active-columns r-sp) learn?)))

(defprotocol PInputGenerator
  "Maintains an input stream and its encoding into bit sets."
  (bit-width [this])
  (bits-value [this])
  (domain-value [this])
  (input-step [this])
  (input-reset [this]))

(defrecord InputGenerator [init-value value transform encode options]
  PInputGenerator
  (bit-width [_] (:bit-width options))
  (bits-value [_] (encode value))
  (domain-value [_] value)
  (input-step [this] (assoc this :value (transform value)))
  (input-reset [this] (assoc this :value init-value)))

(defn generator
  "Creates an input stream generator from an initial value, a function
   to transform the input to the next time step, an encoder function to
   return a set of bit indices, and an options map that should include
   the total `:bit-width`."
  [init-value transform encode options]
  (->InputGenerator init-value init-value transform encode options))

(defn cla-model
  "A CLA model encapsulates a CLA region created according to the
   parameter map `spec`, with an input stream generator."
  [ingen spec]
  (let [r (cla-region spec)]
    {:region r
     :in ingen}))

(defn step
  "Advances a CLA model by transforming the input value, and updating
   the region with the new input. Learning is on unless specifically
   passed as false."
  ([model]
     (step model true))
  ([model learn?]
     (let [new-in (input-step (:in model))]
       (-> model
           (assoc :in new-in)
           (update-in [:region] cla-step (bits-value new-in) learn?)))))

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
