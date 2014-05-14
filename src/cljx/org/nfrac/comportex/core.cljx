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
     are needed as in `sm/sequence-memory-defaults`.

   * (others updated by the orchestration functions:)
     * `:timestep` the simulation time step, an iteration number.
     * `:active-columns` a set of the currently active column ids.
     * `:active-cells` a set of the currently active cell ids.
     * `:busting-columns` a set of the currently _bursting_ column ids.

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
            [org.nfrac.comportex.sequence-memory :as sm]))


