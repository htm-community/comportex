(ns org.nfrac.comportex.protocols)

(defprotocol PTopological
  (topology [this]))

(defprotocol PTopology
  "Operating on a regular grid of certain dimensions, where each
   coordinate is a vector and also has a unique integer index."
  (dimensions [this])
  (coordinates-of-index [this idx])
  (index-of-coordinates [this coord])
  (neighbours* [this coord outer-r inner-r])
  (coord-distance [this coord-a coord-b]))

(defn dims-of
  [x]
  (dimensions (topology x)))

(defn size
  "The total number of elements indexed in a topology."
  [topo]
  (apply * (dimensions topo)))

(defn size-of
  [x]
  (size (topology x)))

(defn neighbours
  "Returns the coordinates within `outer-r`adius in of the given
   `coord`, but excluding any within `inner-r`adius."
  ([topo coord radius]
     (neighbours* topo coord radius 1)) ;; CHECK 0 or 1
  ([topo coord outer-r inner-r]
     (neighbours* topo coord outer-r inner-r)))

(defn neighbours-indices
  "Same as `neighbours` but taking and returning indices instead of
   coordinates."
  ([topo idx radius]
     (neighbours-indices topo idx radius 1))
  ([topo idx outer-r inner-r]
     (->> (neighbours* topo (coordinates-of-index topo idx)
                       outer-r inner-r)
          (map (partial index-of-coordinates topo)))))

(defprotocol PEncodable
  (encode [this offset x]))

(defprotocol PParameterised
  (params [this]))

(defprotocol PRegion
  (region-step* [this ff-bits signal-ff-bits extra-distal learn?])
  (ff-cells-per-column [this])
  (ff-active-cells [this])
  (ff-signal-cells [this])
  (ff-motor-cells [this]) ;; not yet implemented
  (fb-active-cells [this])) ;; not yet implemented

(defn region-step
  ([this ff-bits signal-ff-bits]
     (region-step* this ff-bits signal-ff-bits #{} true))
  ([this ff-bits signal-ff-bits extra-distal learn?]
     (region-step* this ff-bits signal-ff-bits extra-distal learn?)))

(defprotocol PColumnField
  (columns-step [this ff-bits signal-ff-bits cell-depolarisation learn?])
  (active-columns [this])
  (temporal-pooling-columns [this])
  (column-overlaps [this]))

(defprotocol PLayerOfCells
  (layer-step [this a-cols tp-cols extra-distal learn?])
  (layer-depth [this])
  (bursting-columns [this])
  (active-cells [this])
  (learnable-cells [this])
  (signal-cells [this])
  (temporal-pooling-cells [this])
  (predictive-cells [this])
  (prior-predictive-cells [this])
  (depolarisation [this]))

(defprotocol PSynapseGraph
  "The synaptic connections from a set of sources (as integer ids) to
   a set of targets (by integer ids). Synapses have an associated
   permanence value between 0 and 1; above some permanence level they
   are defined to be connected."
  (in-synapses [this target-id]
    "Synapses connecting to the target. A map from source ids to permanences.")
  (sources-connected-to [this target-id])
  (targets-connected-from [this source-id])
  (reinforce-in-synapses [this target-id skip? reinforce? pinc pdec])
  (conj-synapses [this target-id syn-source-ids p])
  (disj-synapses [this target-id syn-source-ids]))

(defprotocol PSegments
  (cell-segments [this cell-id]
    "A vector of segments on the cell, each being a synapse map."))

(defprotocol PFeedForward
  "A feedforward input source with a bit set representation. Could be
   sensory input, or a collection of regions feeding forward to a
   higher-level region."
  (bits-value* [this offset])
  (signal-bits-value* [this offset])
  (source-of-bit [this i])
  (incoming-bits-value [this])
  (source-of-incoming-bit [this i])
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

(defprotocol PTemporal
  (timestep [this]))

(defprotocol PResettable
  (reset [this]))

(defprotocol PSensoryInput
  "A sensory input stream having some domain-specific underlying
   representation as well as possibly a set of domain-specific labels
   that may be useful for classification training or diagnosis."
  (domain-value [this] "Value underlying the current sensory input.")
  (state-labels [this] "A set of values, typically keywords,
    associated with the current sensory input."))
