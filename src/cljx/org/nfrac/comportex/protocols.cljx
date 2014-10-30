(ns org.nfrac.comportex.protocols)

(defprotocol PHTM
  (htm-activate [this])
  (htm-learn [this])
  (htm-depolarise [this])
  (region-seq [this])
  (input-seq [this])
  (update-by-uuid [this region-uuid f]
    "Applies function `f` to the region in a HTM network identified by
     its UUID. Returns the modified HTM network."))

(defn htm-step
  [this]
  (-> this
      (htm-activate)
      (htm-learn)
      (htm-depolarise)))

(defprotocol PRegion
  "Cortical regions need to extend this together with PTopological,
   PFeedForward, PTemporal, PParameterised."
  (region-activate [this ff-bits signal-ff-bits])
  (region-learn [this ff-bits])
  (region-depolarise [this distal-ff-bits distal-fb-bits]))

(defn region-step
  ([this ff-bits]
     (region-step this ff-bits #{} #{} #{}))
  ([this ff-bits signal-ff-bits distal-ff-bits distal-fb-bits]
     (-> this
         (region-activate ff-bits signal-ff-bits)
         (region-learn ff-bits)
         (region-depolarise distal-ff-bits distal-fb-bits))))

(defprotocol PFeedForward
  "A feed-forward input source with a bit set representation. Could be
   sensory input or a region (where cells are bits)."
  (ff-topology [this])
  (bits-value [this offset])
  (signal-bits-value [this offset])
  (source-of-bit [this i]))

(defprotocol PFeedForwardMotor
  (ff-motor-topology [this])
  (motor-bits-value [this offset]))

(defprotocol PColumnField
  (columns-step [this ff-bits signal-ff-bits])
  (columns-learn [this ff-bits a-cols])
  (inhibition-radius [this])
  (column-excitation [this])
  (column-overlaps [this])
  (column-signal-overlaps [this]))

(defprotocol PLayerOfCells
  (layer-activate [this prox-exc prox-sig-exc inh-radius])
  (layer-learn [this])
  (layer-depolarise [this distal-ff-bits distal-fb-bits])
  (layer-depth [this])
  (bursting-columns [this])
  (active-columns [this])
  (active-cells [this])
  (learnable-cells [this])
  (signal-cells [this])
  (temporal-pooling-cells [this])
  (predictive-cells [this])
  (prior-predictive-cells [this])
  (depolarisation [this]))

(defprotocol PSynapseGraph
  "The synaptic connections from a set of sources (as integer ids) to
   a set of targets (as integer ids). Synapses have an associated
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

(defprotocol PEncodable
  "Encoders need to extend this together with PTopological."
  (encode [this offset x]
    "Encodes `x` as a set of integers which are the on-bits, starting
     from the given offset.")
  (decode [this bit-votes n]
    "Finds `n` domain values matching the given bit set in a sequence
     of maps with keys `:value`, `:votes-frac`, `:votes-per-bit`,
     `:bit-coverage`, `:bit-precision`, ordered by votes fraction
     decreasing. The argument `bit-votes` is a map from encoded bit
     index to a number of votes, typically the number of synapse
     connections from predictive cells."))

(defprotocol PSensoryInput
  "Inputs need to extend this together with PFeedForward."
  (input-step [this])
  (domain-value [this] "Value underlying the current sensory input."))

(defprotocol PResettable
  (reset [this]))

(defprotocol PTemporal
  (timestep [this]))

(defprotocol PParameterised
  (params [this]
    "A parameter set as map with keyword keys."))

(defprotocol PTopological
  (topology [this]))

(defprotocol PTopology
  "Operating on a regular grid of certain dimensions, where each
   coordinate is an n-tuple vector---or integer for 1D---and also has
   a unique integer index."
  (dimensions [this])
  (coordinates-of-index [this idx])
  (index-of-coordinates [this coord])
  (neighbours* [this coord outer-r inner-r])
  (coord-distance [this coord-a coord-b]))

(defn size
  "The total number of elements indexed in the topology."
  [topo]
  (apply * (dimensions topo)))

(defn dims-of
  "The dimensions of a PTopological as an n-tuple vector."
  [x]
  (dimensions (topology x)))

(defn size-of
  "The total number of elements in a PTopological."
  [x]
  (size (topology x)))

(defn neighbours
  "Returns the coordinates away from `coord` at distances
  `inner-r` (exclusive) out to `outer-r` (inclusive) ."
  ([topo coord radius]
     (neighbours* topo coord radius 0))
  ([topo coord outer-r inner-r]
     (neighbours* topo coord outer-r inner-r)))

(defn neighbours-indices
  "Same as `neighbours` but taking and returning indices instead of
   coordinates."
  ([topo idx radius]
     (neighbours-indices topo idx radius 0))
  ([topo idx outer-r inner-r]
     (->> (neighbours* topo (coordinates-of-index topo idx)
                       outer-r inner-r)
          (map (partial index-of-coordinates topo)))))
