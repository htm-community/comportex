(ns comportex.js
  (:require
    [org.nfrac.comportex.core :as core]
    [org.nfrac.comportex.encoders :as encoders]
    [org.nfrac.comportex.protocols :as protocols]
  ))

;; Public API

;; Converters from js->clj and clj->js

;; See protocols:
;; PHTM, PRegion, PFeedForward, PFeedBack, PFeedForwardMotor, PLayerOfCells
;; PSynapseGraph, PSegments, PSense, PSelector, PEncoder, PRestartable, PInterruptable
;; PTemporal, PParameterised, PTopological, PTopology

;; Convert JavaScript object or array into ClojureScript data structure. 
;; We can do this by using js->clj function that:
;; "Recursively transforms JavaScript arrays into ClojureScript vectors, 
;; and JavaScript objects into ClojureScript maps. With
;; option ‘:keywordize-keys true’ will convert object fields from
;; strings to keywords.

;;  `spec` is the parameter specification map. 

(defn js->spec
  [spec]
  (js->clj spec :keywordize-keys true))

(defn js->htm
  [htm]
  (js->clj htm))

(defn js->motor
  [motor]
  (js->clj motor))

(defn js->sensory
  [sensory]
  (js->clj sensory))

(defn js->topo
  [topo]
  (js->clj topo))

(defn js->region
  [region]
  (js->clj region))


;; .core

;; ===================================================================

(defn ^:export sensory-region
  "Constructs a cortical region with one layer.

  `spec` is the parameter specification map. See documentation on
  `cells/parameter-defaults` for possible keys. Any keys given here
  will override those default values."

  [spec]
  (clj->js (core/sensory-region (js->spec spec))))


;; ===================================================================

(defn ^:export sensorimotor-region
  "Constructs a cortical region with two layers. `spec` can contain
  nested maps under :layer-3 and :layer-4 that are merged in for
  specific layers.

  This sets `:lateral-synapses? false` in Layer 4, and true in Layer
  3."

  [spec]
  (clj->js (core/sensorimotor-region (js->spec spec))))


;; ===================================================================

(defn ^:export sense-node
  "Creates a sense node with given topology, matching the encoder that
  will generate its bits."

  [topo sensory motor]
  (clj->js (core/sense-node (js->clj [topo sensory motor]))))

;; ===================================================================


(defn ^:export region-network
  "Builds a network of regions and senses from the given dependency
  map. The keywords used in the dependency map are used to look up
  region-building functions, parameter specifications, and sensors in
  the remaining argments.

  Sensors are defined to be the form `[selector encoder]`, satisfying
  protocols PSelector and PEncoder respectively. Sensors in the
  `main-sensors` map can make activating (proximal) connections while
  those in the `motor-sensors` map can make depolarising (distal)
  connections. The same sensor may also be included in both maps.

  For each node, the combined dimensions of its feed-forward sources
  is calculated and used to set the `:input-dimensions` parameter in
  its `spec`. Also, the combined dimensions of feed-forward motor
  inputs are used to set the `:distal-motor-dimensions` parameter, and
  the combined dimensions of its feed-back superior regions is used to
  set the `:distal-topdown-dimensions` parameter. The updated spec is
  passed to a function (typically `sensory-region`) to build a
  region. The build function is found by calling `region-builders`
  with the region id keyword.

  For example to build the network `inp -> v1 -> v2`:

   `
   (region-network
    {:v1 [:input]
     :v2 [:v1]}
    {:v1 sensory-region
     :v2 sensory-region}
    {:v1 spec
     :v2 spec}
    {:input sensor}
    nil)`"
  [ff-deps region-builders region-specs main-sensors motor-sensors]
  (clj->js (core/region-network (js->clj [ff-deps region-builders region-specs main-sensors]motor-sensors))))

;; ===================================================================

(defn ^:export regions-in-series
  "Constructs an HTM network consisting of n regions in a linear
  series. The regions are given keys :rgn-0, :rgn-1, etc. Senses feed
  only to the first region. Their sensors are given in a map with
  keyword keys. Sensors are defined to be the form `[selector encoder]`.

  This is a convenience wrapper around `region-network`."

  [n build-region specs sensors]
  (clj->js (core/regions-in-series (js->clj [n build-region specs sensors]))))

;; ===================================================================

(defn ^:export column-state-freqs
  "Returns a map with the frequencies of columns in states
  `:active` (bursting), `:predicted`, `:active-predicted`. Note that
  these are distinct categories. The names are possibly misleading.
  Argument `layer-fn` is called on the region to obtain a layer of
  cells; if omitted it defaults to the output layer."
  [rgn]
  (clj->js (core/column-state-freqs (js->clj rgn))))

;; ===================================================================

(defn ^:export predicted-bit-votes
  [rgn]
  (clj->js (core/predicted-bit-votes (js->clj rgn))))

;; ===================================================================

(defn cells-proximal-bit-votes
  "For decoding. Given a set of cells in the layer, returns a map from
  incoming bit index to the number of connections to that bit from the
  cells' columns."
  [lyr cells]
  (clj->js (core/cells-proximal-bit-votes (js->clj [lyr cells]))))

;; ===================================================================

(defn ^:export predictions
  [htm sense-id n-predictions]
  (clj->js (core/predictions (js->clj [htm sense-id n-predictions]))))

;; ===================================================================

(defn ^:export cell-excitation-breakdowns
  "Calculates the various sources contributing to total excitation
  level of each of the `cell-ids` in the given layer. Returns a map
  keyed by these cell ids. Each cell value is a map with keys

  * :total - number.
  * :proximal-unstable - a map keyed by source region/sense id.
  * :proximal-stable - a map keyed by source region/sense id.
  * :distal - a map keyed by source region/sense id.
  * :boost - number.
  "

  [htm prior-htm rgn-id lyr-id cell-ids]
  (clj->js (core/cell-excitation-breakdowns (js->clj [htm prior-htm rgn-id lyr-id cell-ids]))))

;; ===================================================================

(defn ^:export update-excitation-breakdown
  "Takes an excitation breakdown such as returned under one key from
  cell-excitation-breakdowns, and updates each numeric component with
  the function f. Key :total will be updated accordingly. The default
  is to scale the values to a total of 1.0. To aggregate breakdowns,
  use `(util/deep-merge-with +)`."

  [breakdown]
  (clj->js (core/update-excitation-breakdown (js->clj breakdown))))

;; protocols
;; mostly just protocols (ie. interfaces)

(defn ^:export htm-step
  [htm inval]
  (clj->js (protocols/htm-step (js->clj [htm inval]))))


;; encoders

(defn ^:export vec-selector
  [& selectors]
  (clj->js (encoders/vec-selector (js->clj selectors))))

(defn ^:export prediction-stats
  [x-bits bit-votes total-votes]
  (clj->js (encoders/prediction-stats (js->clj [x-bits bit-votes total-votes]))))

(defn ^:export decode-by-brute-force
  [e try-values bit-votes]
  (clj->js (encoders/decode-by-brute-force (js->clj [e try-values bit-votes]))))

(defn ^:export encat
  "Returns an encoder for a sequence of values, where each is encoded
  separately before the results are concatenated into a single
  sense. Each value by index is passed to the corresponding index of
  `encoders`."

  [encoders]
  (clj->js (encoders/encat (js->clj encoders))))

(defn ^:export ensplat
  "Returns an encoder for a sequence of values. The given encoder will
  be applied to each value, and the resulting encodings
  overlaid (splatted together), taking the union of the sets of bits."

  [encoders]
  (encoders/ensplat (js->clj encoders)))

(defn ^:export linear-encoder
  [dimensions n-active lower upper]
  (clj->js (encoders/linear-encoder (js->clj [dimensions n-active [lower upper] ]))))

(defn ^:export category-encoder
  [dimensions values]
  (clj->js (encoders/category-encoder (js->clj [dimensions values]))))

(defn ^:export no-encoder
  [dimensions]
  (clj->js (encoders/no-encoder (js->clj dimensions))))

(defn ^:export unique-sdr
  [x n-bits n-active]
  (clj->js (encoders/unique-sdr (js->clj [x n-bits n-active]))))

(defn ^:export unique-encoder
  "This encoder generates a unique bit set for each distinct value,
  based on its hash. `dimensions` is given as a vector."

  [dimensions n-active]
  (encoders/unique-encoder (js->clj [dimensions n-active])))

(defn ^:export linear-2d-encoder
  "Returns a simple encoder for a tuple of two numbers representing a
  position in rectangular bounds. The encoder maps input spatial
  positions to boxes of active bits in corresponding spatial positions
  of the encoded sense. So input positions close in both coordinates
  will have overlapping bit sets.

  * `dimensions` - of the encoded bits, given as a vector [nx ny].

  * `n-active` is the number of bits to be active.

  * `[x-max y-max]` gives the numeric range of input space to
  cover. The numbers will be clamped to this range, and below by
  zero."

  [dimensions n-active x-max y-max]
  (clj->js (encoders/linear-2d-encoder (js->clj [dimensions n-active [x-max y-max]]))))

(defn ^:export coordinate-encoder
  "Coordinate encoder for integer coordinates, unbounded, with one,
  two or three dimensions. Expects a coordinate, i.e. a sequence of
  numbers with 1, 2 or 3 elements. These raw values will be multiplied
  by corresponding `scale-factors` to obtain integer grid
  coordinates. Each dimension has an associated radius within which
  there is some similarity in encoded SDRs."

  [dimensions n-active scale-factors radii]
  (clj->js (encoders/coordinate-encoder (js->clj [dimensions n-active scale-factors radii]))))

(defn ^:export sampling-linear-encoder
  [dimensions n-active lower upper radius]
  (clj->js (encoders/sampling-linear-encoder (js->clj [dimensions n-active [lower upper] radius]))))

(defn ^:export sensor-cat
  [& sensors]
  (clj->js (encoders/sensor-cat (js->clj sensors))))



