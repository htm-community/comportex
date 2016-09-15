(ns org.nfrac.comportex.protocols
  (:require [org.nfrac.comportex.topography :as topo]
            [org.nfrac.comportex.util :refer [spec-finite]]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common specs

(s/def ::bit (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 2048)))))
(s/def ::bits (s/every ::bit :distinct true))
(s/def ::bits-set (s/every ::bit :kind set?))

(s/def ::signal (s/keys :req-un [::bits
                                 ::topo/topography]))

(s/def ::ff-topo (s/and ::topo/topography #(>= (topo/size %) 1)))
(s/def ::fb-topo ::topo/topography)
(s/def ::lat-topo ::topo/topography)

(s/def ::embedding (s/keys :req-un [::ff-topo
                                    ::fb-topo
                                    ::lat-topo]))

(s/def ::column-id (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 2048)))))
(s/def ::cell-index (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 32)))))
(s/def ::cell-id (s/tuple ::column-id ::cell-index))
(s/def ::seg-path (s/tuple ::column-id ::cell-index ::cell-index))
(s/def ::excitation-amt (-> (spec-finite :min 0 :max 1e12)
                            (s/with-gen #(s/gen (s/int-in 0 500)))))
(s/def ::seg-exc (s/every-kv ::seg-path ::excitation-amt))
(s/def ::timestep nat-int?)

(s/def ::permanence (spec-finite :min 0.0 :max 1.0))
(s/def ::segment (s/every-kv ::bit ::permanence))
(s/def ::operation #{:learn :punish :reinforce})
(s/def ::grow-sources (s/nilable ::bits))
(s/def ::die-sources (s/nilable ::bits))
(s/def ::seg-update
  (s/keys :req-un [::target-id
                   ::operation]
          :opt-un [::grow-sources
                   ::die-sources]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util

(defprotocol PTemporal
  (timestep [this]))

(defprotocol PParameterised
  (params [this]
    "A parameter set as map with keyword keys."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Topography

(defprotocol PTopographic
  (topography [this]))

(defn dims-of
  "The dimensions of a topography as an n-tuple vector."
  [x]
  (topo/dimensions (topography x)))

(defn size-of
  "The total number of elements in a topography."
  [x]
  (topo/size (topography x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy

(defprotocol PHTM
  "A network of layers and senses, forming Hierarchical Temporal Memory."
  (htm-sense [this inval mode]
    "Takes an input value. Updates the HTM's senses by applying
    corresponding sensors to the input value. `mode` may be
    :ff or :lat to update only senses with those deps, or nil to update
    all. Also updates :input-value. Returns updated HTM.")
  (htm-activate [this]
    "Propagates feed-forward input through the network to activate
    columns and cells. Assumes senses have already been encoded, with
    `htm-sense`. Increments the time step. Returns updated HTM.")
  (htm-learn [this]
    "Applies learning rules to synapses. Assumes `this` has been through
    the `htm-activate` phase already. Returns updated HTM.")
  (htm-depolarise [this]
    "Propagates lateral and feed-back activity to put cells into a
    depolarised (predictive) state. Assumes `this` has been through
    the `htm-activate` phase already. Returns updated HTM."))

(defn htm-step
  "Advances a HTM by a full time step with the given input value. Just
  (-> htm (htm-sense inval nil) htm-activate htm-learn htm-depolarise)"
  [htm inval]
  (-> htm
      (htm-sense inval nil)
      (htm-activate)
      (htm-learn)
      (htm-depolarise)))

(defprotocol PSignalSource
  "Neural information sources with a bit set representation. Could be
  an encoded sense or a layer (where cells are bits)."
  (signal* [this]))

(defn signal
  [this]
  (signal* this))

(s/fdef signal
        :args (s/cat :this any?)
        :ret ::signal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Layers

(defprotocol PLayer
  (layer-embed* [this embedding])
  (layer-activate* [this ff-signal])
  (layer-learn* [this])
  (layer-depolarise* [this fb-signal lat-signal])
  (layer-state* [this])
  (layer-depth* [this]))

(defmulti layer-spec type)
(s/def ::layer (s/and (s/multi-spec layer-spec :gen-type)
                      #(satisfies? PLayer %)))

(defmulti layer-unembedded-spec type)
(s/def ::layer-unembedded (s/multi-spec layer-unembedded-spec :gen-type))

(defn layer-embed
  "Allows a layer to configure itself to expect the given input topographies
  on feed-forward, feed-back and lateral signals. To be applied before run."
  [this embedding]
  (layer-embed* this embedding))

(s/fdef layer-embed
        :args (s/cat :layer ::layer-unembedded
                     :embedding ::embedding)
        :ret ::layer)

(defn layer-activate
  [this ff-signal]
  (layer-activate* this ff-signal))

(s/def ::layer-activate-args
  #_"Args spec for layer-activate, given an id here to allow generator override."
  (s/and
   (s/cat :layer ::layer
          :ff-signal ::signal)
   (fn [v]
     (let [par (params (:layer v))
           n-in (topo/size (-> par :embedding :ff-topo))]
       (every? #(< % n-in) (:bits (:ff-signal v)))))))

(s/fdef layer-activate
        :args ::layer-activate-args
        :fn (s/and #(= (timestep (:ret %))
                       (inc (timestep (-> % :args :layer)))))
        :ret ::layer)

(defn layer-learn
  [this]
  (layer-learn* this))

(s/fdef layer-learn
        :args (s/cat :layer ::layer)
        :ret ::layer)

(defn layer-depolarise
  [this fb-signal lat-signal]
  (layer-depolarise* this fb-signal lat-signal))

(s/fdef layer-depolarise
        :args (s/cat :layer ::layer
                     :fb-signal ::signal
                     :lat-signal ::signal)
        :ret ::layer)

(defn layer-state
  "The current information content of a layer, including the sets of active and
  predicted cells. This is a generic view to work with different implementations."
  [layer]
  (layer-state* layer))

(s/def ::active-columns (s/coll-of ::column-id :kind set?))
(s/def ::bursting-columns ::active-columns)
(s/def ::active-cells (s/coll-of ::cell-id :kind set?))
(s/def ::winner-cells
  #_"The set of winning cell ids, one in each active column. These are
  only _learning_ cells when they turn on, but are always _learnable_."
  ::active-cells)
(s/def ::predictive-cells
  #_"The set of predictive cell ids derived from the current active
  cells. Can be nil if the depolarise phase has not been applied yet."
  (s/nilable ::active-cells))
(s/def ::prior-predictive-cells
  #_"The set of predictive cell ids from the previous timestep,
  i.e. their prediction can be compared to the current active cells."
  ::active-cells)
(s/def ::layer-state
  (s/keys :req-un [::active-columns
                   ::bursting-columns
                   ::active-cells
                   ::winner-cells
                   ::predictive-cells
                   ::prior-predictive-cells]))

(s/fdef layer-state
        :args (s/cat :layer ::layer)
        :ret ::layer-state)

(defn layer-depth
  "Number of cells per column."
  [this]
  (layer-depth* this))

(s/fdef layer-depth :ret pos-int?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Synapse graphs

(defprotocol PSynapseGraph
  "The synaptic connections from a set of sources to a set of targets.
   Synapses have an associated permanence value between 0 and 1; above
   some permanence level they are defined to be connected."
  (in-synapses [this target-id]
    "All synapses to the target. A map from source ids to permanences.")
  (sources-connected-to [this target-id]
    "The collection of source ids actually connected to target id.")
  (targets-connected-from [this source-id]
    "The collection of target ids actually connected from source id.")
  (excitations [this active-sources stimulus-threshold]
    "Computes a map of target ids to their degree of excitation -- the
    number of sources in `active-sources` they are connected to -- excluding
    any below `stimulus-threshold`.")
  (bulk-learn* [this seg-updates active-sources pinc pdec pinit]
    "Applies learning updates to a batch of targets. `seg-updates` is
    a sequence of SegUpdate records, one for each target dendrite
    segment."))

(s/def ::n-synapse-targets (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 2048)))))
(s/def ::n-synapse-sources (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 2048)))))

(defmulti synapse-graph-spec type)
(s/def ::synapse-graph (s/and (s/multi-spec synapse-graph-spec :gen-type)
                              #(satisfies? PSynapseGraph %)
                              (s/keys :req [::n-synapse-sources
                                            ::n-synapse-targets])))

(defn bulk-learn
  [this seg-updates active-sources pinc pdec pinit]
  (bulk-learn* this seg-updates active-sources pinc pdec pinit))

(defn validate-seg-update
  [upd sg]
  (let [n (::n-synapse-sources sg)
        syns (in-synapses sg (:target-id upd))]
    (s/assert map? syns)
    (when (:grow-sources upd)
      (s/assert (s/every #(< % n)) (:grow-sources upd)))
    (when (:die-sources upd)
      (s/assert (s/every #(< % n)) (:die-sources upd))
      (s/assert (s/every #(contains? syns %)) (:die-sources upd)))
    true))

(s/def ::bulk-learn-args
  #_"Args spec for bulk-learn, given an id here to allow generator override."
  (s/and
   (s/cat :sg ::synapse-graph
          :seg-updates (s/and (s/every ::seg-update)
                              #(->> (map :target-id %) (apply distinct? nil)))
          :active-sources (s/or :set ::bits-set
                                :fn (s/fspec :args (s/cat :bit ::bit)
                                             :ret any?))
          :pinc ::permanence
          :pdec ::permanence
          :pinit ::permanence)
   (fn [v]
     (every? #(validate-seg-update % (:sg v)) (:seg-updates v)))))

(s/fdef bulk-learn
        :args ::bulk-learn-args
        :ret ::synapse-graph)

(defprotocol PSegments
  (cell-segments [this cell-id]
    "A vector of segments on the cell, each being a synapse map."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sensors and Encoders

(defprotocol PSelector
  "Pulls out a value according to some pattern, like a path or lens.
  Should be serializable. A Sensor is defined as [Selector Encoder]."
  (extract [this state]
    "Extracts a value from `state` according to some configured pattern. A
    simple example is a lookup by keyword in a map."))

(s/def ::selector #(satisfies? PSelector %))

(defprotocol PEncoder
  "Encoders need to extend this together with PTopographic."
  (encode* [this x])
  (decode* [this bit-votes n])
  (input-generator [this]))

(defmulti encoder-spec type)
(s/def ::encoder (s/and (s/multi-spec encoder-spec :gen-type)
                        #(satisfies? PEncoder %)))

(defn encode
  "Encodes `x` as a collection of distinct integers which are the on-bits."
  [encoder x]
  (encode* encoder x))

(s/fdef encode
        :args (->
               (s/cat :encoder ::encoder
                      :x any?)
               (s/with-gen
                 #(gen/bind (s/gen ::encoder)
                            (fn [e]
                              (gen/tuple (gen/return e)
                                         (input-generator e))))))
        :ret ::bits
        :fn (fn [v]
              (let [w (-> v :args :encoder size-of)]
                (if (nil? (-> v :args :x))
                  (empty? (:ret v))
                  (and (<= (count (:ret v)) w)
                       (every? #(< % w) (:ret v)))))))

(s/def ::sensor (s/cat :selector ::selector
                       :encoder ::encoder))

(defn decode
  "Finds `n` domain values matching the given bit set in a sequence
  of maps with keys `:value`, `:votes-frac`, `:votes-per-bit`,
  `:bit-coverage`, `:bit-precision`, ordered by votes fraction
  decreasing. The argument `bit-votes` is a map from encoded bit
  index to a number of votes, typically the number of synapse
  connections from predictive cells."
  [encoder bit-votes n]
  (decode* encoder bit-votes n))

(s/def ::votes-frac (s/and number? (complement neg?)))
(s/def ::bit-coverage (s/and number? (complement neg?)))

(s/fdef decode
        :args (s/cat :encoder ::encoder
                     :bit-votes (s/every-kv ::bit nat-int?)
                     :n (s/int-in 0 1000))
        :ret (s/coll-of (s/keys :req-un [::votes-frac
                                         ::bit-coverage])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Etc

(defprotocol PRestartable
  (restart [this]
    "Returns this model (or model component) reverted to its initial
    state prior to any learning."))

(defprotocol PInterruptable
  (break [this mode]
    "Returns this model (or model component) without its current
    sequence state, forcing the following input to be treated as a new
    sequence. `mode` can be

    * :tm, cancels any distal predictions and prevents learning
      lateral/distal connections.
    * :fb, cancels any feedback predictions and prevents learning
      connections on apical dendrites.
    * :syns, cancels any continuing stable synapses used for temporal
      pooling in any higher layers (not `this` layer).
    * :winners, allows new winner cells to be chosen in continuing
      columns."))
