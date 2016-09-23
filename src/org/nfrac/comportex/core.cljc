(ns org.nfrac.comportex.core
  "This namespace presents a largely abstract API which can support different
  implementations of layers. It does not know about HTM layer implementation or
  synapse graphs."
  (:require [org.nfrac.comportex.topography :as topo]
            [org.nfrac.comportex.util :as util :refer [spec-finite]]
            [org.nfrac.comportex.util.algo-graph :as graph]
            [clojure.set :as set]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common specs

(s/def ::bit (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 2048)))))
(s/def ::bits (s/every ::bit :distinct true))
(s/def ::bits-set (s/every ::bit :kind set?))

(s/def ::signal (s/and (s/map-of keyword? ::bits)
                       #(contains? % :bits)))

(s/def ::ff-topo (s/and ::topo/topography #(>= (topo/size %) 1)))
(s/def ::fb-topo ::topo/topography)
(s/def ::lat-topo ::topo/topography)

(s/def ::embedding (s/keys :req-un [::ff-topo
                                    ::fb-topo
                                    ::lat-topo]))

(s/def ::timestep nat-int?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util

(defprotocol PTemporal
  (timestep [this]))

(defprotocol PParameterised
  (params [this]
    "A parameter set as map with keyword keys."))

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
  (layer-decode-to-ff-bits* [this opts]))

(defmulti layer-spec type)
(s/def ::layer (s/and (s/multi-spec layer-spec :gen-type)
                      #(satisfies? PLayer %)
                      #(contains? % :embedding)))

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
     (let [n-in (-> v :layer :embedding :ff-topo topo/size)]
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


(defn layer-decode-to-ff-bits
  "Converts the current predictive state of layer (by default) into a distribution
  of feed-forward input bits that matches it."
  [layer opts]
  (layer-decode-to-ff-bits* layer opts))

(s/fdef layer-decode-to-ff-bits
        :args (s/cat :layer ::layer
                     :opts (s/keys))
        :ret (s/every-kv ::bit (spec-finite :min 0)))

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
                     :bit-votes (s/every-kv ::bit (spec-finite :min 0))
                     :n (s/int-in 0 1000))
        :ret (s/coll-of (s/keys :req-un [::votes-frac
                                         ::bit-coverage])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc protocols

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy / network implementation

(s/def ::layer-id keyword?)
(s/def ::ff-deps (s/map-of ::layer-id (s/coll-of ::layer-id :kind sequential?)))
(s/def ::fb-deps ::ff-deps)
(s/def ::lat-deps ::ff-deps)
(s/def ::linkages (s/keys :req-un [::ff-deps]
                          :opt-un [::fb-deps
                                   ::lat-deps]))
(s/def ::strata (s/coll-of (s/coll-of ::layer-id :kind set?)))

(s/def ::sensors (s/map-of keyword? ::sensor))

(s/def ::network
  (s/keys :req-un [::ff-deps
                   ::fb-deps
                   ::lat-deps
                   ::strata
                   ::layers
                   ::sensors
                   ::senses]))

(defrecord SenseNode
  [topography bits]
  PTopographic
  (topography [_]
    topography)
  PSignalSource
  (signal* [this]
    {:bits bits}))

(defn composite-signal
  [ffs]
  (let [signals (map signal ffs)
        widths (map size-of ffs)
        sigkeys (distinct (mapcat keys signals))]
    (reduce (fn [m k]
              (assoc m k (util/align-indices widths (map k signals))))
            {:bits ()}
            sigkeys)))

(s/fdef composite-signal
        :args (s/cat :ffs (s/coll-of #(s/valid? ::signal (signal %))))
        :ret ::signal)

(defn source-of-incoming-bit
  "Taking the index of an input bit as received by the given layer, return its
  source element as [src-id j] where src-id is the key of the source layer or
  sense, and j is the index adjusted to refer to the output of that source.

  `field` can be :ff-deps, :fb-deps or :lat-deps depending on which input field
  `i` is an index into."
  [htm lyr-id i field]
  (let [senses (:senses htm)
        layers (:layers htm)
        src-ids (get-in htm [field lyr-id])]
    (loop [src-ids src-ids
           offset 0]
      (when-let [src-id (first src-ids)]
        (let [node (or (senses src-id)
                       (layers src-id))
              width (long (size-of node))]
          (if (< i (+ offset width))
            [src-id (- i offset)]
            (recur (next src-ids)
                   (+ offset width))))))))

(s/fdef source-of-incoming-bit
        :args (s/cat :htm ::network
                     :lyr-id ::layer-id
                     :i nat-int?
                     :field #{:ff-deps :fb-deps :lat-deps})
        :ret (s/cat :src-id keyword?
                    :index nat-int?))

(do #?(:cljs (def pmap map)))

(defn- htm-sense-impl
  [this inval mode]
  (let [{:keys [ff-deps lat-deps sensors senses]} this
        is-ff-dep? (into #{} cat (vals ff-deps))
        is-lat-dep? (into #{} cat (vals lat-deps))
        sm (reduce-kv (fn [m k [selector encoder]]
                        (if (case mode
                              :ff (is-ff-dep? k)
                              :lat (is-lat-dep? k)
                              nil true)
                          (let [bits (->> (extract selector inval)
                                          (encode encoder))]
                            (assoc m k (->SenseNode (topography encoder) bits)))
                          m))
                      senses
                      sensors)]
    (assoc this
           :senses sm
           :input-value inval)))

(defn- htm-activate-impl
  [this]
  (let [{:keys [strata ff-deps layers senses]} this
        lm (-> (reduce
                (fn [m stratum]
                  (->> stratum
                       (pmap (fn [id]
                               (let [ffs (map m (ff-deps id))]
                                 (layer-activate (get layers id)
                                                 (composite-signal ffs)))))
                       (zipmap stratum)
                       (into m)))
                senses
                ;; drop 1st stratum i.e. drop the sensory inputs
                (rest strata))
               ;; get rid of the sense nodes which were seeded into the reduce
               (select-keys (keys layers)))]
    (assoc this :layers lm)))

(defn- htm-learn-impl
  [this]
  (let [layers (:layers this)
        lm (->> (vals layers)
                (pmap layer-learn)
                (zipmap (keys layers)))]
    (assoc this :layers lm)))

(defn- htm-depolarise-impl
  [this]
  (let [{:keys [fb-deps lat-deps layers senses]} this
        lm (->> layers
                (pmap (fn [[id layer]]
                        (let [fbs (map layers (get fb-deps id))
                              lats (map #(or (senses %) (layers %))
                                        (get lat-deps id))]
                          (layer-depolarise layer
                                            (composite-signal fbs)
                                            (composite-signal lats)))))
                (zipmap (keys layers)))]
    (assoc this :layers lm)))

(defrecord Network
    [ff-deps fb-deps lat-deps strata layers sensors senses]
  PHTM
  (htm-sense
    [this inval mode]
    (htm-sense-impl this inval mode))

  (htm-activate
    [this]
    (htm-activate-impl this))

  (htm-learn
    [this]
    (htm-learn-impl this))

  (htm-depolarise
    [this]
    (htm-depolarise-impl this))

  PTemporal
  (timestep [_]
    (timestep (first (vals layers))))

  PInterruptable
  (break [this mode]
    (assoc this
           :layers (->> (vals layers)
                        (map #(break % mode))
                        (zipmap (keys layers)))))

  PRestartable
  (restart [this]
    (assoc this
           :layers (->> (vals layers)
                        (pmap restart)
                        (zipmap (keys layers))))))

(defn layer-keys
  "A sequence of the keys of all layers in topologically-sorted
  order. If `n-levels` is provided, only the layers from that many
  hierarchical levels are included. So 1 gives the first tier directly
  receiving sensory inputs."
  ([htm]
   (layer-keys htm (dec (count (:strata htm)))))
  ([htm n-levels]
   ;; topologically sorted: drop 1st stratum i.e. drop the sensory inputs
   (apply concat (take n-levels (rest (:strata htm))))))

(defn sense-keys
  "A sequence of the keys of all sense nodes."
  [htm]
  (keys (:sensors htm)))

(defn layer-seq
  [htm]
  (map (:layers htm) (layer-keys htm)))

(defn- in-vals-not-keys
  [deps]
  (let [have-deps (set (keys deps))
        are-deps (into #{} cat (vals deps))]
    (set/difference are-deps have-deps)))

(defn series-deps
  [layer-keys sense-keys]
  (let [ff-deps (zipmap layer-keys (list* sense-keys (map vector layer-keys)))]
    {:ff-deps ff-deps}))

(defn add-feedback-deps
  [{:keys [ff-deps] :as linkages}]
  (let [all-ids (into (set (keys ff-deps)) cat (vals ff-deps))
        ff-dag (graph/directed-graph all-ids ff-deps)
        fb-deps (->> (graph/reverse-graph ff-dag)
                     :neighbors
                     (util/remap seq))]
    (assoc linkages :fb-deps fb-deps)))

(defn network
  "Builds a network of layers and senses with the given linkages.
  Linkages between these nodes are given as direct dependencies:
  :ff-deps maps each layer to a list of nodes it takes feed-forward
  input from. Optionally, :fb-deps maps layers to lists of nodes to
  take feed-back input from. And :lat-deps lateral sources, which
  may also be senses (like motor senses).

  Sensors are defined to be the form `[selector encoder]`, satisfying
  protocols PSelector and PEncoder respectively.

  For each layer, the combined dimensions of each of its feed-forward
  sources, feed-back sources, and lateral sources are calculated and
  passed on to layer-embed to allow the layer to configure itself.

  For example a feed-forward network `inp -> v1 -> v2`:

   (network
    {:v1 v1-layer
     :v2 v2-layer}
    {:inp [sel enc]}
    {:ff-deps {:v1 [:inp]
               :v2 [:v1]}})"
  ([layers sensors]
   (assert (= 1 (count layers)) "linkages can be omitted only for single layer.")
   (let [ff-deps (assoc {} (first (keys layers)) (keys sensors))]
     (network layers sensors {:ff-deps ff-deps})))
  ([layers sensors {:keys [ff-deps fb-deps lat-deps] :as linkages}]
   {:pre [;; all layers must have dependencies
          (every? ff-deps (keys layers))
          ;; all sense nodes must not have dependencies
          (every? (in-vals-not-keys (merge-with concat ff-deps lat-deps))
                  (keys sensors))
          ;; all ids in dependency map must be defined
          (every? layers (keys ff-deps))
          (every? sensors (in-vals-not-keys (merge-with concat ff-deps lat-deps)))]}
   (let [all-ids (into (set (keys ff-deps)) cat (vals ff-deps))
         ff-dag (graph/directed-graph all-ids ff-deps)
         strata (graph/dependency-list ff-dag)
         senses (util/remap (fn [[_ e]]
                              (->SenseNode (topography e) ()))
                            sensors)
         elayers (->
                  (reduce-kv (fn [m id layer]
                               (let [ffs (map m (get ff-deps id))
                                     fbs (map m (get fb-deps id))
                                     lats (map m (get lat-deps id))
                                     ff-topo (topo/topo-union (map topography ffs))
                                     fb-topo (topo/topo-union (map topography fbs))
                                     lat-topo (topo/topo-union (map topography lats))
                                     embedding {:ff-topo ff-topo
                                                :fb-topo fb-topo
                                                :lat-topo lat-topo}]
                                 (assoc m id
                                        (layer-embed layer embedding))))
                             (merge senses layers)
                             layers)
                  ;; get rid of the sense nodes which were seeded into the reduce
                  (select-keys (keys layers)))]
     (map->Network
      {:ff-deps ff-deps
       :fb-deps fb-deps
       :lat-deps lat-deps
       :strata strata
       :sensors sensors
       :senses senses
       :layers elayers}))))

(s/fdef network
        :args (s/cat :layers (s/map-of ::layer-id ::layer-unembedded)
                     :sensors (s/map-of keyword? ::sensor)
                     :linkages (s/? ::linkages))
        :ret ::network)

(defn ff-base
  "Returns the first index that corresponds with `ff-id` within the
  feedforward input to `lyr-id`."
  [htm lyr-id ff-id]
  (let [{:keys [senses layers]} htm]
    (->> (get-in htm [:ff-deps lyr-id])
         (map (fn [id]
                [id
                 (or (senses id)
                     (layers id))]))
         (take-while (fn [[id _]]
                       (not= id ff-id)))
         (map (fn [[id ff]]
                ff))
         (map size-of)
         (reduce + 0))))

(defn predictions
  ([htm sense-id n-predictions]
   (predictions htm sense-id n-predictions {}))
  ([htm sense-id n-predictions opts]
   (let [sense-width (size-of (get-in htm [:senses sense-id]))
         {:keys [fb-deps]} (add-feedback-deps htm)
         pr-votes (->> (fb-deps sense-id)
                       (mapcat (fn [lyr-id]
                                 (let [lyr (get-in htm [:layers lyr-id])
                                       start (ff-base htm lyr-id sense-id)
                                       end (+ start sense-width)]
                                   (->> (layer-decode-to-ff-bits lyr opts)
                                        (keep (fn [[id votes]]
                                                (when (and (<= start id)
                                                           (< id end))
                                                  [(- id start) votes])))))))
                       (reduce (fn [m [id votes]]
                                 (assoc m id
                                        (+ (get m id 0)
                                           votes)))
                               {}))
         [_ encoder] (get-in htm [:sensors sense-id])]
     (decode encoder pr-votes n-predictions))))
