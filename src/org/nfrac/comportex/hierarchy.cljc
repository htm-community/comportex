(ns org.nfrac.comportex.hierarchy
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topography :as topography]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.util.algo-graph :as graph]
            [clojure.set :as set]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

(s/def ::layer-id keyword?)
(s/def ::ff-deps (s/map-of ::layer-id (s/coll-of ::layer-id :kind sequential?)))
(s/def ::fb-deps ::ff-deps)
(s/def ::lat-deps ::ff-deps)
(s/def ::linkages (s/keys :req-un [::ff-deps]
                          :opt-un [::fb-deps
                                   ::lat-deps]))
(s/def ::strata (s/coll-of (s/coll-of ::layer-id :kind set?)))
(s/def ::sensors (s/map-of keyword? ::p/sensor))

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
  p/PTopographic
  (topography [_]
    topography)
  p/PSignalSource
  (signal* [this]
    this))

;;; ## Networks

(defn composite-signal
  [signals]
  (let [topos (map :topography signals)
        widths (map p/size topos)]
    {:bits (util/align-indices widths (map :bits signals))
     :topography (topography/topo-union topos)}))

(s/fdef composite-signal
        :args (s/cat :signals (s/coll-of ::signal))
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
              width (long (p/size (p/topography node)))]
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

(defn source-of-distal-bit
  "Returns [src-id j] where src-id may be a layer key or sense key, and j is
  the index into the output of the source."
  [htm lyr-id i]
  (let [params (p/params (get-in htm [:layers lyr-id]))
        [src-type j] (layer/id->source params i)]
    (case src-type
      :this [lyr-id i]
      :lat (source-of-incoming-bit htm lyr-id j :lat-deps))))

(defn source-of-apical-bit
  "Returns [src-id j] where src-id is a layer key, and j is the index into the
  output of the source layer."
  [htm lyr-id i]
  (source-of-incoming-bit htm lyr-id i :fb-deps))

(do #?(:cljs (def pmap map)))

(defn- htm-sense-impl
  [this inval mode]
  (let [sm (reduce-kv (fn [m k [selector encoder]]
                        (let [bits (->> (p/extract selector inval)
                                        (p/encode encoder))]
                          (assoc m k (->SenseNode (p/topography encoder) bits))))
                      {}
                      (:sensors this))]
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
                                 (p/layer-activate
                                  (get layers id)
                                  (composite-signal (map p/signal ffs))))))
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
                (pmap p/layer-learn)
                (zipmap (keys layers)))]
    (assoc this :layers lm)))

(defn- htm-depolarise-impl
  [this]
  (let [{:keys [fb-deps lat-deps layers senses]} this
        lm (->> layers
                (pmap (fn [[id layer]]
                        (let [fbs (map layers (fb-deps id))
                              lats (map #(or (senses %) (layers %))
                                        (lat-deps id))]
                          (p/layer-depolarise
                           layer
                           (composite-signal (map p/signal fbs))
                           (composite-signal (map p/signal lats))))))
                (zipmap (keys layers)))]
    (assoc this :layers lm)))

(defrecord Network
    [ff-deps fb-deps lat-deps strata layers sensors senses]
  p/PHTM
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

  p/PTemporal
  (timestep [_]
    (p/timestep (first (vals layers))))

  p/PInterruptable
  (break [this mode]
    (assoc this
           :layers (->> (vals layers)
                        (map #(p/break % mode))
                        (zipmap (keys layers)))))

  p/PRestartable
  (restart [this]
    (assoc this
           :layers (->> (vals layers)
                        (pmap p/restart)
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
        are-deps (set (apply concat (vals deps)))]
    (set/difference are-deps have-deps)))

(defn series-deps
  [layer-keys sense-keys]
  (let [ff-deps (zipmap layer-keys (list* sense-keys (map vector layer-keys)))]
    {:ff-deps ff-deps}))

(defn add-feedback
  [{:keys [ff-deps] :as linkages}]
  (let [all-ids (into (set (keys ff-deps))
                      (in-vals-not-keys ff-deps))
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
  passed on to p/layer-embed to allow the layer to configure itself.

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
          (every? (in-vals-not-keys ff-deps) (keys sensors))
          ;; all ids in dependency map must be defined
          (every? layers (keys ff-deps))
          (every? sensors (in-vals-not-keys ff-deps))]}
   (let [all-ids (into (set (keys ff-deps))
                       (in-vals-not-keys ff-deps))
         ff-dag (graph/directed-graph all-ids ff-deps)
         strata (graph/dependency-list ff-dag)
         senses (util/remap (fn [[_ e]]
                              (->SenseNode (p/topography e) ()))
                            sensors)
         elayers (->
                  (reduce-kv (fn [m id layer]
                               (let [ffs (map m (ff-deps id))
                                     fbs (map m (fb-deps id))
                                     lats (map m (lat-deps id))
                                     ff-topo (topo-union (map p/topography ffs))
                                     fb-topo (topo-union (map p/topography fbs))
                                     lat-topo (topo-union (map p/topography lats))
                                     embedding {:ff-topo ff-topo
                                                :fb-topo fb-topo
                                                :lat-topo lat-topo}]
                                 (assoc m id
                                        (p/layer-embed layer embedding))))
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
                     :sensors (s/map-of keyword? ::p/sensor)
                     :linkages ::linkages)
        :ret ::network)

;;; ## Stats

(defn column-state-freqs
  "Returns a map with the frequencies of columns in states
  `:active` (bursting), `:predicted`, `:active-predicted`. Note that
  these are distinct categories. The names are possibly misleading."
  [lyr]
  (let [lstate (p/layer-state lyr)
        a-cols (:active-columns lstate)
        ppc (:prior-predictive-cells lstate)
        pp-cols (into #{} (map first ppc))
        hit-cols (set/intersection pp-cols a-cols)
        col-states (merge (zipmap pp-cols (repeat :predicted))
                          (zipmap a-cols (repeat :active))
                          (zipmap hit-cols (repeat :active-predicted)))]
    (-> {:active 0, :predicted 0, :active-predicted 0}
        (merge (frequencies (vals col-states)))
        (assoc :timestep (p/timestep lyr)
               :size (p/size (p/topography lyr))))))

(s/fdef column-state-freqs
        :args (s/cat :lyr ::p/layer-of-cells)
        :ret (s/keys :req-un [::active
                              ::predicted
                              ::active-predicted
                              ::p/timestep]))

;;; ## Tracing columns back to senses

(defn segs-proximal-bit-votes
  [lyr seg-paths]
  (let [psg (:proximal-sg lyr)]
    (->> seg-paths
         (reduce (fn [m seg-path]
                   (let [ids (p/sources-connected-to psg seg-path)]
                     (reduce (fn [m id]
                               (assoc! m id (inc (get m id 0))))
                             m ids)))
                 (transient {}))
         (persistent!))))

(defn cells-proximal-bit-votes
  "For decoding. Given a set of cells in the layer, returns a map from
  incoming bit index to the number of connections to that bit from the
  cells' columns."
  [lyr cells]
  (segs-proximal-bit-votes lyr (map #(conj % 0) cells)))

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
         (map p/topography)
         (map p/size)
         (reduce + 0))))

(defn predictions
  ([htm sense-id n-predictions]
   (predictions
    htm sense-id n-predictions (comp :predictive-cells p/layer-state)))
  ([htm sense-id n-predictions cells-fn]
   (let [sense-width (p/size-of (get-in htm [:senses sense-id]))
         {:keys [fb-deps]} (add-feedback (select-keys htm [:ff-deps]))
         pr-votes (->> (fb-deps sense-id)
                       (mapcat (fn [lyr-id]
                                 (let [lyr (get-in htm [:layers lyr-id])
                                       start (ff-base htm lyr-id sense-id)
                                       end (+ start sense-width)
                                       cells (cells-fn lyr)]
                                   (->> (cells-proximal-bit-votes lyr cells)
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
     (p/decode encoder pr-votes n-predictions))))

(defn- zap-fewer
  [n xs]
  (if (< (count xs) n) (empty xs) xs))

(defn cell-excitation-breakdowns
  "Calculates the various sources contributing to total excitation
  level of each of the `cell-ids` in the given layer. Returns a map
  keyed by these cell ids. Each cell value is a map with keys

  * :total - number.
  * :proximal-unstable - a map keyed by source layer/sense id.
  * :proximal-stable - a map keyed by source layer/sense id.
  * :distal - a map keyed by source layer/sense id.
  * :boost - number.
  "
  [htm prior-htm lyr-id cell-ids]
  (let [lyr (get-in htm [:layers lyr-id])
        prior-lyr (get-in prior-htm [:layers lyr-id])
        params (:params lyr)
        ff-stim-thresh (:stimulus-threshold (:proximal params))
        d-stim-thresh (:stimulus-threshold (:distal params))
        a-stim-thresh (:stimulus-threshold (:apical params))
        distal-weight (:distal-vs-proximal-weight params)
        active-state (:active-state lyr)
        prior-active-state (:active-state prior-lyr)
        distal-state (:distal-state prior-lyr)
        apical-state (:apical-state prior-lyr)
        ;; inputs to layer
        ff-bits (:in-ff-bits active-state)
        ff-s-bits (:in-stable-ff-bits active-state)
        ff-b-bits (set/difference ff-bits ff-s-bits)
        distal-bits (:active-bits distal-state)
        apical-bits (:active-bits apical-state)
        ff-bits-srcs (into {}
                           (map (fn [i]
                                  (let [[k _] (source-of-incoming-bit
                                               htm lyr-id i :ff-bits)]
                                    [i k])))
                           ff-bits)
        distal-bits-srcs (into {}
                               (map (fn [i]
                                      (let [[k _] (source-of-distal-bit
                                                   htm lyr-id i)]
                                        [i k])))
                               distal-bits)
        apical-bits-srcs (into {}
                               (map (fn [i]
                                      (let [[k _] (source-of-apical-bit
                                                   htm lyr-id i)]
                                        [i k])))
                               apical-bits)
        ;; synapse graphs - pre-learning state so from prior time step
        psg (:proximal-sg prior-lyr)
        dsg (:distal-sg prior-lyr)
        asg (:apical-sg prior-lyr)
        ;; internal sources
        boosts (:boosts prior-lyr)]
    (into {}
          (map (fn [cell-id]
                 (let [[col ci] cell-id
                       ;; breakdown of proximal excitation by source
                       [ff-seg-path _] (get (:fully-matching-ff-segs active-state) [col 0])
                       ff-conn-sources (when ff-seg-path
                                         (p/sources-connected-to psg ff-seg-path))
                       active-ff-b (->> (filter ff-b-bits ff-conn-sources)
                                        (zap-fewer ff-stim-thresh))
                       active-ff-s (->> (filter ff-s-bits ff-conn-sources)
                                        (zap-fewer ff-stim-thresh))
                       ff-b-by-src (frequencies (map ff-bits-srcs active-ff-b))
                       ff-s-by-src (frequencies (map ff-bits-srcs active-ff-s))
                       ;; breakdown of distal excitation by source
                       [d-seg-path _] (get (:fully-matching-segs distal-state) cell-id)
                       d-conn-sources (when d-seg-path
                                        (p/sources-connected-to dsg d-seg-path))
                       active-d (->> (filter distal-bits d-conn-sources)
                                     (zap-fewer d-stim-thresh))
                       d-by-src (->> (frequencies (map distal-bits-srcs active-d))
                                     (util/remap #(* % distal-weight)))
                       ;; same for apical
                       [a-seg-path _] (get (:fully-matching-segs apical-state) cell-id)
                       a-conn-sources (when a-seg-path
                                        (p/sources-connected-to asg a-seg-path))
                       active-a (->> (filter apical-bits a-conn-sources)
                                     (zap-fewer a-stim-thresh))
                       a-by-src (->> (frequencies (map apical-bits-srcs active-a))
                                     (util/remap #(* % distal-weight)))
                       ;; excitation levels
                       b-overlap (count active-ff-b)
                       s-overlap (count active-ff-s)
                       d-a-exc (->> (+ (count active-d) (count active-a))
                                    (* distal-weight))
                       ;; effect of boosting
                       overlap (+ b-overlap s-overlap)
                       boost-amt (* overlap (- (get boosts col) 1.0))
                       ;; total excitation
                       total (+ b-overlap s-overlap boost-amt d-a-exc)]
                   [cell-id {:total total
                             :proximal-unstable ff-b-by-src
                             :proximal-stable ff-s-by-src
                             :boost boost-amt
                             :distal (merge d-by-src a-by-src)}])))
          cell-ids)))

(defn update-excitation-breakdown
  "Takes an excitation breakdown such as returned under one key from
  cell-excitation-breakdowns, and updates each numeric component with
  the function f. Key :total will be updated accordingly. The default
  is to scale the values to a total of 1.0. To aggregate breakdowns,
  use `(util/deep-merge-with +)`."
  ([breakdown]
   (let [total (:total breakdown)]
     (update-excitation-breakdown breakdown #(/ % total))))
  ([breakdown f]
   (persistent!
    (reduce-kv (fn [m k v]
                 (let [new-v (if (map? v)
                               (util/remap f v)
                               (f v))
                       v-total (if (map? v)
                                 (reduce + (vals new-v))
                                 new-v)]
                   (assoc! m k new-v
                           :total (+ (get m :total) v-total))))
               (transient {:total 0.0})
               (dissoc breakdown :total)))))
