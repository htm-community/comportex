(ns org.nfrac.comportex.core
  "A _region_ is the main composable unit in this library. It
   represents a bank of neurons arranged in columns, responding to an
   array of feed-forward input bits, as well as distal connections to
   itself and possibly other regions."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.util.algo-graph :as graph]
            [clojure.set :as set]))

(defn layers
  "A sequence of keywords looking up layers in the region. The first
  is the input layer, the last is the (feed-forward) output layer."
  [rgn]
  (concat (when (:layer-4 rgn) [:layer-4])
          (when (:layer-3 rgn) [:layer-3])))

;;; # Sensory = L3

(declare sensory-region)

(defrecord SensoryRegion
    [layer-3]
  p/PRegion
  (region-activate
    [this ff-bits stable-ff-bits]
    (assoc this
      :layer-3 (p/layer-activate layer-3 ff-bits stable-ff-bits)))

  (region-learn
    [this]
    (if (:freeze? (p/params this))
      this
      (assoc this
        :layer-3 (p/layer-learn layer-3))))

  (region-depolarise
    [this distal-ff-bits apical-fb-bits apical-fb-wc-bits]
    (assoc this
        :layer-3 (p/layer-depolarise layer-3 distal-ff-bits apical-fb-bits apical-fb-wc-bits)))

  p/PTopological
  (topology [_]
    (p/topology layer-3))
  p/PFeedForward
  (ff-topology [_]
    (p/ff-topology layer-3))
  (bits-value [_]
    (p/bits-value layer-3))
  (stable-bits-value [_]
    (p/stable-bits-value layer-3))
  (source-of-bit [_ i]
    (p/source-of-bit layer-3 i))
  p/PFeedBack
  (wc-bits-value [_]
    (p/wc-bits-value layer-3))
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    topology/empty-topology)
  (motor-bits-value
    [_]
    (sequence nil))
  p/PTemporal
  (timestep [_]
    (p/timestep layer-3))
  p/PParameterised
  (params [_]
    (p/params layer-3))
  p/PInterruptable
  (break [this mode]
    (assoc this
           :layer-3 (p/break layer-3 mode)))
  p/PRestartable
  (restart [this]
    (sensory-region (p/params this))))

(defn sensory-region
  "Constructs a cortical region with one layer.

  `spec` is the parameter specification map. See documentation on
  `cells/parameter-defaults` for possible keys. Any keys given here
  will override those default values."
  [spec]
  (let [unk (set/difference (set (keys spec))
                            (set (keys cells/parameter-defaults)))]
    (when (seq unk)
      (println "Warning: unknown keys in spec:" unk)))
  (map->SensoryRegion
   {:layer-3 (cells/layer-of-cells spec)}))

;;; # Sensorimotor = L3 + L4

(declare sensorimotor-region)

(defrecord SensoriMotorRegion
    [layer-4 layer-3]
  p/PRegion
  (region-activate
    [this ff-bits stable-ff-bits]
    (let [l4 (p/layer-activate layer-4 ff-bits stable-ff-bits)
          l3 (p/layer-activate layer-3
                               (p/bits-value l4)
                               (p/stable-bits-value l4))]
      (assoc this
       :layer-4 l4
       :layer-3 l3)))

  (region-learn
    [this]
    (if (:freeze? (p/params this))
      this
      (assoc this
        :layer-4 (p/layer-learn layer-4)
        :layer-3 (p/layer-learn layer-3))))

  (region-depolarise
    [this distal-ff-bits apical-fb-bits apical-fb-wc-bits]
    ;; TODO feedback from L3 to L4?
    (let [l4 (p/layer-depolarise layer-4 distal-ff-bits #{} #{})
          l3 (p/layer-depolarise layer-3 #{} apical-fb-bits apical-fb-wc-bits)]
     (assoc this
       :layer-4 l4
       :layer-3 l3)))

  p/PTopological
  (topology [_]
    (p/topology layer-3))
  p/PFeedForward
  (ff-topology [_]
    (p/ff-topology layer-3))
  (bits-value [_]
    (p/bits-value layer-3))
  (stable-bits-value [_]
    (p/stable-bits-value layer-3))
  (source-of-bit [_ i]
    (p/source-of-bit layer-3 i))
  p/PFeedBack
  (wc-bits-value [_]
    (p/wc-bits-value layer-3))
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    ;; TODO
    topology/empty-topology)
  (motor-bits-value
    [_]
    (sequence nil))
  p/PTemporal
  (timestep [_]
    (p/timestep layer-4))
  p/PParameterised
  (params [_]
    (p/params layer-4))
  p/PInterruptable
  (break [this mode]
    (assoc this
           :layer-4 (p/break layer-4 mode)
           :layer-3 (p/break layer-3 mode)))
  p/PRestartable
  (restart [this]
    (sensorimotor-region (p/params this))))

(defn sensorimotor-region
  "Constructs a cortical region with two layers. `spec` can contain
  nested maps under :layer-3 and :layer-4 that are merged in for
  specific layers.

  This sets `:lateral-synapses? false` in Layer 4, and true in Layer
  3."
  [spec]
  (let [unk (set/difference (set (keys spec))
                            (set (keys cells/parameter-defaults))
                            #{:layer-4 :layer-3})]
    (when (seq unk)
      (println "Warning: unknown keys in spec:" unk)))
  (let [l4-spec (-> (assoc spec
                      :lateral-synapses? false)
                    (util/deep-merge (:layer-4 spec {}))
                    (dissoc :layer-3 :layer-4))
        l4 (cells/layer-of-cells l4-spec)
        l3-spec (-> (assoc spec
                      :input-dimensions (p/dimensions (p/ff-topology l4))
                      :distal-motor-dimensions [0]
                      :lateral-synapses? true)
                    (util/deep-merge (:layer-3 spec {}))
                    (dissoc :layer-3 :layer-4))
        l3 (cells/layer-of-cells l3-spec)]
    (map->SensoriMotorRegion
    {:layer-3 l3
     :layer-4 l4})))

(defrecord SenseNode
    [topo bits sensory? motor?]
  p/PTopological
  (topology [_]
    topo)
  p/PFeedForward
  (ff-topology [_]
    (if sensory?
      topo
      topology/empty-topology))
  (bits-value
    [_]
    (if sensory?
      bits
      (sequence nil)))
  (stable-bits-value
    [_]
    (sequence nil))
  (source-of-bit
    [_ i]
    [i])
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    (if motor?
      topo
      topology/empty-topology))
  (motor-bits-value
    [_]
    (if motor?
      bits
      (sequence nil)))
  p/PSense
  (sense-activate [this bits]
    (assoc this :bits bits)))

(defn sense-node
  "Creates a sense node with given topology, matching the encoder that
  will generate its bits."
  [topo sensory? motor?]
  (->SenseNode topo () sensory? motor?))

;;; ## Region Networks

(defn combined-bits-value
  "Returns the total bit set from a collection of sources satisfying
   `PFeedForward` or `PFeedForwardMotor`. `flavour` should
   be :standard, :stable or :motor."
  [ffs flavour]
  (let [topo-fn (case flavour
                  (:standard
                   :stable
                   :wc) p/ff-topology
                   :motor p/ff-motor-topology)
        bits-fn (case flavour
                  :standard p/bits-value
                  :stable p/stable-bits-value
                  :wc p/wc-bits-value
                  :motor p/motor-bits-value)
        widths (map (comp p/size topo-fn) ffs)]
    (->> (map bits-fn ffs)
         (util/align-indices widths)
         (into #{}))))

(defn source-of-incoming-bit
  "Taking the index of an input bit as received by the given region,
  return its source element as [k id] where k is the key of the source
  region or sense, and id is the index adjusted to refer to the output
  of that source.

  If i is an index into the feed-forward field, type is :ff-deps, if i
  is an index into the feed-back field, type is :fb-deps."
  ([htm rgn-id i type]
     (source-of-incoming-bit htm rgn-id i type p/ff-topology))
  ([htm rgn-id i type topology-fn]
     (let [senses (:senses htm)
           regions (:regions htm)
           node-ids (get-in htm [type rgn-id])]
       (loop [node-ids node-ids
              offset 0]
         (when-let [node-id (first node-ids)]
           (let [node (or (senses node-id)
                          (regions node-id))
                 width (long (p/size (topology-fn node)))]
             (if (< i (+ offset width))
               [node-id (- i offset)]
               (recur (next node-ids)
                      (+ offset width)))))))))

(defn source-of-distal-bit
  "Returns [src-id src-lyr-id j] where src-id may be a region key or
   sense key, src-lyr-id is nil for senses, and j is the index into
   the output of the source."
  [htm rgn-id lyr-id i]
  (let [rgn (get-in htm [:regions rgn-id])
        lyr (get rgn lyr-id)
        spec (p/params lyr)
        [src-type adj-i] (cells/id->source spec i)]
    (case src-type
      :this [rgn-id lyr-id i]
      :ff (if (= lyr-id (first (layers rgn)))
            (let [[src-id j] (source-of-incoming-bit htm rgn-id adj-i :ff-deps
                                                     p/ff-motor-topology)
                  src-rgn (get-in htm [:regions src-id])]
              [src-id
               (when src-rgn (last (layers src-rgn))) ;; nil for senses
               j])
            ;; this is not the input layer; source must be within region
            [rgn-id (first (layers rgn)) i]))))

(defn source-of-apical-bit
  "Returns [src-id src-lyr-id j] where src-id is a region key, and j
  is the index into the output of the region."
  [htm rgn-id lyr-id i]
  (let [rgn (get-in htm [:regions rgn-id])
        lyr (get rgn lyr-id)
        spec (p/params lyr)]
    (if (= lyr-id (last (layers rgn)))
      (let [[src-id j] (source-of-incoming-bit htm rgn-id i :fb-deps)
            src-rgn (get-in htm [:regions src-id])]
        [src-id
         (last (layers src-rgn))
         j])
      ;; this is not the top layer; source must be within region
      [rgn-id (last (layers rgn)) i])))

(defn topo-union
  [topos]
  (apply topology/combined-dimensions
         (map p/dimensions topos)))

;; TODO - better way to do this
(defn fb-dim-from-spec
  [spec]
  (let [spec (util/deep-merge cells/parameter-defaults spec)]
    (topology/make-topology (conj (:column-dimensions spec)
                                  (:depth spec)))))

(do #?(:cljs (def pmap map)))

(defrecord RegionNetwork
    [ff-deps fb-deps strata sensors senses regions]
  p/PHTM
  (htm-sense
    [this inval mode]
    (let [sm (reduce-kv (fn [m k sense-node]
                          (if (case mode
                                :sensory (:sensory? sense-node)
                                :motor (:motor? sense-node)
                                nil true)
                            (let [[selector encoder] (get sensors k)
                                  in-bits (->> (p/extract selector inval)
                                               (p/encode encoder))]
                              (assoc m k (p/sense-activate sense-node in-bits)))
                            m))
                        senses
                        senses)]
      (assoc this
             :senses sm
             :input-value inval)))

  (htm-activate
    [this]
    (let [rm (-> (reduce
                  (fn [m stratum]
                    (->> stratum
                         (pmap (fn [id]
                                 (let [region (regions id)
                                       ff-ids (ff-deps id)
                                       ffs (map m ff-ids)]
                                   (p/region-activate
                                    region
                                    (combined-bits-value ffs :standard)
                                    (combined-bits-value ffs :stable)))))
                         (zipmap stratum)
                         (into m)))
                  senses
                  ;; drop 1st stratum i.e. drop the sensory inputs
                  (rest strata))
                 ;; get rid of the sense nodes which were seeded into the reduce
                 (select-keys (keys regions)))]
      (assoc this :regions rm)))

  (htm-learn
    [this]
    (let [rm (->> (vals regions)
                  (pmap p/region-learn)
                  (zipmap (keys regions)))]
      (assoc this :regions rm)))

  (htm-depolarise
    [this]
    (let [rm (->> regions
                  (pmap (fn [[id region]]
                          (let [ff-ids (ff-deps id)
                                fb-ids (fb-deps id)
                                ffs (map #(or (senses %) (regions %))
                                         ff-ids)
                                fbs (map regions fb-ids)]
                            (p/region-depolarise
                             region
                             (combined-bits-value ffs :motor)
                             (combined-bits-value fbs :standard)
                             (combined-bits-value fbs :wc)))))
                  (zipmap (keys regions)))]
      (assoc this :regions rm)))

  p/PTemporal
  (timestep [_]
    (p/timestep (first (vals regions))))

  p/PInterruptable
  (break [this mode]
    (assoc this
           :regions (->> (vals regions)
                         (map #(p/break % mode))
                         (zipmap (keys regions)))))

  p/PRestartable
  (restart [this]
    (assoc this
      :regions (->> (vals regions)
                    (pmap p/restart)
                    (zipmap (keys regions))))))

(defn region-keys
  "A sequence of the keys of all regions in topologically-sorted
  order. If `n-levels` is provided, only the regions from that many
  hierarchical levels are included. So 1 gives the first tier directly
  receiving sensory inputs."
  ([htm]
   (region-keys htm (dec (count (:strata htm)))))
  ([htm n-levels]
   ;; topologically sorted: drop 1st stratum i.e. drop the sensory inputs
   (apply concat (take n-levels (rest (:strata htm))))))

(defn sense-keys
  "A sequence of the keys of all sense nodes."
  [htm]
  (first (:strata htm)))

(defn region-seq
  [htm]
  (map (:regions htm) (region-keys htm)))

(defn- in-vals-not-keys
  [deps]
  (let [have-deps (set (keys deps))
        are-deps (set (apply concat (vals deps)))]
    (set/difference are-deps have-deps)))

(defn region-network
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
  {:pre [;; all regions must have dependencies
         (every? ff-deps (keys region-specs))
         ;; all sense nodes must not have dependencies
         (every? (in-vals-not-keys ff-deps) (keys main-sensors))
         (every? (in-vals-not-keys ff-deps) (keys motor-sensors))
         ;; all ids in dependency map must be defined
         (every? region-specs (keys ff-deps))
         (every? (merge main-sensors motor-sensors) (in-vals-not-keys ff-deps))]}
  (merge-with (fn [main-sensor motor-sensor]
                (assert (= main-sensor motor-sensor)
                        "Equal keys in main-sensors and motor-sensors must be same sensor."))
              main-sensors motor-sensors)
  (let [all-ids (into (set (keys ff-deps))
                      (in-vals-not-keys ff-deps))
        ff-dag (graph/directed-graph all-ids ff-deps)
        strata (graph/dependency-list ff-dag)
        fb-deps (->> (graph/reverse-graph ff-dag)
                         :neighbors
                         (util/remap seq))
        ;; sensors may appear with same key in both main- and motor-
        sm (->> (merge-with merge
                            (util/remap (fn [[_ e]]
                                          {:topo (p/topology e), :sensory? true})
                                        main-sensors)
                            (util/remap (fn [[_ e]]
                                          {:topo (p/topology e), :motor? true})
                                        motor-sensors))
                (util/remap (fn [{:keys [topo sensory? motor?]}]
                              (sense-node topo sensory? motor?))))
        rm (-> (reduce (fn [m id]
                         (let [spec (region-specs id)
                               build-region (region-builders id)
                               ;; feed-forward
                               ff-ids (ff-deps id)
                               ffs (map m ff-ids)
                               ff-dim (topo-union (map p/ff-topology ffs))
                               ffm-dim (topo-union  (map p/ff-motor-topology ffs))
                               ;; top-down feedback (if any)
                               fb-ids (fb-deps id)
                               fb-specs (map region-specs fb-ids)
                               fb-dim (topo-union (map fb-dim-from-spec fb-specs))]
                           (->> (assoc spec :input-dimensions ff-dim
                                       :distal-motor-dimensions ffm-dim
                                       :distal-topdown-dimensions fb-dim)
                                (build-region)
                                (assoc m id))))
                       sm
                       ;; topological sort. drop 1st stratum i.e. senses
                       (apply concat (rest strata)))
               ;; get rid of the sense nodes which were seeded into the reduce
               (select-keys (keys region-specs)))]
    (map->RegionNetwork
     {:ff-deps ff-deps
      :fb-deps fb-deps
      :strata strata
      :sensors (merge main-sensors motor-sensors)
      :senses sm
      :regions rm})))

(defn regions-in-series
  "Constructs an HTM network consisting of n regions in a linear
  series. The regions are given keys :rgn-0, :rgn-1, etc. Senses feed
  only to the first region. Their sensors are given in a map with
  keyword keys. Sensors are defined to be the form `[selector encoder]`.

  This is a convenience wrapper around `region-network`."
  ([n build-region specs sensors]
   (regions-in-series
    n build-region specs sensors nil))
  ([n build-region specs main-sensors motor-sensors]
   {:pre [(sequential? specs)
          (= n (count (take n specs)))]}
   (let [rgn-keys (map #(keyword (str "rgn-" %)) (range n))
         sense-keys (keys (merge main-sensors motor-sensors))
         ;; make {:rgn-0 [senses], :rgn-1 [:rgn-0], :rgn-2 [:rgn-1], ...}
         deps (zipmap rgn-keys (list* sense-keys (map vector rgn-keys)))]
     (region-network deps
                     (constantly build-region)
                     (zipmap rgn-keys specs)
                     main-sensors
                     motor-sensors))))

;;; ## Stats

(defn column-state-freqs
  "Returns a map with the frequencies of columns in states
  `:active` (bursting), `:predicted`, `:active-predicted`. Note that
  these are distinct categories. The names are possibly misleading.
  Argument `layer-fn` is called on the region to obtain a layer of
  cells; if omitted it defaults to the output layer."
  ([rgn]
     (column-state-freqs rgn (last (layers rgn))))
  ([rgn layer-fn]
     (let [lyr (layer-fn rgn)
           a-cols (p/active-columns lyr)
           ppc (p/prior-predictive-cells lyr)
           pp-cols (into #{} (map first ppc))
           hit-cols (set/intersection pp-cols a-cols)
           col-states (merge (zipmap pp-cols (repeat :predicted))
                             (zipmap a-cols (repeat :active))
                             (zipmap hit-cols (repeat :active-predicted)))]
       (-> {:active 0, :predicted 0, :active-predicted 0}
           (merge (frequencies (vals col-states)))
           (assoc :timestep (p/timestep rgn)
                  :size (p/size (p/topology rgn)))))))

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

(defn predicted-bit-votes
  [rgn]
  (let [lyr (get rgn (first (layers rgn)))
        pc (p/predictive-cells lyr)]
    (cells-proximal-bit-votes lyr pc)))

(defn ff-base
  "Returns the first index that corresponds with `ff-id` within the
  feedforward input to `rgn-id`."
  [htm rgn-id ff-id]
  (let [{:keys [senses regions]} htm]
    (->> (get-in htm [:ff-deps rgn-id])
         (map (fn [id]
                [id
                 (or (senses id)
                     (regions id))]))
         (take-while (fn [[id _]]
                       (not= id ff-id)))
         (map (fn [[id ff]]
                ff))
         (map p/ff-topology)
         (map p/size)
         (reduce + 0))))

(defn predictions
  ([htm sense-id n-predictions]
   (predictions
    htm sense-id n-predictions p/predictive-cells))
  ([htm sense-id n-predictions cells-fn]
   (let [sense-width (-> (get-in htm [:senses sense-id])
                         p/ff-topology
                         p/size)
         pr-votes (->> (get-in htm [:fb-deps sense-id])
                       (mapcat (fn [rgn-id]
                                 (let [rgn (get-in htm [:regions rgn-id])
                                       start (ff-base htm rgn-id sense-id)
                                       end (+ start sense-width)
                                       lyr (get rgn (first (layers rgn)))
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
  * :proximal-unstable - a map keyed by source region/sense id.
  * :proximal-stable - a map keyed by source region/sense id.
  * :distal - a map keyed by source region/sense id.
  * :boost - number.
  "
  [htm prior-htm rgn-id lyr-id cell-ids]
  (let [rgn (get-in htm [:regions rgn-id])
        lyr (get-in htm [:regions rgn-id lyr-id])
        prior-lyr (get-in prior-htm [:regions rgn-id lyr-id])
        spec (:spec lyr)
        ff-stim-thresh (:stimulus-threshold (:proximal spec))
        d-stim-thresh (:stimulus-threshold (:distal spec))
        a-stim-thresh (:stimulus-threshold (:apical spec))
        distal-weight (:distal-vs-proximal-weight spec)
        state (:state lyr)
        prior-state (:state prior-lyr)
        distal-state (:distal-state prior-lyr)
        apical-state (:apical-state prior-lyr)
        ;; inputs to layer
        ff-bits (:in-ff-bits state)
        ff-s-bits (:in-stable-ff-bits state)
        ff-b-bits (set/difference ff-bits ff-s-bits)
        distal-bits (:active-bits distal-state)
        apical-bits (:active-bits apical-state)
        is-input-layer? (= lyr-id (first (layers rgn)))
        ff-bits-srcs (if is-input-layer?
                       (into {}
                             (map (fn [i]
                                    (let [[k _] (source-of-incoming-bit
                                                 htm rgn-id i :ff-bits)]
                                      [i k])))
                             ff-bits)
                       (constantly rgn-id))
        distal-bits-srcs (into {}
                               (map (fn [i]
                                      (let [[k _] (source-of-distal-bit
                                                   htm rgn-id lyr-id i)]
                                        [i k])))
                               distal-bits)
        apical-bits-srcs (into {}
                               (map (fn [i]
                                      (let [[k _] (source-of-apical-bit
                                                   htm rgn-id lyr-id i)]
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
                       [ff-seg-path _] (get (:matching-ff-seg-paths state) [col 0])
                       ff-conn-sources (when ff-seg-path
                                         (p/sources-connected-to psg ff-seg-path))
                       active-ff-b (->> (filter ff-b-bits ff-conn-sources)
                                        (zap-fewer ff-stim-thresh))
                       active-ff-s (->> (filter ff-s-bits ff-conn-sources)
                                        (zap-fewer ff-stim-thresh))
                       ff-b-by-src (frequencies (map ff-bits-srcs active-ff-b))
                       ff-s-by-src (frequencies (map ff-bits-srcs active-ff-s))
                       ;; breakdown of distal excitation by source
                       [d-seg-path _] (get (:matching-seg-paths distal-state) cell-id)
                       d-conn-sources (when d-seg-path
                                        (p/sources-connected-to dsg d-seg-path))
                       active-d (->> (filter distal-bits d-conn-sources)
                                     (zap-fewer d-stim-thresh))
                       d-by-src (->> (frequencies (map distal-bits-srcs active-d))
                                     (util/remap #(* % distal-weight)))
                       ;; same for apical
                       [a-seg-path _] (get (:matching-seg-paths apical-state) cell-id)
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
