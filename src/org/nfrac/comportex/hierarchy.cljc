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
(s/def ::strata (s/coll-of (s/coll-of ::layer-id :kind set?)))
(s/def ::sensors (s/map-of keyword? ::p/sensor))

(s/def ::network
  (s/keys :req-un [::ff-deps
                   ::fb-deps
                   ::strata
                   ::layers
                   ::sensors
                   ::senses]))

(defrecord SenseNode
    [topo bits sensory? motor?]
  p/PTopographic
  (topography [_]
    topo)
  p/PFeedForward
  (ff-topography [_]
    (if sensory?
      topo
      topography/empty-topography))
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
  (ff-motor-topography [_]
    (if motor?
      topo
      topography/empty-topography))
  (motor-bits-value
    [_]
    (if motor?
      bits
      (sequence nil)))
  p/PSense
  (sense-activate [this bits]
    (assoc this :bits bits)))

(defn sense-node
  "Creates a sense node with given topography, matching the encoder that
  will generate its bits."
  [topo sensory? motor?]
  (->SenseNode topo () sensory? motor?))

;;; ## Networks

(defn combined-bits-value
  "Returns the total bit set from a collection of sources satisfying
   `PFeedForward` or `PFeedForwardMotor`. `flavour` should
   be :standard, :stable or :motor."
  [ffs flavour]
  (let [topo-fn (case flavour
                  (:standard
                   :stable
                   :wc) p/ff-topography
                   :motor p/ff-motor-topography)
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
  "Taking the index of an input bit as received by the given layer, return its
  source element as [src-id j] where src-id is the key of the source layer or
  sense, and j is the index adjusted to refer to the output of that source.

  If i is an index into the feed-forward field, type is :ff-deps, if i
  is an index into the feed-back field, type is :fb-deps."
  ([htm lyr-id i type]
   (source-of-incoming-bit htm lyr-id i type p/ff-topography))
  ([htm lyr-id i type topography-fn]
   (let [senses (:senses htm)
         layers (:layers htm)
         node-ids (get-in htm [type lyr-id])]
     (loop [node-ids node-ids
            offset 0]
       (when-let [node-id (first node-ids)]
         (let [node (or (senses node-id)
                        (layers node-id))
               width (long (p/size (topography-fn node)))]
           (if (< i (+ offset width))
             [node-id (- i offset)]
             (recur (next node-ids)
                    (+ offset width)))))))))

(s/fdef source-of-incoming-bit
        :args (s/cat :htm ::network
                     :lyr-id ::layer-id
                     :i nat-int?
                     :type #{:ff-deps :fb-deps}
                     :topography-fn (s/fspec :ret ::p/bits))
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
      :ff (source-of-incoming-bit htm lyr-id j :ff-deps p/ff-motor-topography))))

(defn source-of-apical-bit
  "Returns [src-id j] where src-id is a layer key, and j is the index into the
  output of the source layer."
  [htm lyr-id i]
  (source-of-incoming-bit htm lyr-id i :fb-deps))

(defn topo-union
  [topos]
  (apply topography/combined-dimensions
         (map p/dimensions topos)))

;; TODO - better way to do this
(defn fb-dim-from-params
  [params]
  (let [params (util/deep-merge layer/parameter-defaults params)]
    (topography/make-topography (conj (:column-dimensions params)
                                      (:depth params)))))

(do #?(:cljs (def pmap map)))

(defn- htm-sense-impl
  [this inval mode]
  (let [{:keys [sensors senses]} this
        sm (reduce-kv (fn [m k sense-node]
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

(defn- htm-activate-impl
  [this]
  (let [{:keys [strata ff-deps layers senses]} this
        lm (-> (reduce
                (fn [m stratum]
                  (->> stratum
                       (pmap (fn [id]
                               (let [ff-ids (ff-deps id)
                                     ffs (map m ff-ids)]
                                 (p/layer-activate
                                  (get layers id)
                                  (combined-bits-value ffs :standard)
                                  (combined-bits-value ffs :stable)))))
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
  (let [{:keys [ff-deps fb-deps layers senses]} this
        lm (->> layers
                (pmap (fn [[id layer]]
                        (let [ff-ids (ff-deps id)
                              fb-ids (fb-deps id)
                              ffs (map #(or (senses %) (layers %))
                                       ff-ids)
                              fbs (map layers fb-ids)]
                          (p/layer-depolarise
                           layer
                           (combined-bits-value ffs :motor)
                           (combined-bits-value fbs :standard)
                           (combined-bits-value fbs :wc)))))
                (zipmap (keys layers)))]
    (assoc this :layers lm)))

(defrecord Network
    [ff-deps fb-deps strata layers sensors senses]
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
  (first (:strata htm)))

(defn layer-seq
  [htm]
  (map (:layers htm) (layer-keys htm)))

(defn- in-vals-not-keys
  [deps]
  (let [have-deps (set (keys deps))
        are-deps (set (apply concat (vals deps)))]
    (set/difference are-deps have-deps)))

(defn network
  "Builds a network of layers and senses from the given dependency
  map. The keywords used in the dependency map are used to look up
  layer-building functions, parameter specifications, and sensors in
  the remaining argments.

  Sensors are defined to be the form `[selector encoder]`, satisfying
  protocols PSelector and PEncoder respectively. Sensors in the
  `main-sensors` map can make activating (proximal) connections while
  those in the `motor-sensors` map can make depolarising (distal)
  connections. The same sensor may also be included in both maps.

  For each node, the combined dimensions of its feed-forward sources
  is calculated and used to set the `:input-dimensions` parameter in
  its `params`. Also, the combined dimensions of feed-forward motor
  inputs are used to set the `:distal-motor-dimensions` parameter, and
  the combined dimensions of its feed-back superior layers is used to
  set the `:distal-topdown-dimensions` parameter. The updated params
  are passed to a function to build a layer. The build function is found
  by calling `layer-builders` with the layer id keyword.

  So: [params (-> layer-id layer-params attach-dimensions)
       builder (layer-builders layer-id)
       layer (builder params)]

  For example to build the network `inp -> v1 -> v2`:

   `
   (network
    {:v1 [:input]
     :v2 [:v1]}
    (constantly layer/layer-of-cells)
    {:v1 params
     :v2 params}
    {:input sensor}
    nil)`"
  [ff-deps layer-builders layer-params main-sensors motor-sensors]
  {:pre [;; all layers must have dependencies
         (every? ff-deps (keys layer-params))
         ;; all sense nodes must not have dependencies
         (every? (in-vals-not-keys ff-deps) (keys main-sensors))
         (every? (in-vals-not-keys ff-deps) (keys motor-sensors))
         ;; all ids in dependency map must be defined
         (every? layer-params (keys ff-deps))
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
                                          {:topo (p/topography e), :sensory? true})
                                        main-sensors)
                            (util/remap (fn [[_ e]]
                                          {:topo (p/topography e), :motor? true})
                                        motor-sensors))
                (util/remap (fn [{:keys [topo sensory? motor?]}]
                              (sense-node topo sensory? motor?))))
        rm (-> (reduce (fn [m id]
                         (let [params (layer-params id)
                               build-layer (layer-builders id)
                               ;; feed-forward
                               ff-ids (ff-deps id)
                               ffs (map m ff-ids)
                               ff-dim (topo-union (map p/ff-topography ffs))
                               ffm-dim (topo-union  (map p/ff-motor-topography ffs))
                               ;; top-down feedback (if any)
                               fb-ids (fb-deps id)
                               fb-params (map layer-params fb-ids)
                               fb-dim (topo-union (map fb-dim-from-params fb-params))]
                           (->> (assoc params :input-dimensions ff-dim
                                       :distal-motor-dimensions ffm-dim
                                       :distal-topdown-dimensions fb-dim)
                                (build-layer)
                                (assoc m id))))
                       sm
                       ;; topological sort. drop 1st stratum i.e. senses
                       (apply concat (rest strata)))
               ;; get rid of the sense nodes which were seeded into the reduce
               (select-keys (keys layer-params)))]
    (map->Network
     {:ff-deps ff-deps
      :fb-deps fb-deps
      :strata strata
      :sensors (merge main-sensors motor-sensors)
      :senses sm
      :regions (delay (throw (ex-info "bad key :regions" {})))
      :layers rm})))

(s/fdef network
        :args (s/cat :ff-deps ::ff-deps
                     :layer-builders (s/fspec
                                      :args (s/cat :layer-id ::layer-id)
                                      :ret (s/fspec
                                            :args (s/cat :params (s/keys))
                                            :ret ::p/layer-of-cells))
                     :layer-params (s/map-of ::layer-id (s/keys))
                     :main-sensors (s/map-of keyword? ::p/sensor)
                     :motor-sensors (s/map-of keyword? ::p/sensor))
        :ret ::network)

(defn layers-in-series
  "Constructs an HTM network consisting of n layers in a linear
  series. The layers are given keys :layer-a, :layer-b, etc. Senses feed
  only to the first layer. Their sensors are given in a map with
  keyword keys. Sensors are defined to be the form `[selector encoder]`.

  This is a convenience wrapper around `network`."
  ([n build-layer paramseq sensors]
   (layers-in-series
    n build-layer paramseq sensors nil))
  ([n build-layer paramseq main-sensors motor-sensors]
   {:pre [(sequential? paramseq)
          (= n (count (take n paramseq)))]}
   (let [char-a 97
         lyr-keys (->> (range n)
                       (map #(char (+ % char-a)))
                       (map #(keyword (str "layer-" %))))
         sense-keys (keys (merge main-sensors motor-sensors))
         ;; {:layer-a [senses], :layer-b [:layer-a], :layer-c [:layer-b], ...}
         deps (zipmap lyr-keys (list* sense-keys (map vector lyr-keys)))]
     (network deps
              (constantly build-layer)
              (zipmap lyr-keys paramseq)
              main-sensors
              motor-sensors))))

(s/fdef layers-in-series
        :args (s/cat :n (-> (s/int-in 1 1000)
                            (s/with-gen #(s/gen (s/int-in 1 2))))
                     :build-layer (s/fspec :args (s/cat :params (s/keys))
                                           :ret ::p/layer-of-cells)
                     :paramseq (s/coll-of (s/keys))
                     :sensors (s/map-of keyword? ::p/sensor)
                     :motor-sensors (s/? (s/map-of keyword? ::p/sensor)))
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
         (map p/ff-topography)
         (map p/size)
         (reduce + 0))))

(defn predictions
  ([htm sense-id n-predictions]
   (predictions
    htm sense-id n-predictions (comp :predictive-cells p/layer-state)))
  ([htm sense-id n-predictions cells-fn]
   (let [sense-width (-> (get-in htm [:senses sense-id])
                         p/ff-topography
                         p/size)
         pr-votes (->> (get-in htm [:fb-deps sense-id])
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
