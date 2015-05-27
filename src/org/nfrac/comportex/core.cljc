(ns org.nfrac.comportex.core
  "A _region_ is the main composable unit in this library. It
   represents a bank of neurons arranged in columns, responding to an
   array of feed-forward input bits, as well as distal connections to
   itself and possibly other regions."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.algo-graph :as graph]
            [cljs-uuid.core :as uuid]
            [clojure.set :as set]))

(defn layers
  "first is the input layer, last is the output layer."
  [rgn]
  (concat (when (:layer-4 rgn) [:layer-4])
          (when (:layer-3 rgn) [:layer-3])))

(declare sensory-region)

(defrecord SensoryRegion
    [layer-3 uuid step-counter]
  p/PRegion
  (region-activate
    [this ff-bits stable-ff-bits]
    (assoc this
      :step-counter (inc step-counter)
      :layer-3 (p/layer-activate layer-3 ff-bits stable-ff-bits)))

  (region-learn
    [this]
    (if (:freeze? (p/params this))
      this
      (assoc this
        :layer-3 (p/layer-learn layer-3))))

  (region-depolarise
    [this distal-ff-bits distal-fb-bits]
    (assoc this
      :layer-3 (p/layer-depolarise layer-3 distal-ff-bits distal-fb-bits)))

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
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    topology/empty-topology)
  (motor-bits-value
    [_]
    (sequence nil))
  p/PTemporal
  (timestep [_]
    step-counter)
  p/PParameterised
  (params [_]
    (p/params layer-3))
  p/PResettable
  (reset [this]
    (-> (sensory-region (p/params this))
        (assoc :uuid uuid))))

(defn sensory-region
  "Constructs a cortical region with the given specification map. See
   documentation on `cells/parameter-defaults` for possible keys. Any
   keys given here will override those default values."
  [spec]
  (let [unk (set/difference (set (keys spec))
                            (set (keys cells/parameter-defaults)))]
    (when (seq unk)
      (println "Warning: unknown keys in spec:" unk)))
  (map->SensoryRegion
   {:layer-3 (cells/layer-of-cells spec)
    :uuid (uuid/make-random)
    :step-counter 0}))

(declare sensorimotor-region)

(defrecord SensoriMotorRegion
    [layer-4 layer-3 uuid step-counter]
  p/PRegion
  (region-activate
    [this ff-bits stable-ff-bits]
    (let [l4 (p/layer-activate layer-4 ff-bits stable-ff-bits)
          l3 (p/layer-activate layer-3
                               (p/bits-value l4)
                               (p/stable-bits-value l4))]
      (assoc this
       :step-counter (inc step-counter)
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
    [this distal-ff-bits distal-fb-bits]
    ;; TODO feedback from L3 to L4?
    (let [l4 (p/layer-depolarise layer-4 distal-ff-bits #{})
          l3 (p/layer-depolarise layer-3 #{} distal-fb-bits)]
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
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    ;; TODO
    topology/empty-topology)
  (motor-bits-value
    [_]
    (sequence nil))
  p/PTemporal
  (timestep [_]
    step-counter)
  p/PParameterised
  (params [_]
    (p/params layer-4))
  p/PResettable
  (reset [this]
    (-> (sensorimotor-region (p/params this))
        (assoc :uuid uuid))))

(defn sensorimotor-region
  "spec can contain nested maps under :layer-3 and :layer-4 that are
   merged in for specific layers."
  [spec]
  (let [unk (set/difference (set (keys spec))
                            (set (keys cells/parameter-defaults))
                            #{:layer-4 :layer-3})]
    (when (seq unk)
      (println "Warning: unknown keys in spec:" unk)))
  (let [l4-spec (-> (assoc spec
                      :lateral-synapses? false)
                    (merge (:layer-4 spec))
                    (dissoc :layer-3 :layer-4))
        l4 (cells/layer-of-cells l4-spec)
        l3-spec (-> (assoc spec
                      :input-dimensions (p/dimensions (p/ff-topology l4))
                      :distal-motor-dimensions [0]
                      :lateral-synapses? true)
                    (merge (:layer-3 spec))
                    (dissoc :layer-3 :layer-4))
        l3 (cells/layer-of-cells l3-spec)]
    (map->SensoriMotorRegion
    {:layer-3 l3
     :layer-4 l4
     :uuid (uuid/make-random)
     :step-counter 0})))

(defrecord ExportedSensoriMotorInput
    [value topo ff-topo bitsv stable-bitsv ff-motor-topo motor-bitsv]
  p/PTopological
  (topology [_]
    topo)
  p/PFeedForward
  (ff-topology [_]
    ff-topo)
  (bits-value [_]
    bitsv)
  (stable-bits-value [_]
    stable-bitsv)
  (source-of-bit [_ i]
    [i])
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    ff-motor-topo)
  (motor-bits-value [_]
    motor-bitsv))

(defrecord SensoriMotorInput
    [encoder motor-encoder value]
  p/PTopological
  (topology [_]
    (if encoder
      (p/topology encoder)
      (p/topology motor-encoder)))
  p/PFeedForward
  (ff-topology [_]
    (if encoder
      (p/topology encoder)
      topology/empty-topology))
  (bits-value
    [_]
    (if encoder
      (p/encode encoder value)
      (sequence nil)))
  (stable-bits-value
    [_]
    (sequence nil))
  (source-of-bit
    [_ i]
    [i])
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    (if motor-encoder
      (p/topology motor-encoder)
      topology/empty-topology))
  (motor-bits-value
    [_]
    (if motor-encoder
      (p/encode motor-encoder value)
      (sequence nil)))
  p/PInputSource
  (input-step [this in-value]
    (assoc this :value in-value))
  (input-export [this]
    (->ExportedSensoriMotorInput value
                                 (p/topology this)
                                 (p/ff-topology this)
                                 (p/bits-value this)
                                 (p/stable-bits-value this)
                                 (p/ff-motor-topology this)
                                 (p/motor-bits-value this))))

(defn sensory-input
  "Creates an input source from an encoder."
  [encoder]
  (->SensoriMotorInput encoder nil nil))

(defn sensorimotor-input
  "Creates an input source from an encoder (for the proximal
   feed-forward output) and/or a motor encoder (for the distal
   feed-forward output). The encoders operate on the same value so
   should select their relevant parts of it. Remember that HTM models
   go through the three phases [activate -> learn -> depolarise] on
   each timestep: therefore motor signals, which act to depolarise
   cells, should appear the time step before a corresponding sensory
   signal."
  [encoder motor-encoder]
  (->SensoriMotorInput encoder motor-encoder nil))

;;; ## Region Networks

(defn combined-bits-value
  "Returns the total bit set from a collection of sources satisfying
   `PFeedForward` or `PFeedForwardMotor`. `flavour` should
   be :standard, :stable or :motor."
  [ffs flavour]
  (let [topo-fn (case flavour
                  (:standard
                   :stable) p/ff-topology
                   :motor p/ff-motor-topology)
        bits-fn (case flavour
                  :standard p/bits-value
                  :stable p/stable-bits-value
                  :motor p/motor-bits-value)
        widths (map (comp p/size topo-fn) ffs)]
    (->> (map bits-fn ffs)
         (util/align-indices widths)
         (into #{}))))

(defn source-of-incoming-bit
  "Taking the index of an input bit as received by the given region,
  return its source element as [k id] where k is the key of the source
  region or input, and id is the index adjusted to refer to the output
  of that source."
  ([htm rgn-id i]
     (source-of-incoming-bit htm rgn-id i p/ff-topology))
  ([htm rgn-id i topology-fn]
     (let [inputs (:inputs htm)
           regions (:regions htm)
           ff-ids (get-in htm [:ff-deps rgn-id])]
       (loop [ff-ids ff-ids
              offset 0]
         (when-let [ff-id (first ff-ids)]
           (let [ff (or (inputs ff-id)
                        (regions ff-id))
                 width (long (p/size (topology-fn ff)))]
             (if (< i (+ offset width))
               [ff-id (- i offset)]
               (recur (next ff-ids)
                      (+ offset width)))))))))

(defn source-of-distal-bit
  "Returns [src-id src-lyr-id j] where src-id may be a region key or
   input key, src-lyr-id is nil for inputs, and j is the index into
   the output of the source."
  [htm rgn-id lyr-id i]
  (let [rgn (get-in htm [:regions rgn-id])
        lyr (get rgn lyr-id)
        spec (p/params lyr)
        [src-type adj-i] (cells/id->source spec i)]
    (case src-type
      :this [rgn-id lyr-id i]
      :ff (if (= lyr-id (first (layers rgn)))
            (let [[src-id j] (source-of-incoming-bit htm rgn-id adj-i
                                                     p/ff-motor-topology)
                  src-rgn (get-in htm [:regions src-id])]
              [src-id
               (when src-rgn (last (layers src-rgn))) ;; nil for inputs
               j])
            ;; this is not the input layer; source may be within region?
            [])
      :fb (let [fb-ids (get-in htm [:fb-deps rgn-id])
                ;; TODO trace multiple
                src-rgn-id (first fb-ids)
                src-rgn (get-in htm [:regions src-rgn-id])]
            [src-rgn-id (last (layers src-rgn)) adj-i]))))

(defn topo-union
  [topos]
  (apply topology/combined-dimensions
         (map p/dimensions topos)))

;; TODO - better way to do this
(defn fb-dim-from-spec
  [spec]
  (let [spec (merge cells/parameter-defaults spec)]
    (topology/make-topology (conj (:column-dimensions spec)
                                  (:depth spec)))))

#?(:cljs (def pmap map))

(defrecord RegionNetwork
    [ff-deps fb-deps strata inputs regions uuid->id]
  p/PHTM
  (htm-activate
    [this in-value]
    (let [im (zipmap (keys inputs)
                     (map p/input-step (vals inputs) (repeat in-value)))
          rm (-> (reduce
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
                  im
                  ;; drop 1st stratum i.e. drop the inputs
                  (rest strata))
                 ;; get rid of the inputs which were seeded into the reduce
                 (select-keys (keys regions)))]
      (assoc this :inputs im :regions rm)))

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
                                ffs (map #(or (inputs %) (regions %))
                                         ff-ids)
                                fbs (map regions fb-ids)]
                            (p/region-depolarise
                             region
                             (combined-bits-value ffs :motor)
                             (combined-bits-value fbs :standard)))))
                  (zipmap (keys regions)))]
      (assoc this :regions rm)))

  (htm-export
    [this]
    (assoc this
      :inputs (zipmap (keys inputs)
                      (map p/input-export (vals inputs)))))

  p/PTemporal
  (timestep [_]
    (p/timestep (first (vals regions))))
  p/PResettable
  (reset [this]
    (assoc this
      :regions (->> (vals regions)
                    (pmap p/reset)
                    (zipmap (keys regions))))))

(defn region-keys
  "A sequence of the keys of all regions in topologically-sorted order."
  [this]
  ;; topologically sorted: drop 1st stratum i.e. drop the inputs
  (apply concat (rest (:strata this))))

(defn input-keys
  "A sequence of the keys of all inputs."
  [this]
  (first (:strata this)))

(defn region-seq
  [this]
  (map (:regions this) (region-keys this)))

(defn input-seq
  [this]
  (map (:inputs this) (input-keys this)))

(defn- in-vals-not-keys
  [deps]
  (let [have-deps (set (keys deps))
        are-deps (set (apply concat (vals deps)))]
    (set/difference are-deps have-deps)))

(defn region-network
  "Builds a network of regions and inputs from the given dependency map.

   For each node, the combined dimensions of its feed-forward sources
   is calculated and used to set the `:input-dimensions` parameter in
   its `spec`. Also, the combined dimensions of feed-forward motor
   inputs are used to set the `:distal-motor-dimensions` parameter,
   and the combined dimensions of its feed-back superior regions is
   used to set the `:distal-topdown-dimensions` parameter. The updated
   spec is passed to `build-region`, which is typically
   `sensory-region`. Returns a RegionNetwork.

   For example to build the network `inp -> v1 -> v2`:

   `
   (region-network {:v1 [:inp]
                    :v2 [:v1]}
                   {:inp (sensory-input nil input-transform encoder)}
                   sensory-region
                   {:v1 spec
                    :v2 spec})`"
  [ff-deps inputs build-region region-specs]
  {:pre [;; anything with a dependency must be a region
         (every? ff-deps (keys region-specs))
         ;; anything without a dependency must be an input
         (every? (in-vals-not-keys ff-deps) (keys inputs))
         ;; all ids in dependency map must be defined
         (every? region-specs (keys ff-deps))
         (every? inputs (in-vals-not-keys ff-deps))]}
  (let [all-ids (into (set (keys ff-deps))
                      (in-vals-not-keys ff-deps))
        ff-dag (graph/directed-graph all-ids ff-deps)
        strata (graph/dependency-list ff-dag)
        fb-deps (->> (graph/reverse-graph ff-dag)
                         :neighbors
                         (util/remap seq))
        rm (-> (reduce (fn [m id]
                         (let [spec (region-specs id)
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
                       inputs
                       ;; topological sort. drop 1st stratum i.e. drop the inputs
                       (apply concat (rest strata)))
               ;; get rid of the inputs which were seeded into the reduce
               (select-keys (keys region-specs)))]
    (map->RegionNetwork
     {:ff-deps ff-deps
      :fb-deps fb-deps
      :strata strata
      :inputs inputs
      :regions rm
      :uuid->id (zipmap (map :uuid (vals rm)) (keys rm))})))

(defn regions-in-series
  "Constructs an HTM network consisting of one input and n regions in
   a linear series. The input key is :input, optional motor input key
   is :motor, and the region keys are :rgn-0, :rgn-1, etc. See
   `region-network`."
  ([build-region input n specs]
   (regions-in-series build-region input nil n specs))
  ([build-region input motor-input n specs]
   {:pre [(sequential? specs)
          (= n (count (take n specs)))]}
   (let [rgn-keys (map #(keyword (str "rgn-" %)) (range n))
         inp-keys (if motor-input [:input :motor] [:input])
         ;; make {:rgn-0 [:input], :rgn-1 [:rgn-0], :rgn-2 [:rgn-1], ...}
         deps (zipmap rgn-keys (list* inp-keys (map vector rgn-keys)))]
     (region-network deps
                     (zipmap inp-keys [input motor-input])
                     build-region
                     (zipmap rgn-keys specs)))))

;;; ## Stats

(defn column-state-freqs
  "Returns a map with the frequencies of columns in states
  `:active` (bursting), `:predicted`, `:active-predicted`. Note that
  these are distinct categories. The names are possibly misleading.
  Argument `layer-fn` is called on the region to obtain a layer of
  cells; if omitted it defaults to `:layer-3`."
  ([rgn]
     (column-state-freqs rgn :layer-3))
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

;;; ## Tracing columns back to input

(defn layer-predicted-bit-votes
  "Returns a map from input bit index to the number of connections to
  it from cells in the predictive state."
  [lyr]
  (let [psg (:proximal-sg lyr)]
    (->> (p/predictive-cells lyr)
         (map first)
         (reduce (fn [m col]
                   ;; TODO: other proximal segments
                   (let [ids (p/sources-connected-to psg [col 0])]
                     (reduce (fn [m id]
                               (assoc! m id (inc (get m id 0))))
                             m ids)))
                 (transient {}))
         (persistent!))))

(defn predicted-bit-votes
  [rgn]
  (let [lyr (get rgn (first (layers rgn)))]
    (layer-predicted-bit-votes lyr)))

(defn predictions
  [htm n-predictions]
  (let [rgn (first (region-seq htm))
        inp (first (input-seq htm))
        pr-votes (predicted-bit-votes rgn)]
    (p/decode (:encoder inp) pr-votes n-predictions)))
