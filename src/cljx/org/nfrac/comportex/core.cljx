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

(defn cell-id->inbit
  [offset depth [col ci]]
  (+ offset (* depth col) ci))

(defn inbit->cell-id
  [depth i]
  [(quot i depth)
   (rem i depth)])

(declare sensory-region)

(defrecord SensoryRegion
    [layer-3 uuid step-counter]
  p/PRegion
  (region-activate
    [this ff-bits signal-ff-bits]
    (assoc this
      :step-counter (inc step-counter)
      :layer-3 (p/layer-activate layer-3 ff-bits signal-ff-bits)))

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
  (ff-topology [this]
    (topology/make-topology (conj (p/dims-of this)
                                  (p/layer-depth layer-3))))
  (bits-value
    [this offset]
    (let [depth (p/layer-depth layer-3)]
      (->> (p/active-cells layer-3)
           (mapv (partial cell-id->inbit offset depth))
           (into #{}))))
  (signal-bits-value
    [_ offset]
    (let [depth (p/layer-depth layer-3)]
      (->> (p/signal-cells layer-3)
           (mapv (partial cell-id->inbit offset depth))
           (into #{}))))
  (source-of-bit
    [_ i]
    (let [depth (p/layer-depth layer-3)]
      (inbit->cell-id depth i)))
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    topology/empty-topology)
  (motor-bits-value
    [_ offset]
    #{})
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

(defn cells->cols
  [cells]
  (into #{} (mapv first cells)))

(declare sensory-motor-region)

(defrecord SensoryMotorRegion
    [layer-4 layer-3 uuid step-counter]
  ;; TODO
)

(defrecord SensoryMotorInput
    [encoder motor-encoder value]
  p/PTopological
  (topology [_]
    ;; TODO combine dimensions with motor component?
    (p/topology encoder))
  p/PFeedForward
  (ff-topology [_]
    (p/topology encoder))
  (bits-value
    [_ offset]
    (p/encode encoder offset value))
  (signal-bits-value
    [_ offset]
    #{})
  (source-of-bit
    [_ i]
    [i])
  p/PFeedForwardMotor
  (ff-motor-topology [_]
    (if motor-encoder
      (p/topology motor-encoder)
      topology/empty-topology))
  (motor-bits-value
    [_ offset]
    (if motor-encoder
      (p/encode motor-encoder offset value)
      #{}))
  p/PInputSource
  (input-step [this in-value]
    (assoc this :value in-value)))

(defn sensory-input
  "Creates an input source from an encoder."
  [encoder]
  (->SensoryMotorInput encoder nil nil))

(defn sensory-motor-input
  "Creates an input source from an encoder (for the proximal
   feed-forward output) and a motor encoder (for the distal
   feed-forward output). The encoders operate on the same value so
   should select their relevant parts of it. Remember that HTM models
   go through the three phases [activate -> learn -> depolarise] on
   each timestep: therefore motor signals, which act to depolarise
   cells, should appear the time step before a corresponding sensory
   signal."
  [encoder motor-encoder]
  (->SensoryMotorInput encoder motor-encoder nil))

(defn combined-bits-value
  "Returns the total bit set from a collection of sources satisfying
   `PFeedForward` or `PFeedForwardMotor`. `flavour` should
   be :standard, :signal or :motor."
  [ffs flavour]
  (let [topo-fn (case flavour
                  (:standard
                   :signal) p/ff-topology
                   :motor p/ff-motor-topology)
        bits-fn (case flavour
                  :standard p/bits-value
                  :signal p/signal-bits-value
                  :motor p/motor-bits-value)
        widths (map (comp p/size topo-fn) ffs)
        offs (list* 0 (reductions + widths))]
    (->> (map bits-fn ffs offs)
         (apply set/union))))

;;; ## Region Networks

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

#+cljs (def pmap map)

(defrecord RegionNetwork
    [ff-deps-map fb-deps-map strata inputs-map regions-map uuid->id]
  p/PHTM
  (htm-activate
    [this in-value]
    (let [im (zipmap (keys inputs-map)
                     (map p/input-step (vals inputs-map) (repeat in-value)))
          rm (-> (reduce
                  (fn [m stratum]
                    (->> stratum
                         (pmap (fn [id]
                                 (let [region (regions-map id)
                                       ff-ids (ff-deps-map id)
                                       ffs (map m ff-ids)]
                                   (p/region-activate
                                    region
                                    (combined-bits-value ffs :standard)
                                    (combined-bits-value ffs :signal)))))
                         (zipmap stratum)
                         (into m)))
                  im
                  ;; drop 1st stratum i.e. drop the inputs
                  (rest strata))
                 ;; get rid of the inputs which were seeded into the reduce
                 (select-keys (keys regions-map)))]
      (assoc this :inputs-map im :regions-map rm)))

  (htm-learn
    [this]
    (let [rm (->> (vals regions-map)
                  (pmap p/region-learn)
                  (zipmap (keys regions-map)))]
      (assoc this :regions-map rm)))

  (htm-depolarise
    [this]
    (let [rm (->> regions-map
                  (pmap (fn [[id region]]
                          (let [ff-ids (ff-deps-map id)
                                fb-ids (fb-deps-map id)
                                ffs (map #(or (inputs-map %) (regions-map %))
                                         ff-ids)
                                fbs (map regions-map fb-ids)]
                            (p/region-depolarise
                             region
                             (combined-bits-value ffs :motor)
                             (combined-bits-value fbs :standard)))))
                  (zipmap (keys regions-map)))]
      (assoc this :regions-map rm)))
  
  (region-seq [_]
    ;; topological sort. drop 1st stratum i.e. drop the inputs
    (map regions-map (apply concat (rest strata))))
  
  (input-seq [_]
    (vals inputs-map))
  
  (update-by-uuid
    [this region-uuid f]
    (update-in this [:regions-map (or (uuid->id region-uuid) region-uuid)]
               f))

  p/PTemporal
  (timestep [_]
    (p/timestep (first (vals regions-map))))
  p/PResettable
  (reset [this]
    (assoc this
      :regions-map (->> (vals regions-map)
                        (pmap p/reset)
                        (zipmap (keys regions-map))))))

(defn- in-vals-not-keys
  [deps-map]
  (let [have-deps (set (keys deps-map))
        are-deps (set (apply concat (vals deps-map)))]
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
  [ff-deps-map inputs-map build-region region-specs-map]
  {:pre [;; anything with a dependency must be a region
         (every? ff-deps-map (keys region-specs-map))
         ;; anything without a dependency must be an input
         (every? (in-vals-not-keys ff-deps-map) (keys inputs-map))
         ;; all ids in dependency map must be defined
         (every? region-specs-map (keys ff-deps-map))
         (every? inputs-map (in-vals-not-keys ff-deps-map))]}
  (let [all-ids (into (set (keys ff-deps-map))
                      (in-vals-not-keys ff-deps-map))
        ff-dag (graph/directed-graph all-ids ff-deps-map)
        strata (graph/dependency-list ff-dag)
        fb-deps-map (->> (graph/reverse-graph ff-dag)
                         :neighbors
                         (util/remap seq))
        rm (-> (reduce (fn [m id]
                         (let [spec (region-specs-map id)
                               ;; feed-forward
                               ff-ids (ff-deps-map id)
                               ffs (map m ff-ids)
                               ff-dim (topo-union (map p/ff-topology ffs))
                               ffm-dim (topo-union  (map p/ff-motor-topology ffs))
                               ;; top-down feedback (if any)
                               fb-ids (fb-deps-map id)
                               fb-specs (map region-specs-map fb-ids)
                               fb-dim (topo-union (map fb-dim-from-spec fb-specs))]
                           (->> (assoc spec :input-dimensions ff-dim
                                       :distal-motor-dimensions ffm-dim
                                       :distal-topdown-dimensions fb-dim)
                                (build-region)
                                (assoc m id))))
                       inputs-map
                       ;; topological sort. drop 1st stratum i.e. drop the inputs
                       (apply concat (rest strata)))
               ;; get rid of the inputs which were seeded into the reduce
               (select-keys (keys region-specs-map)))]
    (map->RegionNetwork
     {:ff-deps-map ff-deps-map
      :fb-deps-map fb-deps-map
      :strata strata
      :inputs-map inputs-map
      :regions-map rm
      :uuid->id (zipmap (map :uuid (vals rm)) (keys rm))})))

(defn regions-in-series
  "Constructs an HTM network consisting of one input and n regions in
   a linear series. See `region-network`."
  [build-region input n spec]
  (let [rgn-keys (map #(keyword (str "r" %)) (range n))
        ;; make {:r0 [:input], :r1 [:r0], :r2 [:r1], ...}
        deps (zipmap rgn-keys (map vector (list* :input rgn-keys)))]
    (region-network deps
                    {:input input}
                    build-region
                    (zipmap rgn-keys (repeat spec)))))

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

(defn predicted-bit-votes
  "Returns a map from input bit index to the number of connections to
   it from columns in the predictive state. `p-cols` is the column ids
   of columns containing predictive cells."
  ([rgn p-cols]
     (predicted-bit-votes rgn p-cols :layer-3))
  ([rgn p-cols layer-fn]
     (let [lyr (layer-fn rgn)
           sg (:proximal-sg lyr)]
       (->> p-cols
            (reduce (fn [m col]
                      (let [ids (p/sources-connected-to sg col)]
                        (reduce (fn [m id]
                                  (assoc! m id (inc (get m id 0))))
                                m ids)))
                    (transient {}))
            (persistent!)))))

(defn predictions
  [model n-predictions]
  (let [rgn (first (p/region-seq model))
        inp (first (p/input-seq model))
        pr-cols (->> (p/predictive-cells (:layer-3 rgn))
                     (map first))
        pr-votes (predicted-bit-votes rgn pr-cols)]
    (p/decode (:encoder inp) pr-votes n-predictions)))
