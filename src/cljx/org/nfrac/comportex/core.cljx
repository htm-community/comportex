(ns org.nfrac.comportex.core
  "A _region_ is the main composable unit in this library. It
   represents a bank of neurons arranged in columns, responding to an
   array of feed-forward input bits, as well as distal connections to
   itself and possibly other regions."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.columns :as columns]
            [org.nfrac.comportex.cells :as cells]
            [cljs-uuid.core :as uuid]
            [clojure.set :as set]
            [clojure.zip :as zip]))

(defn cell-id->inbit
  [offset depth [col ci]]
  (+ offset (* depth col) ci))

(defn inbit->cell-id
  [depth i]
  [(quot i depth)
   (rem i depth)])

(declare sensory-region)

(defrecord SensoryRegion
    [column-field layer-3 uuid step-counter]
  p/PRegion
  (region-activate
    [this ff-bits signal-ff-bits]
    (let [step-cf (p/columns-step column-field ff-bits signal-ff-bits)
          lyr (p/layer-activate layer-3
                                (p/column-excitation step-cf)
                                (p/column-signal-overlaps step-cf)
                                (p/inhibition-radius step-cf))]
      (assoc this
        :step-counter (inc step-counter)
        :column-field step-cf
        :layer-3 lyr)))

  (region-learn
    [this ff-bits]
    (if (:freeze? (p/params this))
      this
      (let [cf (p/columns-learn column-field ff-bits (p/active-columns layer-3))
            lyr (p/layer-learn layer-3)]
        (assoc this
          :column-field cf
          :layer-3 lyr))))

  (region-depolarise
    [this distal-bits]
    (assoc this
      :layer-3 (p/layer-depolarise layer-3 distal-bits)))
  
  p/PTopological
  (topology [_]
    (p/topology column-field))
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
    (merge (p/params column-field)
           (p/params layer-3)))
  p/PResettable
  (reset [this]
    (-> (sensory-region (p/params this))
        (assoc :uuid uuid))))

(defn sensory-region
  "Constructs a cortical region with the given specification map. See
   documentation on parameter defaults in the `columns` and `cells`
   namespaces for possible keys. Any keys given here will override
   those default values."
  [spec]
  (map->SensoryRegion
   {:column-field (columns/column-field spec)
    :layer-3 (cells/layer-of-cells spec)
    :uuid (uuid/make-random)
    :step-counter 0}))

(defn cells->cols
  [cells]
  (into #{} (mapv first cells)))

(declare sensory-motor-region)

(defrecord SensoryMotorRegion
    [column-field layer-4 layer-3 uuid step-counter]
  ;; TODO
)

(defn sensory-motor-region
  "Constructs a cortical region with the given specification map. See
   documentation on parameter defaults in the `columns` and `cells`
   namespaces for possible keys. Any keys given here will override
   those default values."
  [spec]
  (map->SensoryMotorRegion
   {:column-field (columns/column-field spec)
    :layer-4 (cells/layer-of-cells (assoc spec
                                     :motor-distal-size :TODO
                                     :lateral-synapses? false))
    :layer-3 (cells/layer-of-cells (assoc spec
                                     :motor-distal-size 0
                                     :lateral-synapses? true))
    :uuid (uuid/make-random)
    :step-counter 0}))

(defrecord SensoryInput
    [init-value value transform encoder]
  p/PTopological
  (topology [_]
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
    topology/empty-topology)
  (motor-bits-value
    [_ offset]
    #{})
  p/PSensoryInput
  (input-step [this]
    (assoc this :value (transform value)))
  (domain-value [_]
    value)
  p/PResettable
  (reset [this]
    (assoc this :value init-value)))

(defn sensory-input
  "Creates an input stream from an initial value, a function to
   transform the input to the next time step, and an encoder."
  [init-value transform encoder]
  (->SensoryInput init-value init-value transform encoder))

(defrecord SensoryMotorInput
    [init-value value transform encoder motor-encoder]
  p/PTopological
  (topology [_]
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
    (p/topology motor-encoder))
  (motor-bits-value
    [_ offset]
    (p/encode motor-encoder offset value))
  p/PSensoryInput
  (input-step [this]
    (assoc this :value (transform value)))
  (domain-value [_]
    value)
  p/PResettable
  (reset [this]
    (assoc this :value init-value)))

(defn sensory-motor-input
  "Creates an input stream from an initial value, a function to
   transform the input to the next time step, and two encoders
   operating on the same value."
  [init-value transform encoder motor-encoder]
  (->SensoryMotorInput init-value init-value transform encoder motor-encoder))

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

;;; ## Region trees

(def is-input? #(satisfies? p/PSensoryInput %))

(defn- ->ffs
  [subs]
  (map (fn [x] (if (is-input? x) x (:region x))) subs))

(defn region-tree-zipper
  [tree]
  (zip/zipper :subs :subs (fn [x cs] (assoc x :subs cs)) tree))

(defn region-tree-seq
  "A sequence of the sub region trees in a region tree (including
  itself). The order is the same as `region-seq`."
  [tree]
  (->> (tree-seq :subs :subs tree)
       (filter :region)
       (reverse) ;; put in bottom to top order, for first path down tree
       (vec)))

(defrecord RegionTree
    [region subs]
  p/PHTM
  (htm-activate
    [this]
    (let [new-subs (map (fn [x]
                          (if (is-input? x)
                            (p/input-step x)
                            (p/htm-activate x)))
                        subs)
          new-rgn (p/region-activate region
                                     (combined-bits-value (->ffs new-subs) :standard)
                                     (combined-bits-value (->ffs new-subs) :signal))]
      (assoc this :region new-rgn :subs new-subs)))
  (htm-learn
    [this]
    (let [new-subs (map (fn [x] (if (is-input? x) x (p/htm-learn x)))
                        subs)
          new-rgn (p/region-learn region
                                  (combined-bits-value (->ffs new-subs) :standard))]
      (assoc this :region new-rgn :subs new-subs)))
  (htm-depolarise
    [this]
    (let [new-subs (map (fn [x] (if (is-input? x) x (p/htm-depolarise x)))
                        subs)
          new-rgn (p/region-depolarise region
                                       (combined-bits-value (->ffs new-subs) :motor))]
      (assoc this :region new-rgn :subs new-subs)))
  (region-seq [this]
    (map :region (region-tree-seq this)))
  (input-seq [this]
    (->> (tree-seq :subs :subs this)
         (remove :region)))
  (update-by-uuid
    [this region-uuid f]
    (loop [loc (region-tree-zipper this)]
      (if-let [rgn (:region (zip/node loc))]
        (if (= region-uuid (:uuid rgn))
          (-> (zip/edit loc #(update-in % [:region] f))
              (zip/root))
          (if (zip/end? loc)
            ::no-matching-UUID!
            (recur (zip/next loc)))))))
  p/PBitsAggregated
  (source-of-incoming-bit
    [this i]
    (loop [sub-i 0
           offset 0]
      (when (< sub-i (count subs))
        (let [sub (nth subs sub-i)
              w (p/size-of sub)]
          (if (<= offset i (+ offset w -1))
            [sub-i (p/source-of-bit sub (- i offset))]
            (recur (inc sub-i) (+ offset (long w))))))))
  p/PTemporal
  (timestep [_]
    (p/timestep region))
  p/PResettable
  (reset [this]
    (assoc this
      :region (p/reset region)
      :subs (map p/reset subs))))

(defn region-tree
  [rgn subs]
  (->RegionTree rgn subs))

(defn tree
  "A helper function to build a hierarchical network. The `subs`
   should be a sequence of subtrees or sensory inputs (things
   satisfying PFeedForward and PTopological). The combined dimensions
   of these is calculated and used to set the `:input-dimensions`
   parameter in `spec`. The updated spec is passed to `build-region`.
   Returns a RegionTree."
  [build-region spec subs]
  (let [ffs (->ffs subs)
        dims (apply topology/combined-dimensions
                    (map (comp p/dimensions p/ff-topology) ffs))
        mdims (apply topology/combined-dimensions
                     (map (comp p/dimensions p/ff-motor-topology) ffs))]
    (-> (assoc spec :input-dimensions dims
               :extra-distal-size (apply * mdims))
        (build-region)
        (region-tree subs))))

(defn regions-in-series
  [build-region input n spec]
  (->> input
       (iterate (fn [sub-model]
                  (tree build-region spec
                        [sub-model])))
       (take (inc n))
       (last)))

;;; ## Stats

(defn column-state-freqs
  "Returns a map with the frequencies of columns in states `:active`,
  `:predicted`, `:active-predicted`."
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
  [rgn p-cols]
  (let [cf (:column-field rgn)
        sg (:ff-sg cf)]
    (->> p-cols
         (reduce (fn [m col]
                   (let [ids (p/sources-connected-to sg col)]
                     (reduce (fn [m id]
                               (assoc! m id (inc (get m id 0))))
                             m ids)))
                 (transient {}))
         (persistent!))))

(defn predictions
  [model n-predictions]
  (let [rgn (first (p/region-seq model))
        inp (first (p/input-seq model))
        pr-cols (->> (p/predictive-cells (:layer-3 rgn))
                     (map first))
        pr-votes (predicted-bit-votes rgn pr-cols)]
    (p/decode (:encoder inp) pr-votes n-predictions)))
