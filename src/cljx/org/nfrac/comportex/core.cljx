(ns org.nfrac.comportex.core
  "A _region_ is the main composable unit in this library. It
   represents a field of neurons arranged in columns, responding to
   an input bit array."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.pooling :as pooling]
            [org.nfrac.comportex.sequence-memory :as sm]
            [org.nfrac.comportex.encoders :as enc]
            [cljs-uuid.core :as uuid]
            [clojure.set :as set]
            [clojure.zip :as zip]))

(declare sensory-region)

(defrecord SensoryRegion
    [column-field layer uuid step-counter]
  p/PRegion
  (region-step*
    [this ff-bits signal-ff-bits extra-distal learn?]
    (let [next-cf (p/columns-step column-field ff-bits signal-ff-bits
                                  (p/depolarisation layer) learn?)
          next-layer (p/layer-step layer (p/active-columns next-cf)
                                   (p/temporal-pooling-columns next-cf)
                                   extra-distal learn?)]
      (assoc this
        :column-field next-cf
        :layer next-layer
        :step-counter (inc step-counter))))
  (ff-cells-per-column [_]
    (p/layer-depth layer))
  (ff-active-cells [_]
    (p/active-cells layer))
  (ff-signal-cells [_]
    (p/signal-cells layer))
  p/PTopological
  (topology [_]
    (p/topology column-field))
  p/PTemporal
  (timestep [_]
    step-counter)
  p/PParameterised
  (params [_]
    (merge (p/params column-field)
           (p/params layer)))
  p/PResettable
  (reset [this]
    (-> (sensory-region (p/params this))
        (assoc :uuid uuid))))

(defn sensory-region
  "Constructs a cortical region with the given specification map. See
   documentation on parameter defaults in the `pooling` and
   `sequence-memory` namespaces for possible keys. Any keys given here
   will override those default values."
  [spec]
  (map->SensoryRegion
   {:column-field (pooling/column-field spec)
    :layer (sm/layer-of-cells spec)
    :uuid (uuid/make-random)
    :step-counter 0}))

(defrecord SensoryInput
    [init-value value transform encoder]
  p/PFeedForward
  (bit-width [_]
    (enc/encoder-bit-width encoder))
  (bits-value* [_ offset]
    (enc/encode encoder offset value))
  (signal-bits-value* [_ offset]
    #{})
  (source-of-bit [_ i]
    [i])
  (incoming-bits-value [this]
    (p/bits-value* this 0))
  (source-of-incoming-bit [this i]
    [0 (p/source-of-bit this i)])
  (feed-forward-step* [this _]
    (assoc this :value (transform value)))
  p/PSensoryInput
  (domain-value [_] value)
  (state-labels [_] #{})
  p/PResettable
  (reset [this]
    (assoc this :value init-value)))

(defn sensory-input
  "Creates an input stream from an initial value, a function to
   transform the input to the next time step, and an encoder."
  [init-value transform encoder]
  (->SensoryInput init-value init-value transform encoder))

(defn combined-bit-width
  "Returns the total bit width from a collection of
   sources (satisfying PFeedForward)."
  [ffs]
  (reduce + (map p/bit-width ffs)))

(defn combined-bits-value
  "Returns the total bit set from a collection of sources (satisfying
   PFeedForward). `bits-fn` should be `p/bits-value*` or
   `p/signal-bits-value*`."
  [ffs bits-fn]
  (let [ws (map p/bit-width ffs)
        os (list* 0 (reductions + ws))]
    (->> (map bits-fn ffs os)
         (apply set/union))))

(defn cell-id->inbit
  [offset depth [col ci]]
  (+ offset (* depth col) ci))

(defn inbit->cell-id
  [depth i]
  [(quot i depth)
   (rem i depth)])

(defrecord RegionTree
    [region subs]
  p/PFeedForward
  (bit-width [_]
    (* (p/ff-cells-per-column region)
       (p/size (p/topology region))))
  (bits-value*
    [_ offset]
    (let [depth (p/ff-cells-per-column region)]
      (->> (p/ff-active-cells region)
           (mapv (partial cell-id->inbit offset depth))
           (into #{}))))
  (signal-bits-value*
    [_ offset]
    (let [depth (p/ff-cells-per-column region)]
      (->> (p/ff-signal-cells region)
           (mapv (partial cell-id->inbit offset depth))
           (into #{}))))
  (source-of-bit
    [_ i]
    (let [depth (p/ff-cells-per-column region)]
      (inbit->cell-id depth i)))
  (incoming-bits-value [_]
    (combined-bits-value subs p/bits-value*))
  (source-of-incoming-bit
    [_ i]
    (loop [sub-i 0
           offset 0]
      (when (< sub-i (count subs))
        (let [sub (nth subs sub-i)
              w (p/bit-width sub)]
          (if (<= offset i (+ offset w -1))
            [sub-i (p/source-of-bit sub (- i offset))]
            (recur (inc sub-i) (+ offset (long w))))))))
  (feed-forward-step*
    [this learn?]
    (let [new-subs (map #(p/feed-forward-step* % learn?) subs)
          new-rgn (p/region-step region
                                 (combined-bits-value new-subs p/bits-value*)
                                 (combined-bits-value new-subs p/signal-bits-value*)
                                 #{} ;; TODO extra-distal
                                 learn?)]
      (assoc this :region new-rgn :subs new-subs)))
  p/PTemporal
  (timestep [_]
    (p/timestep region))
  p/PResettable
  (reset [this]
    (-> this
        (update-in [:subs] #(map p/reset %))
        (update-in [:region] p/reset))))

(defn region-tree
  [rgn subs]
  (->RegionTree rgn subs))

(defn tree
  "A helper function to build a hierarchical network. The `subs`
   should be a sequence of subtrees or input generators. The combined
   bit width from these is calculated and used to set the
   `:input-dimensions` parameter in `spec`. If the parameter
   `:ff-potential-radius-frac` is defined it is used to calculate (and
   override) the `:ff-potential-radius` parameter as a fraction of the
   input size. The updated spec is passed to `build-region`. Returns a
   RegionTree."
  [build-region spec subs]
  (let [width (combined-bit-width subs)
        radius (if-let [x (:ff-potential-radius-frac spec)]
                 (long (* x width))
                 (:ff-potential-radius spec))]
    (-> (assoc spec :input-dimensions [width]
               :ff-potential-radius radius)
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

(defn region-tree-seq
  "A sequence of the sub region trees in a region tree (including
  itself). The order is the same as `region-seq`."
  [tree]
  (->> (tree-seq :subs :subs tree)
       (filter :region)
       (reverse) ;; put in bottom to top order, for first path down tree
       (vec)))

(defn region-seq
  "A sequence of the regions in a region tree. The order is the same
   as `region-tree-seq`."
  [tree]
  (map :region (region-tree-seq tree)))

(defn inputs-seq
  "A seq of the input generators in a region tree."
  [tree]
  (->> (tree-seq :subs :subs tree)
       (remove :region)))

(defn region-tree-zipper
  [tree]
  (zip/zipper :subs :subs (fn [x cs] (assoc x :subs cs)) tree))

(defn update-by-uuid
  "Applies function `f` to the region in `tree` identified by its
   UUID. Returns the modified region tree."
  [tree region-uuid f]
  (loop [loc (region-tree-zipper tree)]
    (if-let [rgn (:region (zip/node loc))]
      (if (= region-uuid (:uuid rgn))
        (-> (zip/edit loc #(update-in % [:region] f))
            (zip/root))
        (if (zip/end? loc)
          ::no-matching-UUID!
          (recur (zip/next loc)))))))

(defn column-state-freqs
  "Returns a map with the frequencies of columns in states `:active`,
  `:predicted`, `:active-predicted`."
  [rgn]
  (let [cf (:column-field rgn)
        lyr (:layer rgn)
        a-cols (p/active-columns cf)
        ppc (p/prior-predictive-cells lyr)
        pp-cols (into #{} (map first ppc))
        hit-cols (set/intersection pp-cols a-cols)
        col-states (merge (zipmap pp-cols (repeat :predicted))
                          (zipmap a-cols (repeat :active))
                          (zipmap hit-cols (repeat :active-predicted)))]
    (-> {:active 0, :predicted 0, :active-predicted 0}
        (merge (frequencies (vals col-states)))
        (assoc :timestep (p/timestep rgn)
               :size (p/size (p/topology rgn))))))

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
