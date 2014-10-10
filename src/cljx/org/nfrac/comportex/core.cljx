(ns org.nfrac.comportex.core
  "A _region_ is the main composable unit in this library. It
   represents a field of neurons arranged in columns, responding to
   an input bit array."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.columns :as columns]
            [org.nfrac.comportex.cells :as cells]
            [cljs-uuid.core :as uuid]
            [clojure.set :as set]
            [clojure.zip :as zip]))

;; default implementation of PMotorTopological
(extend-type
    #+cljs object, #+clj java.lang.Object
    p/PMotorTopological
    (motor-topology [_]
      (topology/make-topology [0])))

(declare sensory-region)

(defrecord SensoryRegion
    [column-field layer-3 uuid step-counter]
  p/PRegion
  (region-step*
    [this ff-bits signal-ff-bits distal-bits learn?]
    (let [prior-ac (p/active-cells layer-3)
          prior-lc (p/learnable-cells layer-3)
          step-cf (p/columns-step column-field ff-bits signal-ff-bits)
          next-lyr (cond-> (p/layer-step layer-3
                                         (p/column-overlaps step-cf)
                                         (p/column-signal-overlaps step-cf)
                                         (p/inhibition-radius step-cf))
                           learn? (p/layer-learn prior-ac prior-lc)
                           true (p/layer-depolarise distal-bits))
          next-cf (cond-> step-cf
                          learn? (p/columns-learn ff-bits signal-ff-bits
                                                  (p/active-columns next-lyr)))]
      (assoc this
        :column-field next-cf
        :layer-3 next-lyr
        :step-counter (inc step-counter))))
  (ff-cells-per-column [_]
    (p/layer-depth layer-3))
  (ff-active-cells [_]
    (p/active-cells layer-3))
  (ff-signal-cells [_]
    (p/signal-cells layer-3))
  p/PTopological
  (topology [_]
    (p/topology column-field))
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

(declare sensory-motor-region)

(defn cells->cols
  [cells]
  (into #{} (mapv first cells)))

(defrecord SensoryMotorRegion
    [column-field layer-4 layer-3 uuid step-counter]
  p/PRegion
  (region-step*
    [this ff-bits signal-ff-bits distal-bits learn?]
    (let [prior-ac (p/active-cells layer-3)
          prior-lc (p/learnable-cells layer-3)
          step-cf (p/columns-step column-field ff-bits signal-ff-bits)
          next-l4 (cond-> (p/layer-step layer-4
                                        (p/column-overlaps step-cf)
                                        (p/column-signal-overlaps step-cf)
                                        (p/inhibition-radius step-cf))
                          learn? (p/layer-learn #{} #{}) ;; TODO ??
                          true (p/layer-depolarise distal-bits)) ;; ??
          next-cf (cond-> step-cf
                          learn? (p/columns-learn ff-bits signal-ff-bits
                                                  ;; TODO combine with layer 3?
                                                  (p/active-columns next-l4)))
          ;; TODO TODO TODO
          ;; add L4 TP cell activation to proximal inputs
          ]
      (assoc this
        :column-field next-cf
        :layer-4 next-l4
        :layer-3 layer-3
        :step-counter (inc step-counter))))
  (ff-cells-per-column [_]
    (p/layer-depth layer-3))
  (ff-active-cells [_]
    (p/active-cells layer-3))
  (ff-signal-cells [_]
    (p/signal-cells layer-3))
  p/PTopological
  (topology [_]
    (p/topology column-field))
  p/PTemporal
  (timestep [_]
    step-counter)
  p/PParameterised
  (params [_]
    (merge (p/params column-field)
           (p/params layer-3)
           (-> (p/params layer-4)
               (select-keys [:motor-distal-size]))))
  p/PResettable
  (reset [this]
    (-> (sensory-motor-region (p/params this))
        (assoc :uuid uuid))))

(defn sensory-motor-region
  "Constructs a cortical region with the given specification map. See
   documentation on parameter defaults in the `columns` and `cells`
   namespaces for possible keys. Any keys given here will override
   those default values."
  [spec]
  (map->SensoryMotorRegion
   {:column-field (columns/column-field spec)
    :layer-4 (cells/layer-of-cells (assoc spec
                                     :motor-distal-size ()
                                     :lateral-synapses? false))
    :layer-3 (cells/layer-of-cells (assoc spec
                                     :motor-distal-size 0
                                     :lateral-synapses? true))
    :uuid (uuid/make-random)
    :step-counter 0}))

(defrecord SensoryInput
    [init-value value transform encoder]
  p/PFeedForward
  (bits-value*
    [_ offset]
    (p/encode encoder offset value))
  (signal-bits-value*
    [_ offset]
    #{})
  (feed-forward-step* [this _]
    (assoc this :value (transform value)))
  (motor-bits-value*
    [this offset]
    #{})
  p/PTopological
  (topology [_]
    (p/topology encoder))
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

(defrecord SensoryMotorInput
    [init-value value transform encoder motor-encoder]
  p/PFeedForward
  (bits-value*
    [_ offset]
    (p/encode encoder offset value))
  (signal-bits-value*
    [_ offset]
    #{})
  (motor-bits-value*
    [_ offset]
    (p/encode motor-encoder offset value))
  (feed-forward-step* [this _]
    (assoc this :value (transform value)))
  p/PTopological
  (topology [_]
    (p/topology encoder))
  p/PMotorTopological
  (motor-topology [_]
    (p/topology motor-encoder))
  p/PSensoryInput
  (domain-value [_] value)
  (state-labels [_] #{})
  p/PResettable
  (reset [this]
    (assoc this :value init-value)))

(defn sensory-motor-input
  "Creates an input stream from an initial value, a function to
   transform the input to the next time step, and an encoder."
  [init-value transform encoder motor-encoder]
  (->SensoryMotorInput init-value init-value transform encoder motor-encoder))

(defn combined-bits-value
  "Returns the total bit set from a collection of sources (satisfying
   PFeedForward and PTopological). `bits-fn` should be `p/bits-value`
   or `p/signal-bits-value`."
  [ffs bits-fn]
  (let [ws (map p/size-of ffs)
        os (list* 0 (reductions + ws))]
    (->> (map bits-fn ffs os)
         (apply set/union))))

(defn combined-motor-bits-value
  "Returns the total bit set from a collection of sources (satisfying
   PFeedForward and PMotorTopological)."
  [ffs]
  (let [ws (map (comp p/size p/motor-topology) ffs)
        os (list* 0 (reductions + ws))]
    (->> (map p/motor-bits-value* ffs os)
         (apply set/union))))

(defn cell-id->inbit
  [offset depth [col ci]]
  (+ offset (* depth col) ci))

(defn inbit->cell-id
  [depth i]
  [(quot i depth)
   (rem i depth)])

(defn incoming-bits-value
  [rgn-tree bits-fn]
  (combined-bits-value (:subs rgn-tree) bits-fn))

(defrecord RegionTree
    [region subs]
  p/PFeedForward
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
  (motor-bits-value*
    [_ offset]
    ;; TODO
    #{})
  (feed-forward-step*
    [this learn?]
    (let [new-subs (map #(p/feed-forward-step % learn?) subs)
          new-rgn (p/region-step region
                                 (combined-bits-value new-subs p/bits-value)
                                 (combined-bits-value new-subs p/signal-bits-value)
                                 (combined-motor-bits-value new-subs)
                                 learn?)]
      (assoc this :region new-rgn :subs new-subs)))
  p/PFeedForwardComposite
  (source-of-bit
    [_ i]
    (let [depth (p/ff-cells-per-column region)]
      (inbit->cell-id depth i)))
  (source-of-incoming-bit
    [_ i]
    (loop [sub-i 0
           offset 0]
      (when (< sub-i (count subs))
        (let [sub (nth subs sub-i)
              w (p/size-of sub)]
          (if (<= offset i (+ offset w -1))
            [sub-i (p/source-of-bit sub (- i offset))]
            (recur (inc sub-i) (+ offset (long w))))))))
  p/PTopological
  (topology [_]
    (topology/make-topology (conj (p/dims-of region)
                                  (p/ff-cells-per-column region))))
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
   should be a sequence of subtrees or sensory inputs (things
   satisfying PFeedForward and PTopological). The combined dimensions
   of these is calculated and used to set the `:input-dimensions`
   parameter in `spec`. If the parameter `:ff-potential-radius-frac`
   is defined it is used to calculate (and override) the
   `:ff-potential-radius` parameter as a fraction of the largest
   single dimension. The updated spec is passed to `build-region`.
   Returns a RegionTree."
  [build-region spec subs]
  (let [dims (apply topology/combined-dimensions (map p/dims-of subs))
        mdims (apply topology/combined-dimensions (map p/motor-dims-of subs))
        radius (if-let [x (:ff-potential-radius-frac spec)]
                 (long (* x (apply max dims)))
                 (:ff-potential-radius spec))]
    (-> (assoc spec :input-dimensions dims
               :extra-distal-size (apply * mdims)
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
