(ns org.nfrac.comportex.layer
  "Implements a single HTM-style layer.

   **Argument name conventions:**

   * `col` -- a column id, an integer index in the layer.
   * `ci` -- a cell id, an integer index in the column.
   * `si` -- a segment id, an integer index in the cell.
   * `cell-id` -- a vector `[col ci]`.
   * `seg-path` -- a vector `[col ci si]`.
   * `exc` - excitation amount, based on number of active synapses, a non-negative number.
   * `fully-matching-segs` -- maps cell-id to [seg-path exc], meeting stimulus-threshold.
   * `matching-segs` -- same but includes partial matches, meeting learn-threshold.
   * `pcon` -- the connected threshold of synaptic permanence.
   * `ff-bits` -- the set of indices of active bits/cells on proximal dendrites.
   * `ac` -- the set of ids of active cells.
   * `pc` -- the set of ids of predictive cells.
   * `tp` -- temporal pooling.
   * `lc` -- the set of ids of learning cells.
   * `a-cols` -- the set of ids of active columns.
   * `seg` or `syns` -- incoming synapses as a map from source id to permanence.
"
  (:require [org.nfrac.comportex.layer.params :as params]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.homeostasis :as homeo]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topography :as topo]
            [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.util :as util
             :refer [count-filter remap round spec-finite]]
            [clojure.test.check.random :as random]
            [clojure.set :as set]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]))

;;; ## Specs

(s/def ::column-id (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 2048)))))
(s/def ::cell-index (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 32)))))
(s/def ::cell-id (s/tuple ::column-id ::cell-index))
(s/def ::seg-path (s/tuple ::column-id ::cell-index ::cell-index))

(s/def ::seg-exc (s/every-kv ::seg-path ::syn/excitation-amt))

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


(s/def ::active-cols ::active-columns)
(s/def ::burst-cols ::active-cols)
(s/def ::best-seg (s/cat :seg ::seg-path, :exc ::syn/excitation-amt))

(s/def ::in-ff-signal ::cx/signal)
(s/def ::stable-active-cells ::active-cells)
(s/def ::col-active-cells (s/map-of ::column-id (s/coll-of ::cell-id)))
(s/def ::fully-matching-ff-segs (s/every-kv ::cell-id ::best-seg))
(s/def ::col-overlaps (s/every-kv (s/tuple ::column-id #{0}) ::syn/excitation-amt))

(s/def ::active-state
  #_"Represents the activation of cells in a layer; that state which is volatile
  across time steps."
  (s/keys :req-un [::fully-matching-ff-segs
                   ::active-cols
                   ::active-cells
                   ::cx/timestep]
          :opt-un [::in-ff-signal
                   ::burst-cols
                   ::col-active-cells
                   ::stable-active-cells
                   ::col-overlaps]))

(s/def ::out-stable-ff-bits ::cx/bits)

(s/def ::tp-state
  #_"Represents temporal pooling state; the information about activation of
  cells which persists across many time steps."
  (s/keys :opt-un [::out-stable-ff-bits]))

(s/def ::col-winners (s/map-of ::column-id ::cell-id))
(s/def ::winner-seg (s/map-of #{:distal :apical} ::matching-segs))
(s/def ::learning-cells (s/nilable (s/every ::cell-id)))
(s/def ::learning-updates (s/map-of ::cell-id ::syn/seg-update))
(s/def ::learning (s/map-of #{:proximal :distal :apical :ilateral}
                            ::learning-updates))
(s/def ::punishments (s/map-of #{:proximal :distal :apical :ilateral}
                               (s/coll-of ::syn/seg-update)))
(s/def ::prior-active-cells ::active-cells)

(s/def ::learn-state
  #_"Represents the changes due to learning in one time step, and information
  used in the learning process."
  (s/keys :req-un [::prior-active-cells
                   ::cx/timestep]
          :opt-un [::col-winners
                   ::winner-seg
                   ::learning-cells
                   ::learning
                   ::punishments]))

(s/def ::active-bits ::cx/bits-set)
(s/def ::learnable-bits ::cx/bits-set)
(s/def ::cell-exc (s/map-of ::cell-id ::syn/excitation-amt))
(s/def ::pred-cells (s/every ::cell-id :kind set?))
(s/def ::fully-matching-segs (s/map-of ::cell-id ::best-seg))
(s/def ::matching-segs (s/map-of ::cell-id (s/nilable ::best-seg)))
(s/def ::distal-state
  #_"Represents the activation state of a synapse graph, e.g. the distal
  segments, or apical segments, of cells in one layer."
  (s/keys :req-un [::active-bits
                   ::learnable-bits
                   ::cell-exc
                   ::pred-cells
                   ::fully-matching-segs
                   ::cx/timestep]))

(s/def ::proximal-sg ::syn/synapse-graph)
(s/def ::distal-sg ::syn/synapse-graph)
(s/def ::apical-sg ::syn/synapse-graph)
(s/def ::ilateral-sg ::syn/synapse-graph)
(s/def ::prior-active-state ::active-state)
(s/def ::prior-distal-state ::distal-state)
(s/def ::apical-state ::distal-state)
(s/def ::prior-apical-state ::distal-state)

(declare layer-of-cells)

(s/def ::layer-of-cells-unembedded
  (->
   (s/keys :req-un [::params/params
                    ::util/rng
                    ::topography
                    ::active-state
                    ::prior-active-state
                    ::tp-state
                    ::distal-state
                    ::prior-distal-state
                    ::apical-state
                    ::prior-apical-state
                    ::learn-state
                    ::inh-radius
                    ::boosts
                    ::active-duty-cycles
                    ::overlap-duty-cycles])
   ;; this only generates fresh (empty) layers; see ./fancy-generators ns.
   (s/with-gen #(gen/fmap layer-of-cells (s/gen ::params/params)))))

(declare layer-embed-impl)

(s/def ::layer-of-cells
  (->
   (s/merge ::layer-of-cells-unembedded
            (s/keys :req-un [::cx/embedding
                             ::proximal-sg
                             ::distal-sg
                             ::apical-sg
                             ::ilateral-sg]))
   (s/with-gen #(util/fn->generator #'layer-embed-impl))))

;;; ## Synapse tracing

(defn distal-sources-widths
  [params embedding]
  [(if (:lateral-synapses? params)
     (reduce * (:depth params) (:column-dimensions params))
     0)
   (topo/size (-> embedding :lat-topo))])

(defn cell->id
  "Converts a cell id to an output bit index.
  Applies to cells in the current layer only."
  [depth [col ci]]
  (+ (* col depth) ci))

(s/fdef cell->id
        :args (s/cat :depth ::params/depth, :cell-id ::cell-id)
        :ret nat-int?)

(defn- cells->bits
  [depth cells]
  (map (partial cell->id depth) cells))

(defn id->cell
  "Converts an output bit index to a cell id.
  Applies to cells in the current layer only."
  [depth id]
  [(quot id depth)
   (rem id depth)])

(s/fdef id->cell
        :args (s/cat :depth ::params/depth, :id nat-int?)
        :ret ::cell-id)

(defn id->source
  "Returns a vector [k v] where k is one of :this or :lat. In the
  case of :this, v is [col ci], otherwise v gives the index in the
  lateral input field."
  [params embedding id]
  (let [[this-w lat-w] (distal-sources-widths params embedding)]
    (cond
     (< id this-w) [:this (id->cell (:depth params) id)]
     (< id (+ this-w lat-w)) [:lat (- id this-w)])))

(s/fdef id->source
        :args (s/cat :params ::params/params
                     :embedding ::cx/embedding
                     :id nat-int?)
        :ret (s/or :this (s/tuple #{:this} ::cell-id)
                   :lat (s/tuple #{:lat} nat-int?)))

;;; ## Activation

(defn segment-activation
  "Returns the number of active cells to which the synapses are
  connected, i.e. where synapse permanence is equal to or greater than
  `pcon`."
  [syns active-bits pcon]
  (count-filter (fn [[id p]]
                  (and (>= p pcon)
                       (active-bits id)))
                syns))

(s/fdef segment-activation
        :args (s/cat :syns ::syn/segment
                     :active-bits ::cx/bits-set
                     :pcon ::syn/permanence) ;; can be zero here
        :ret nat-int?)

(defn best-matching-segment
  "Finds the segment in the cell having the most active synapses, as
  long as is above the activation threshold `min-act`, only considering
  synapses with permanence values at or above `pcon`.
  Returns `[seg-index excitation synapses]`. If no such segments exist,
  returns `[nil 0 {}]`."
  [segs active-bits min-act pcon]
  (loop [segs (seq segs)
         si 0
         best-si 0
         best-act 0
         best-syns nil]
    (if-let [syns (first segs)]
      (let [act (long (segment-activation syns active-bits pcon))
            best? (> act best-act)]
        (recur (next segs)
               (inc si)
               (if best? si best-si)
               (if best? act best-act)
               (if best? syns best-syns)))
      ;; finished
      (if (>= best-act min-act)
        [best-si best-act best-syns]
        [nil 0 {}]))))

(s/fdef best-matching-segment
        :args (s/cat :segs (s/every ::syn/segment)
                     :active-bits ::cx/bits-set
                     :min-act ::params/learn-threshold
                     :pcon ::syn/permanence) ;; can be zero here
        :ret (s/cat :seg-index (s/nilable nat-int?)
                    :exc ::syn/excitation-amt
                    :syns ::syn/segment))

(defn best-segment-excitations-and-paths
  "Finds the most excited dendrite segment for each cell. Returns two maps,
  one maps cells to their final excitation amount, and the other maps cells to
  their most excited segment together with its excitation amount."
  [seg-exc]
  (loop [seg-exc (seq seg-exc)
         cell-excs (transient {})
         cell-best-seg (transient {})]
    (if-let [[path exc] (first seg-exc)]
      (let [cell-id (pop path) ;; seg-id to cell-id: [col ci _]
            prev-exc (get cell-excs cell-id 0.0)]
        (if (> exc prev-exc)
          (recur (next seg-exc)
                 (assoc! cell-excs cell-id exc)
                 (assoc! cell-best-seg cell-id [path exc]))
          (recur (next seg-exc)
                 cell-excs
                 cell-best-seg)))
      ;; finished
      [(persistent! cell-excs)
       (persistent! cell-best-seg)])))

(s/fdef best-segment-excitations-and-paths
        :args (s/cat :seg-exc ::seg-exc)
        :ret (s/cat :cell-exc ::cell-exc
                    :cell-best-seg (s/map-of ::cell-id ::best-seg)))

(defn best-by-column
  "Returns a map of column ids to representative excitation values,
  being the greatest excitation of its constituent cells or segments."
  [cell-exc]
  (persistent!
   (reduce-kv (fn [m id exc]
                (let [[col _] id] ;; cell-id / seg-id to col
                  (assoc! m col (max exc (get m col 0.0)))))
              (transient {})
              cell-exc)))

(s/fdef best-by-column
        :args (s/cat :cell-exc ::cell-exc)
        :ret (s/map-of ::column-id ::syn/excitation-amt))

(defn total-excitations
  "Combine the proximal and distal excitations in a map of cell id to
  excitation, as a weighted sum. Normally only cells with some
  proximal input are included, but if `spontaneous-activation?` is
  true, this is relaxed (i.e. prediction alone could cause
  activation).

  * col-exc is keyed by column as [col 0]."
  [col-exc distal-exc distal-weight spontaneous-activation? depth]
  (let [;; expand to all cells within columns
        basic-exc (for [[[col _] exc] col-exc
                        ci (range depth)
                        :let [cell-id [col ci]]]
                    [cell-id exc])]
    (if (zero? distal-weight)
      (into {} basic-exc)
      (let [basic-exc (if spontaneous-activation?
                        (into (zipmap (keys distal-exc) (repeat 0.0))
                              basic-exc)
                        basic-exc)]
        ;; add distal values
        (persistent!
         (reduce (fn [m [id p-exc]]
                   (let [d-exc (distal-exc id 0.0)]
                     (assoc! m id (+ p-exc (* distal-weight d-exc)))))
                 (transient {})
                 basic-exc))))))

(s/fdef total-excitations
        :args (s/cat :col-exc (s/every-kv (s/tuple ::column-id #{0})
                                          ::syn/excitation-amt)
                     :distal-exc ::cell-exc
                     :distal-weight ::params/distal-vs-proximal-weight
                     :spontaneous-activation? ::params/spontaneous-activation?
                     :depth ::params/depth)
        :ret ::cell-exc
        ;; check that every column in col-exc appears in result
        :fn (fn [v] (every? (->> v :ret keys (map first) (set))
                            (->> v :args :col-exc keys (map first)))))

(defn column-active-cells
  "Returns a sequence of cell ids to become active in the column.
  If no cells have excitation over the threshold, then all become
  active (bursting). Otherwise, only cells above the threshold become
  active; but if the top excitation exceeds all others by at least
  `dominance-margin` then the others are inhibited even if they are
  over the threshold."
  [col cell-exc depth threshold dominance-margin]
  (let [cell-ids (for [ci (range depth)] [col ci])]
    (loop [ids cell-ids
           best-ids ()
           best-exc -99999.9
           good-ids () ;; over threshold
           second-exc (double threshold)]
      (if-let [id (first ids)]
        (let [exc (double (cell-exc id 0))
              equal-best? (== exc best-exc)
              new-best? (> exc best-exc)
              good? (>= exc threshold)]
          (recur (next ids)
                 (cond equal-best? (conj best-ids id)
                       new-best? (list id)
                       :else best-ids)
                 (if new-best? exc best-exc)
                 (if good? (conj good-ids id) good-ids)
                 (if new-best?
                   (max best-exc second-exc) ;; think best-exc (second-exc just for init)
                   (if (< second-exc exc best-exc) ;; between second & best
                     exc
                     second-exc))))
        ;; finished
        (cond
          ;; stimulus threshold not reached
          (< best-exc threshold)
          cell-ids
          ;; best cells exceed all others by dominance-margin
          (>= (- best-exc second-exc) dominance-margin)
          best-ids
          ;; otherwise, all cells over threshold become active
          :else
          good-ids)))))

(s/fdef column-active-cells
        :args (s/cat :cols ::column-id
                     :cell-exc ::cell-exc
                     :depth ::params/depth
                     :threshold ::params/stimulus-threshold
                     :dominance-margin ::params/dominance-margin)
        :ret (s/coll-of ::cell-id :min-count 1 :distinct true))

(defn select-active-cells
  "Determines active cells in the given columns and whether they are bursting."
  [a-cols cell-exc depth threshold dominance-margin]
  (loop [cols (seq a-cols)
         col-ac (transient {}) ;; active cells by column
         ac (transient #{}) ;; active cells
         sac (transient #{}) ;; stable active cells
         b-cols (transient #{})] ;; bursting columns

    (if-let [col (first cols)]
      (let [this-ac (column-active-cells col cell-exc depth
                                         threshold dominance-margin)
            bursting? (< (cell-exc (first this-ac) 0) threshold)]
        (recur (next cols)
               (assoc! col-ac col this-ac)
               (reduce conj! ac this-ac)
               (if bursting? sac (reduce conj! sac this-ac))
               (if bursting? (conj! b-cols col) b-cols)))
      ;; finished
      {:col-active-cells (persistent! col-ac)
       :active-cells (persistent! ac)
       :stable-active-cells (persistent! sac)
       :burst-cols (persistent! b-cols)})))

(s/fdef select-active-cells
        :args (s/cat :a-cols ::active-columns
                     :cell-exc ::cell-exc
                     :depth ::params/depth
                     :threshold ::params/stimulus-threshold
                     :dominance-margin ::params/dominance-margin)
        :ret (s/keys :opt-un [::col-active-cells
                              ::active-cells
                              ::stable-active-cells
                              ::burst-cols]))

;;; ## Learning

(defn select-winner-cell-and-seg
  "For one column, selects a winning cell and dendrite segments.
  Returns [winner-cell [distal-seg-path exc] [apical-seg-path exc]]
  giving the best matching existing segments to learn on, if any."
  [ac distal-state apical-state distal-sg apical-sg params rng]
  (let [fully-matching-distal (:fully-matching-segs distal-state)
        fully-matching-apical (:fully-matching-segs apical-state)
        d-full-matches (keep fully-matching-distal ac)
        a-full-matches (keep fully-matching-apical ac)
        distal-bits (:active-bits distal-state)
        apical-bits (:active-bits apical-state)
        min-distal (:learn-threshold (:distal params))
        min-apical (:learn-threshold (:apical params))
        apical-bias-frac (:apical-bias-frac params)
        ;; TODO: perf - maintain index of targets-by-source with pcon=0
        best-partial-distal-segment
        (fn [cell-id]
          (let [cell-segs (syn/cell-segments distal-sg cell-id)
                [match-si exc seg] (best-matching-segment
                                    cell-segs distal-bits min-distal 0.0)]
            (when match-si
              [(conj cell-id match-si) exc])))
        best-partial-apical-segment
        (fn [cell-id]
          (let [cell-segs (syn/cell-segments apical-sg cell-id)
                [match-si exc seg] (best-matching-segment
                                    cell-segs apical-bits min-apical 0.0)]
            (when match-si
              [(conj cell-id match-si) exc])))
        distal-match*
        (cond
          ;; * if one matching distal segment
          ;; ==> select it
          (== (count d-full-matches) 1)
          (first d-full-matches)
          ;; * if multiple matching distal segments
          ;; ==> these are the active cells; fall through to apical selection
          ;; (and finally select distal segment afterwards)
          (> (count d-full-matches) 1)
          nil
          ;; * if some partial matching distal segments
          ;; ==> select best of those
          :else
          (let [partial-matches (keep best-partial-distal-segment ac)]
            (if (seq partial-matches)
              (apply max-key second partial-matches)
              ;; * otherwise - no distal matches
              nil)))

        apical-match
        (cond
          ;; * if there is already a distal match
          ;; ==> select best apical segment on that cell
          distal-match*
          (let [cell-id (pop (first distal-match*))]
            (if-let [full-match (fully-matching-apical cell-id)]
              full-match
              (best-partial-apical-segment cell-id)))
          ;; * if multiple matching distal segments / cells
          ;; ==> select best by apical, fall back to random
          (or (> (count d-full-matches) 1)
              ;; * if some matching apical segment
              ;; ==> with some probability, select it, otherwise random
              (and (pos? apical-bias-frac)
                   (<= (random/rand-double rng) apical-bias-frac)))
          (if (seq a-full-matches)
            (util/rand-nth rng a-full-matches)
            (let [partial-matches (keep best-partial-apical-segment ac)]
              (if (seq partial-matches)
                (apply max-key second partial-matches)
                ;; * otherwise - no apical matches
                nil)))

          ;; * otherwise - no matching apical segments
          :else
          nil)

        distal-match
        (cond
          ;; * if already have it, nothing to do
          distal-match*
          distal-match*
          ;; * if multiple matching distal segments
          ;;   (deferred case from above)
          ;; ==> select best segment on chosen cell (by apical or randomly)
          (> (count d-full-matches) 1)
          (let [cell-id (if apical-match
                          (pop (first apical-match))
                          (util/rand-nth rng ac))
                match (fully-matching-distal cell-id)]
            (assert match "fully active distal, if any, should equal active cells")
            match)
          :else
          nil)
        winner-cell
        (cond
          distal-match
          (pop (first distal-match))
          apical-match
          (pop (first apical-match))
          :else
          (util/rand-nth rng ac))]
    [winner-cell distal-match apical-match]))

(s/fdef select-winner-cell-and-seg
        :args (s/cat :col-ac (s/coll-of ::cell-id :min-count 1 :distinct true)
                     :distal-state ::distal-state
                     :apical-state ::distal-state
                     :distal-sg ::syn/synapse-graph
                     :apical-sg ::syn/synapse-graph
                     :params ::params/params
                     :rng ::util/rng)
        :ret (s/cat :winner-cell ::cell-id
                    :distal-match (s/nilable ::best-seg)
                    :apical-match (s/nilable ::best-seg)))

(defn select-winner-cells-and-segs
  "The returned :winner-seg maps are keyed by winning cell ids but only identify
  a segment when an existing one matches sufficiently to be learning. Otherwise,
  the value is nil and a new segment will be grown. We keep winner cells stable
  in continuing active columns."
  [col-ac prior-winners distal-state apical-state distal-sg apical-sg params rng]
  (let [reset? (empty? (:active-bits distal-state))]
    (loop [col-ac col-ac
           col-winners (transient {})
           winning-distal (transient {})
           winning-apical (transient {})
           rng rng]
      (if-let [[col ac] (first col-ac)]
        (if reset?
          (recur (next col-ac)
                 (assoc! col-winners col (first ac))
                 winning-distal winning-apical rng)
          (let [prior-winner (get prior-winners col)
                ac (if (and prior-winner (some #(= % prior-winner) ac))
                     [prior-winner]
                     ac)
                [rng* rng] (random/split rng)
                [winner dmatch amatch]
                (select-winner-cell-and-seg ac distal-state apical-state
                                            distal-sg apical-sg params rng*)]
            (recur (next col-ac)
                   (assoc! col-winners col winner)
                   (assoc! winning-distal winner dmatch)
                   (assoc! winning-apical winner amatch)
                   rng)))
        ;; finished
        {:col-winners (persistent! col-winners)
         :winner-seg {:distal (persistent! winning-distal)
                      :apical (persistent! winning-apical)}}))))

(s/fdef select-winner-cells-and-segs
        :args (s/cat :col-ac ::col-active-cells
                     :prior-winners (s/nilable ::col-winners)
                     :distal-state ::distal-state
                     :apical-state ::distal-state
                     :distal-sg ::syn/synapse-graph
                     :apical-sg ::syn/synapse-graph
                     :params ::params/params
                     :rng ::util/rng)
        :ret (s/keys :req-un [::col-winners
                              ::winner-seg]))

(defn new-segment-id
  "Returns a segment index on the cell at which to grow a new segment,
  together with any existing synapses at that index. It may refer to
  the end of the existing vector to append to it, or it may refer to
  an existing segment that is to be culled before the new one
  grows. If the maximum number of segments has been reached, an
  existing one is chosen to be replaced based on having the fewest
  connected synapses, or fewest synapses to break ties."
  [segs pcon max-segs max-syns]
  (let [segs (take-while seq segs)]
    (if (>= (count segs) max-segs)
      ;; select the one with fewest connected, or fewest synapses, or first
      (let [si (apply min-key (fn [si]
                                (let [syns (nth segs si)
                                      n-conn (count-filter #(>= % pcon) (vals syns))]
                                  (+ (* n-conn max-syns)
                                     (count syns)
                                     (/ si (count segs)))))
                      (range (count segs)))]
        [si (nth segs si)])
      ;; have not reached limit; append
      [(count segs) nil])))

(s/fdef new-segment-id
        :args (s/cat :segs (s/every ::syn/segment)
                     :pcon ::params/perm-connected
                     :max-segs ::params/max-segments
                     :max-syns ::params/max-synapse-count)
        :ret (s/cat :seg-index nat-int?
                    :syns (s/nilable ::syn/segment)))

(defn segment-new-synapse-source-ids
  "Returns a collection of up to n ids chosen from the learnable
  source bits. May be less than `n` if the random samples have
  duplicates or some already exist on the segment, or if there are
  fewer than `n` learnable cells."
  [seg learnable-bits-vec n rng]
  (when (seq learnable-bits-vec)
    (->> learnable-bits-vec
         (util/sample rng n)
         (distinct)
         (remove seg))))

(s/fdef segment-new-synapse-source-ids
        :args (s/cat :seg ::syn/segment
                     :learnable-bits (s/coll-of nat-int? :distinct true :kind vector?)
                     :n (s/int-in 0 1e7)
                     :rng ::util/rng)
        :ret (s/nilable (s/coll-of nat-int? :distinct true)))

(defn learning-updates
  "Takes the learning `cells` and maps each to a SegUpdate record,
  which includes the segment path to learn on, together with lists of
  any synapse sources to add or remove. Any matching segments to learn
  on are given as `matching-segs`, mapping cell ids to `[seg-path
  exc]`. If this is missing for a cell then a new segment will be
  grown, perhaps replacing an existing one.

  Note that ''cell-ids'' here may also refer to columns in a proximal
  synapse graph, where the convention is [col 0]. Everything else is
  the same since proximal synapses graphs can also have multiple
  segments [col 0 seg-idx]."
  [cells matching-segs sg learnable-bits rng
   {pcon :perm-connected
    min-act :learn-threshold
    new-syns :new-synapse-count
    max-syns :max-synapse-count
    max-segs :max-segments}]
  (let [learnable-bits (vec learnable-bits)] ;; for faster sampling
    (loop [cells (seq cells)
           m (transient {})
           rng rng]
      (if-let [cell-id (first cells)]
        (let [[matching-path exc] (get matching-segs cell-id)
              new-segment? (not matching-path)
              cell-segs (syn/cell-segments sg cell-id)
              [new-si replaced-syns] (when new-segment?
                                       (new-segment-id cell-segs pcon max-segs
                                                       max-syns))
              seg (if new-segment?
                    {}
                    (let [[_ _ si] matching-path]
                      (nth cell-segs si)))
              ;; don't usurp existing synapses, wait for them to be culled
              ;; (was problem of constantly displacing new synapses)
              grow-n (-> (- new-syns (or exc 0))
                         ;; only grow in free space
                         (min (- max-syns (count seg)))
                         (max 0))
              [rng* rng] (random/split rng)
              grow-source-ids (segment-new-synapse-source-ids seg learnable-bits
                                                              grow-n rng*)
              die-source-ids (when new-segment? (keys replaced-syns))
              seg-path (if new-segment? (conj cell-id new-si) matching-path)]
          (recur (next cells)
                 ;; if not enough learnable sources to grow a new segment, skip it
                 (if (and new-segment?
                          (< (count grow-source-ids) min-act))
                   m ;; skip
                   (assoc! m cell-id (syn/seg-update seg-path :learn grow-source-ids
                                                     die-source-ids)))
                 rng))
        ;; finished
        (persistent! m)))))

(s/fdef learning-updates
        :args (s/cat :cells (s/nilable (s/every ::cell-id :distinct true))
                     :matching-segs ::matching-segs
                     :sg ::syn/synapse-graph
                     :learnable-bits (s/nilable ::cx/bits)
                     :rng ::util/rng
                     :params (s/keys :req-un [::params/perm-connected
                                              ::params/learn-threshold
                                              ::params/new-synapse-count
                                              ::params/max-synapse-count
                                              ::params/max-segments]))
        :ret (s/map-of ::cell-id ::syn/seg-update))

(defn learn-distal
  [sg distal-state cells matching-segs dparams rng]
  (let [learning (learning-updates cells matching-segs
                                   sg (:learnable-bits distal-state)
                                   rng dparams)
        new-sg (if (seq learning)
                 (syn/bulk-learn sg (vals learning) (:active-bits distal-state)
                               (:perm-inc dparams) (:perm-dec dparams)
                               (:perm-init dparams))
                 sg)]
    [new-sg
     learning]))

(s/fdef learn-distal
        :args (s/cat :sg ::syn/synapse-graph
                     :distal-state ::distal-state
                     :cells (s/nilable (s/coll-of ::cell-id))
                     :matching-segs ::matching-segs
                     :dparams ::params/synapse-graph-params
                     :rng ::util/rng)
        :ret (s/cat :new-sg ::syn/synapse-graph
                    :learning (s/map-of ::cell-id ::syn/seg-update)))

(defn punish-distal
  [sg distal-state prior-distal-state prior-active-cells dparams]
  (let [bad-cells (set/difference (:pred-cells prior-distal-state)
                                  ;; Ignore any which are still predictive.
                                  (:pred-cells distal-state)
                                  prior-active-cells)
        fully-matching-segs (:fully-matching-segs prior-distal-state)
        punishments (for [cell bad-cells
                          :let [[seg-path _] (fully-matching-segs cell)]]
                      (syn/seg-update seg-path :punish nil nil))
        new-sg (if punishments
                 (syn/bulk-learn sg punishments (:active-bits prior-distal-state)
                                 (:perm-inc dparams) (:perm-punish dparams)
                                 (:perm-init dparams))
                 sg)]
    [new-sg
     punishments]))

(s/fdef punish-distal
        :args (s/cat :sg ::syn/synapse-graph
                     :distal-state ::distal-state
                     :prior-distal-state ::distal-state
                     :prior-active-cells (s/coll-of ::cell-id :kind set?)
                     :dparams ::params/synapse-graph-params)
        :ret (s/cat :new-sg ::syn/synapse-graph
                    :punishments (s/every ::syn/seg-update)))

(defn layer-learn-lateral
  [this cells matching-segs]
  (let [sg (:distal-sg this)
        dstate (:distal-state this)
        dparams (:distal (:params this))
        [rng* rng] (random/split (:rng this))
        [new-sg learning] (learn-distal sg dstate cells matching-segs dparams rng*)]
    (assoc this
           :rng rng
           :learn-state (assoc-in (:learn-state this)
                                  [:learning :distal] learning)
           :distal-sg new-sg)))

(defn layer-learn-apical
  [this cells matching-segs]
  (let [sg (:apical-sg this)
        dstate (:apical-state this)
        dparams (:apical (:params this))
        [rng* rng] (random/split (:rng this))
        [new-sg learning] (learn-distal sg dstate cells matching-segs dparams rng*)]
    (assoc this
           :rng rng
           :learn-state (assoc-in (:learn-state this)
                                  [:learning :apical] learning)
           :apical-sg new-sg)))

(defn layer-punish-lateral
  [this prior-ac]
  (let [sg (:distal-sg this)
        dstate (:distal-state this)
        pdstate (:prior-distal-state this)
        dparams (:distal (:params this))
        [new-sg punishments] (punish-distal sg dstate pdstate prior-ac dparams)]
    (assoc this
           :learn-state (assoc-in (:learn-state this)
                                  [:punishments :distal] punishments)
           :distal-sg new-sg)))

(defn layer-punish-apical
  [this prior-ac]
  (let [sg (:apical-sg this)
        dstate (:apical-state this)
        pdstate (:prior-apical-state this)
        dparams (:apical (:params this))
        [new-sg punishments] (punish-distal sg dstate pdstate prior-ac dparams)]
    (assoc this
           :learn-state (assoc-in (:learn-state this)
                                  [:punishments :apical] punishments)
           :apical-sg new-sg)))

(defn layer-learn-ilateral
  [this cols]
  (let [sg (:ilateral-sg this)
        dparams (:ilateral (:params this))
        ids (map vector cols (repeat 0))
        matching-segs (into {}
                            (map (fn [id]
                                   [id [(conj id 0) 1.0]]))
                            ids)
        state {:active-bits cols
               :learnable-bits cols}
        [rng* rng] (random/split (:rng this))
        [new-sg learning] (learn-distal sg state ids matching-segs dparams rng*)]
    (assoc this
           :rng rng
           :learn-state (assoc-in (:learn-state this)
                                  [:learning :ilateral] learning)
           :ilateral-sg new-sg)))

(defn layer-learn-proximal
  [this cols]
  (let [sg (:proximal-sg this)
        state (:active-state this)
        pparams (:proximal (:params this))
        min-prox (:learn-threshold pparams)
        active-bits (set (:bits (:in-ff-signal state)))
        fully-matching-segs (:fully-matching-ff-segs state)
        ids (map vector cols (repeat 0))
        matching-segs
        (persistent!
         (reduce (fn [m id]
                   (if-let [full-match (fully-matching-segs id)]
                     (assoc! m id full-match)
                     (let [cell-segs (syn/cell-segments sg id)
                           [match-si exc seg] (best-matching-segment
                                               cell-segs active-bits min-prox 0.0)]
                       (if match-si
                         (assoc! m id [(conj id match-si) exc])
                         m))))
                 (transient {})
                 ids))
        [rng* rng] (random/split (:rng this))
        prox-learning (learning-updates ids
                                        matching-segs
                                        sg
                                        (when (:grow? pparams)
                                          active-bits)
                                        rng* pparams)
        psg (cond-> sg
              (seq prox-learning)
              (syn/bulk-learn (vals prox-learning) active-bits
                              (:perm-inc pparams) (:perm-dec pparams)
                              (:perm-init pparams))
              ;; positive learning rate is higher for stable (predicted) inputs
              (and (seq prox-learning)
                   (seq (::stable-bits (:in-ff-signal state)))
                   (> (:perm-stable-inc pparams) (:perm-inc pparams)))
              (syn/bulk-learn (map #(syn/seg-update (:target-id %) :reinforce nil nil)
                                   (vals prox-learning))
                              (set (::stable-bits (:in-ff-signal state)))
                              (- (:perm-stable-inc pparams) (:perm-inc pparams))
                              (:perm-dec pparams)
                              (:perm-init pparams)))]
    (assoc this
           :rng rng
           :learn-state (assoc-in (:learn-state this)
                                  [:learning :proximal] prox-learning)
           :proximal-sg psg)))

(s/fdef layer-learn-proximal
        :args (s/cat :layer ::layer-of-cells
                     :cols (s/coll-of ::column-id))
        :ret ::layer-of-cells)

(defn punish-proximal
  [sg state pparams]
  (let [fully-matching-segs (:fully-matching-ff-segs state)
        pred-cells (set (keys fully-matching-segs))
        active-cells (set (map vector (:active-cols state) (repeat 0)))
        active-bits (set (:bits (:in-ff-signal state)))
        bad-cells (set/difference pred-cells
                                  active-cells)
        punishments (for [cell bad-cells
                          :let [[seg-path _] (fully-matching-segs cell)]
                          :when seg-path]
                      (syn/seg-update seg-path :punish nil nil))
        new-sg (if punishments
                 (syn/bulk-learn sg punishments
                                 active-bits
                                 (:perm-inc pparams) (:perm-punish pparams)
                                 (:perm-init pparams))
                 sg)]
    [new-sg
     punishments]))

(defn layer-punish-proximal
  [this]
  (let [sg (:proximal-sg this)
        state (:active-state this)
        pparams (:proximal (:params this))
        [new-sg punishments] (punish-proximal sg state pparams)]
    (assoc this
           :learn-state (assoc-in (:learn-state this)
                                  [:punishments :proximal] punishments)
           :proximal-sg new-sg)))

;;; ## Orchestration

(defn update-inhibition-radius
  [layer]
  (let [params (:params layer)]
    (assoc layer :inh-radius
           (inh/inhibition-radius (:proximal-sg layer)
                                  (topo/make-topography (:column-dimensions params))
                                  (-> layer :embedding :ff-topo)))))

;; these are records only so as to work with repl/truncate-large-data-structures
(defrecord LayerActiveState [])
(defrecord LayerTPState [])
(defrecord LayerLearnState [])
(defrecord LayerDistalState [])

(def empty-active-state
  (map->LayerActiveState
   {:active-cells #{}
    :active-cols #{}
    :fully-matching-ff-segs {}}))

(def empty-tp-state
  (map->LayerTPState
   {}))

(def empty-learn-state
  (map->LayerLearnState
   {:learning-cells #{}
    :prior-active-cells #{}}))

(def empty-distal-state
  (map->LayerDistalState
   {:active-bits #{}
    :learnable-bits #{}
    :cell-exc {}
    :pred-cells #{}
    :fully-matching-segs {}}))

(defmulti spatial-pooling
  "Spatial pooling: choosing a column representation."
  (fn [layer ff-signal fb-cell-exc]
    (:spatial-pooling (:params layer))))

(s/fdef spatial-pooling
        :args (s/cat :layer ::layer-of-cells
                     :ff-signal ::cx/signal
                     :fb-cell-exc (-> ::cell-exc (s/with-gen #(gen/return {}))))
        :ret (s/keys :req-un [::active-cols
                              ::fully-matching-ff-segs
                              ::col-overlaps]))

(defmulti temporal-pooling
  "Temporal pooling: a relatively stable layer output signal over time."
  (fn [layer active-state prev-tp-state]
    (:temporal-pooling (:params layer))))

(s/fdef temporal-pooling
        :args (s/cat :layer ::layer-of-cells
                     :active-state ::active-state
                     :prev-tp-state ::tp-state)
        :ret (s/keys :req-un [::out-stable-ff-bits]))

(defmethod spatial-pooling :standard
  [layer ff-signal fb-cell-exc]
  (let [proximal-sg (:proximal-sg layer)
        boosts (:boosts layer)
        params (:params layer)
        ;; proximal excitation as number of active synapses, keyed by [col 0 seg-idx]
        col-seg-overlaps (syn/excitations proximal-sg (:bits ff-signal)
                                          (:stimulus-threshold (:proximal params)))
        ;; these both keyed by [col 0]
        [raw-col-exc matching-ff-segs]
        (best-segment-excitations-and-paths col-seg-overlaps)
        col-exc (homeo/apply-overlap-boosting raw-col-exc boosts)
        ;; combine excitation values for selecting columns
        abs-cell-exc (total-excitations col-exc fb-cell-exc
                                        (:distal-vs-proximal-weight params)
                                        (:spontaneous-activation? params)
                                        (:depth params))
        n-on (max 1 (round (* (:activation-level params)
                              (:n-columns layer))))
        a-cols (-> (best-by-column abs-cell-exc)
                   (inh/inhibit-globally n-on)
                   (set))]
    {:active-cols a-cols
     :fully-matching-ff-segs matching-ff-segs
     :col-overlaps raw-col-exc}))

(defmethod spatial-pooling :local-inhibition
  [layer ff-signal fb-cell-exc]
  (let [proximal-sg (:proximal-sg layer)
        boosts (:boosts layer)
        params (:params layer)
        ;; proximal excitation as number of active synapses, keyed by [col 0 seg-idx]
        col-seg-overlaps (syn/excitations proximal-sg (:bits ff-signal)
                                          (:stimulus-threshold (:proximal params)))
        ;; these both keyed by [col 0]
        [raw-col-exc matching-ff-segs]
        (best-segment-excitations-and-paths col-seg-overlaps)
        col-exc (homeo/apply-overlap-boosting raw-col-exc boosts)
        ;; combine excitation values for selecting columns
        abs-cell-exc (total-excitations col-exc fb-cell-exc
                                        (:distal-vs-proximal-weight params)
                                        (:spontaneous-activation? params)
                                        (:depth params))
        col-dims (:column-dimensions params)
        n-on (max 1 (round (* (:activation-level params) (:n-columns layer))))
        a-cols (-> (best-by-column abs-cell-exc)
                   (inh/inhibit-locally (topo/make-topography col-dims)
                                        (* (:inh-radius layer) (:inh-radius-scale params))
                                        (:inhibition-base-distance params)
                                        n-on)
                   (set))]
    {:active-cols a-cols
     :fully-matching-ff-segs matching-ff-segs
     :col-overlaps raw-col-exc}))

(defn compute-active-state
  [layer ff-signal]
  (let [params (:params layer)
        distal-state (:distal-state layer)
        apical-state (:apical-state layer)
        ;; ignore apical excitation unless there is matching distal.
        ;; unlike other segments, allow apical excitation to add to distal
        fb-cell-exc (if (seq (:cell-exc apical-state))
                       (merge-with + (:cell-exc distal-state)
                                   (select-keys (:cell-exc apical-state)
                                                (keys (:cell-exc distal-state))))
                       (:cell-exc distal-state))
        sp-info (spatial-pooling layer ff-signal fb-cell-exc)
        ;; find active cells in the columns
        cell-info (select-active-cells (:active-cols sp-info) fb-cell-exc
                                       (:depth params)
                                       (:stimulus-threshold (:distal params))
                                       (:dominance-margin params))
        depth (:depth params)]
    (map->LayerActiveState
     (merge
      sp-info
      cell-info
      {:in-ff-signal ff-signal
       :out-immediate-ff-bits (cells->bits depth (:active-cells cell-info))
       :timestep (inc (:timestep (:active-state layer)))}))))

(s/fdef compute-active-state
        :args (s/cat :layer ::layer-of-cells
                     :ff-signal ::cx/signal)
        :ret ::active-state)

(defmethod temporal-pooling :standard
  [layer active-state prev-tp-state]
  (let [params (:params layer)
        ;; here stable means not in a bursting column
        new-stable-cells (:stable-active-cells active-state)
        ;; continuing mini-burst synapses for temporal pooling
        stable-cells-buffer
        (-> (loop [q (or (:stable-cells-buffer prev-tp-state) util/empty-queue)]
              (if (>= (count q) (:stable-activation-steps params))
                (recur (pop q))
                q))
            (conj new-stable-cells))
        all-stable-cells (apply set/union stable-cells-buffer)
        depth (:depth params)
        all-stable-bits (set (cells->bits depth all-stable-cells))]
    (map->LayerTPState
     {:out-stable-ff-bits all-stable-bits
      :stable-cells-buffer stable-cells-buffer})))

(defn compute-tp-state
  [layer active-state]
  (temporal-pooling layer active-state (:tp-state layer)))

(defn compute-distal-state
  [sg active-bits learnable-bits dparams t]
  (let [seg-exc (syn/excitations sg active-bits (:stimulus-threshold dparams))
        [cell-exc fully-matching-segs] (best-segment-excitations-and-paths seg-exc)
        pc (set (keys cell-exc))]
    (map->LayerDistalState
     {:active-bits (set active-bits)
      :learnable-bits (set learnable-bits)
      :cell-exc cell-exc
      :fully-matching-segs fully-matching-segs
      :pred-cells pc
      :timestep t})))

(s/fdef compute-distal-state
        :args (s/cat :sg ::syn/synapse-graph
                     :active-bits ::cx/bits
                     :learnable-bits ::cx/bits
                     :dparams ::params/synapse-graph-params
                     :t ::cx/timestep)
        :ret ::distal-state)

(declare uniform-ff-synapses)

(defn layer-embed-impl
  [layer embedding]
  (let [{:keys [ff-topo fb-topo lat-topo]} embedding
        params (:params layer)
        col-topo (topo/make-topography (:column-dimensions params))
        n-cols (topo/size col-topo)
        depth (:depth params)
        n-distal (+ (if (:lateral-synapses? params)
                      (* n-cols depth) 0)
                    (topo/size lat-topo))
        n-apical (topo/size fb-topo)
        [rng rng*] (random/split (:rng layer))
        col-prox-syns (uniform-ff-synapses col-topo ff-topo
                                           params rng*)
        proximal-sg (syn/col-segs-synapse-graph col-prox-syns n-cols
                                                (:max-segments (:proximal params))
                                                (topo/size ff-topo)
                                                (:perm-connected (:proximal params))
                                                (:grow? (:proximal params)))
        distal-sg (syn/cell-segs-synapse-graph n-cols depth
                                               (:max-segments (:distal params))
                                               n-distal
                                               (:perm-connected (:distal params))
                                               true)
        apical-sg (syn/cell-segs-synapse-graph n-cols depth
                                               (:max-segments (:apical params))
                                               n-apical
                                               (:perm-connected (:apical params))
                                               true)
        ilateral-sg (syn/cell-segs-synapse-graph n-cols 1
                                                 (:max-segments (:ilateral params))
                                                 n-cols
                                                 (:perm-connected (:ilateral params))
                                                 true)]
    (->
     (assoc layer
       :embedding embedding
       :rng rng
       :proximal-sg proximal-sg
       :distal-sg distal-sg
       :apical-sg apical-sg
       :ilateral-sg ilateral-sg)
     (update-inhibition-radius))))

(s/fdef layer-embed-impl
        :args (s/cat :layer ::layer-of-cells-unembedded
                     :embedding ::cx/embedding)
        :ret ::layer-of-cells)

(defn layer-activate-impl
  [layer ff-signal]
  (let [new-active-state (compute-active-state layer ff-signal)
        timestep (:timestep new-active-state)
        {:keys [params prior-active-state tp-state]} layer
        effective? (<= (util/set-similarity (:active-cols new-active-state)
                                            (:active-cols prior-active-state))
                       (:transition-similarity params))]
    (-> layer
        (assoc :active-state new-active-state
               :tp-state (if effective?
                           (compute-tp-state layer new-active-state)
                           tp-state)
               :prior-active-state (if effective?
                                     new-active-state
                                     prior-active-state)))))

(defn layer-learn-impl
  [layer]
  (if (< (:timestep (:prior-active-state layer))
         (:timestep (:active-state layer)))
    layer
    ;; effective transition, based on sufficient difference.
    (let [{:keys [params active-state learn-state distal-state apical-state rng
                  distal-sg apical-sg]} layer
          a-cols (:active-cols active-state)
          col-ac (:col-active-cells active-state)
          prior-winners (when-not (:break-winners? learn-state)
                          (:col-winners learn-state))
          [rng* rng] (random/split rng)
          {:keys [col-winners winner-seg]}
          (select-winner-cells-and-segs col-ac prior-winners distal-state apical-state
                                        distal-sg apical-sg params rng*)
          ;; learning cells are all the winning cells
          winner-cells (vals col-winners)
          lc winner-cells
          depth (:depth params)
          out-wc-bits (set (cells->bits depth winner-cells))
          timestep (:timestep active-state)]
      (cond->
       layer
       true (assoc :learn-state (assoc empty-learn-state
                                       :col-winners col-winners
                                       :winner-seg winner-seg
                                       :learning-cells lc
                                       :out-wc-bits out-wc-bits
                                       :prior-active-cells (:active-cells active-state)
                                       :timestep timestep)
                   :rng rng)
       (:learn? (:distal params)) (layer-learn-lateral lc (:distal winner-seg))
       (:learn? (:apical params)) (layer-learn-apical lc (:apical winner-seg))
       (:learn? (:ilateral params)) (layer-learn-ilateral a-cols)
       (:learn? (:proximal params)) (layer-learn-proximal a-cols)
       (:punish? (:distal params)) (layer-punish-lateral (:prior-active-cells learn-state))
       (:punish? (:apical params)) (layer-punish-apical (:prior-active-cells learn-state))
       (:punish? (:proximal params)) (layer-punish-proximal)
       true (update :active-duty-cycles homeo/update-duty-cycles
                    a-cols (:duty-cycle-period params))
       true (update :overlap-duty-cycles homeo/update-duty-cycles
                    (map first (keys (:col-overlaps active-state)))
                    (:duty-cycle-period params))
       (zero? (mod timestep (:boost-active-every params))) (homeo/boost-active)
       (zero? (mod timestep (:adjust-overlap-every params))) (homeo/adjust-overlap)
       (zero? (mod timestep (:float-overlap-every params))) (homeo/layer-float-overlap)
       (zero? (mod timestep (:inh-radius-every params))) (update-inhibition-radius)))))

(defn layer-depolarise-impl
  [layer fb-signal lat-signal]
  (let [{:keys [params prior-active-state learn-state distal-state apical-state
                distal-sg apical-sg]} layer
        depth (:depth params)
        widths (distal-sources-widths params (:embedding layer))
        distal-bits (util/align-indices widths
                                        [(if (:lateral-synapses? params)
                                           (:out-immediate-ff-bits prior-active-state)
                                           [])
                                         (:bits lat-signal)])
        distal-lbits (util/align-indices widths
                                         [(if (:lateral-synapses? params)
                                            (:out-wc-bits learn-state)
                                            [])
                                          (or (::winner-bits lat-signal)
                                              (:bits lat-signal))])
        apical-bits (:bits fb-signal)
        apical-lbits (::winner-bits fb-signal ())
        timestep (cx/timestep layer)]
   (assoc layer
     :prior-distal-state distal-state
     :prior-apical-state apical-state
     :distal-state (compute-distal-state distal-sg distal-bits distal-lbits
                                         (:distal params) timestep)
     :apical-state (compute-distal-state apical-sg apical-bits apical-lbits
                                         (:apical params) timestep))))

(defn layer-state-impl
  [{:keys [active-state learn-state distal-state prior-active-state
           prior-distal-state] :as layer}]
  {:active-columns (:active-cols active-state)
   :bursting-columns (:burst-cols active-state)
   :active-cells (:active-cells active-state)
   :winner-cells (set (vals (:col-winners learn-state)))
   :predictive-cells (when (== (:timestep active-state)
                               (:timestep distal-state))
                       (:pred-cells distal-state))
   :prior-predictive-cells (let [t-1 (dec (:timestep active-state))]
                             (cond
                               ;; after depolarise phase has run
                               (== t-1 (:timestep prior-distal-state))
                               (:pred-cells prior-distal-state)
                               ;; before depolarise phase has run
                               (== t-1 (:timestep distal-state))
                               (:pred-cells distal-state)))})

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


(defn segs-proximal-bit-votes
  [lyr seg-paths]
  (let [psg (:proximal-sg lyr)]
    (->> seg-paths
         (reduce (fn [m seg-path]
                   (let [ids (syn/sources-connected-to psg seg-path)]
                     (reduce (fn [m id]
                               (assoc! m id (inc (get m id 0))))
                             m ids)))
                 (transient {}))
         (persistent!))))

(defn- cells->columns
  [cells]
  (sequence (comp (map first) (distinct)) cells))

(defn layer-decode-to-ff-bits-impl
  "Decodes a set of columns into corresponding feed-forward input bits. Each
  column adds its connections to an accumulated number of votes for each input
  bit. The default is the columns containing predictive cells. To override,
  pass a function in opts key `:get-columns` which takes the layer and returns
  a collection of column ids."
  [layer {:keys [get-columns] :as opts
          :or {get-columns (comp cells->columns :predictive-cells cx/layer-state)}}]
  (when-let [unk (keys (dissoc opts :get-columns))]
    (println "Unknown opts keys to layer-decode-to-ff-bits: " unk))
  (let [cols (get-columns layer)
        segs (map (fn [col] [col 0 0]) cols)]
    (segs-proximal-bit-votes layer segs)))

(defrecord LayerOfCells
    [params rng topography embedding n-columns
     proximal-sg distal-sg apical-sg ilateral-sg active-state prior-active-state tp-state
     distal-state prior-distal-state apical-state prior-apical-state learn-state]

  cx/PLayer

  (layer-embed*
   [this embedding]
   (layer-embed-impl this embedding))

  (layer-activate*
    [this ff-signal]
    (layer-activate-impl this ff-signal))

  (layer-learn*
    [this]
    (layer-learn-impl this))

  (layer-depolarise*
    [this fb-signal lat-signal]
    (layer-depolarise-impl this fb-signal lat-signal))

  (layer-state*
   [this]
   (layer-state-impl this))

  (layer-decode-to-ff-bits*
   [this opts]
   (layer-decode-to-ff-bits-impl this opts))

  cx/PInterruptable
  (break [this mode]
    (case mode
      :tm (assoc this :distal-state
                 (assoc empty-distal-state :timestep (:timestep active-state)))
      :fb (assoc this :apical-state
                 (assoc empty-distal-state :timestep (:timestep active-state)))
      :syns (update-in this [:tp-state :stable-cells-buffer] empty)
      :winners (assoc-in this [:learn-state :break-winners?] true)))

  cx/PTopographic
  (topography [this]
    (:topography this))

  cx/PSignalSource
  (signal* [this]
    {:bits (set/union (:out-immediate-ff-bits active-state)
                      (:out-stable-ff-bits tp-state))
     ::stable-bits (:out-stable-ff-bits tp-state)
     ::winner-bits (:out-wc-bits learn-state)})

  cx/PTemporal
  (timestep [_]
    (:timestep active-state))

  cx/PParameterised
  (params [_]
    params))

(defn uniform-ff-synapses
  "Generates feed-forward synapses connecting columns to the input bit array.
  Connections are made locally by scaling the input space to the column space.
  Potential synapses are chosen within a radius in input space of
  `ff-potential-radius` fraction of the longest single dimension, and of those,
  `ff-init-frac` are chosen from a uniform random distribution. Initial
  permanence values are uniformly distributed in the range of `ff-perm-init`."
  [topo itopo params rng]
  (let [[p-lo p-hi] (:ff-perm-init params)
        global? (>= (:ff-potential-radius params) 1.0)
        ;; radius in input space, fraction of longest dimension
        radius (long (* (:ff-potential-radius params)
                        (apply max (topo/dimensions itopo))))
        frac (:ff-init-frac params)
        input-size (topo/size itopo)
        n-cols (topo/size topo)
        one-d? (or (== 1 (count (topo/dimensions topo)))
                   (== 1 (count (topo/dimensions itopo))))
        [cw ch cdepth] (topo/dimensions topo)
        [iw ih idepth] (topo/dimensions itopo)
        ;; range of coordinates usable as focus (adjust for radius at edges)
        focus-ix (fn [frac width]
                   (if (<= width (inc (* 2 radius)))
                     (quot width 2)
                     (-> frac
                         (* (- width (* 2 radius)))
                         (+ radius)
                         (int))))
        ;; range of z coordinates usable as focus for radius
        focus-izs (when idepth
                    (if (<= idepth (inc (* 2 radius)))
                      (list (quot idepth 2))
                      (range radius (- idepth radius))))]
    (if global?
      (let [n-syns (round (* frac input-size))]
        (->> (random/split-n rng n-cols)
             (mapv (fn [col-rng]
                     (into {}
                           (map (fn [rng]
                                  (let [[rng1 rng2] (random/split rng)]
                                    [(util/rand-int rng1 input-size)
                                     (util/rand rng2 p-lo p-hi)])))
                           (random/split-n col-rng n-syns))))))
      (->> (random/split-n rng n-cols)
           (mapv (fn [col col-rng]
                   (let [focus-i (if one-d?
                                   (int (* input-size (/ col n-cols)))
                                   ;; use corresponding positions in 2D
                                   (let [[cx cy _] (topo/coordinates-of-index topo col)
                                         ix (focus-ix (/ cx cw) iw)
                                         iy (focus-ix (/ cy ch) ih)
                                         ;; in 3D, choose z coordinate from range
                                         iz (when idepth
                                              (nth focus-izs (mod col (count focus-izs))))
                                         icoord (if idepth [ix iy iz] [ix iy])]
                                     (topo/index-of-coordinates itopo icoord)))
                         all-ids (vec (topo/neighbours-indices itopo focus-i radius -1))
                         n (round (* frac (count all-ids)))
                         [rng1 rng2] (random/split col-rng)
                         ids (cond
                               (< frac 0.4) ;; for performance:
                               (util/sample rng1 n all-ids)
                               (< frac 1.0)
                               (util/reservoir-sample rng1 n all-ids)
                               :else
                               all-ids)]
                     (into {}
                           (map (fn [id rng]
                                  [id (util/rand rng p-lo p-hi)])
                                ids
                                (random/split-n rng2 (count ids))))))
                 (range))))))

(s/fdef uniform-ff-synapses
        :args (s/cat :topo ::topo/topography
                     :itopo ::topo/topography
                     :params (s/keys :req-un [::params/ff-perm-init
                                              ::params/ff-init-frac
                                              ::params/ff-potential-radius])
                     :rng ::util/rng)
        :ret (s/every ::syn/segment :kind vector?))

(defn check-param-deprecations
  [params]
  (assert (not (contains? params :global-inhibition?))
          (str ":global-inhibition? now implied by default :spatial-pooling; "
               "for local algorithm use :spatial-pooling :local-inhibition")))

(defn init-layer-state
  [params]
  (let [unk (set/difference (set (keys params))
                            (set (keys params/parameter-defaults)))]
    (when (seq unk)
      (println "Warning: unknown keys in params:" unk)))
  (let [params (->> (util/deep-merge params/parameter-defaults params)
                    (s/assert ::params/params))
        col-dims (:column-dimensions params)
        n-cols (reduce * col-dims)
        depth (:depth params)
        topo (topo/make-topography (conj col-dims depth))
        active-state (assoc empty-active-state :timestep 0)
        learn-state (assoc empty-learn-state :timestep 0)
        distal-state (assoc empty-distal-state :timestep 0)]
    (check-param-deprecations params)
    {:params params
     :rng (random/make-random (:random-seed params))
     :topography topo
     :n-columns n-cols
     :inh-radius 1
     :active-state active-state
     :prior-active-state active-state
     :learn-state learn-state
     :distal-state distal-state
     :prior-distal-state distal-state
     :apical-state distal-state
     :prior-apical-state distal-state
     :tp-state empty-tp-state
     :boosts (vec (repeat n-cols 1.0))
     :active-duty-cycles (vec (repeat n-cols (:activation-level params)))
     :overlap-duty-cycles (vec (repeat n-cols (:activation-level params)))}))

(defn layer-of-cells
  [params]
  (->
   (init-layer-state params)
   (map->LayerOfCells)))

(s/fdef layer-of-cells :ret ::layer-of-cells-unembedded)

(defmethod cx/layer-spec LayerOfCells [_]
  ::layer-of-cells)

(defmethod cx/layer-unembedded-spec LayerOfCells [_]
  ::layer-of-cells-unembedded)

(defn column-state-freqs
  "Returns a map with the frequencies of columns in states
  `:active` (bursting), `:predicted`, `:active-predicted`. Note that
  these are distinct categories. The names are possibly misleading."
  [lyr]
  (let [lstate (cx/layer-state lyr)
        a-cols (:active-columns lstate)
        ppc (:prior-predictive-cells lstate)
        pp-cols (into #{} (map first ppc))
        hit-cols (set/intersection pp-cols a-cols)
        col-states (merge (zipmap pp-cols (repeat :predicted))
                          (zipmap a-cols (repeat :active))
                          (zipmap hit-cols (repeat :active-predicted)))]
    (-> {:active 0, :predicted 0, :active-predicted 0}
        (merge (frequencies (vals col-states)))
        (assoc :timestep (cx/timestep lyr)
               :n-columns (:n-columns lyr)))))

(s/fdef column-state-freqs
        :args (s/cat :lyr ::layer-of-cells)
        :ret (s/keys :req-un [::active
                              ::predicted
                              ::active-predicted
                              ::cx/timestep]))
