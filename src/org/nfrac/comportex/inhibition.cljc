(ns org.nfrac.comportex.inhibition
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util
             :refer [abs round mean remap]]))

(defn numeric-span
  [xs]
  (- (apply max xs) (apply min xs)))

(defn column-receptive-field-size
  "Returns the span over the input bit array to which this column has
   connected synapses. Takes the maximum span in any one dimension."
  [sg itopo col]
  (let [ids (p/sources-connected-to sg [col 0 0]) ;; first segment only - good enough?
        coords (map (partial p/coordinates-of-index itopo) ids)]
    (if (seq coords)
      (if (number? (first coords))
        (numeric-span coords)
        (let [m (count (p/dimensions itopo))]
          (->> (for [j (range m)]
                 (numeric-span (map #(nth % j) coords)))
               (apply max))))
      0)))

(defn avg-receptive-field-size
  [sg topo itopo]
  (-> (map (partial column-receptive-field-size sg itopo)
           (range (p/size topo)))
      (mean)))

(defn inhibition-radius
  "The radius in column space defining neighbouring columns, based on
   the average receptive field size.

   * `sg` is the synapse graph linking the inputs to targets.

   * `topo` is the topology of the targets (e.g. columns).

   * `itopo` is the topology of the inputs."
  [sg topo itopo]
  (let [shared-frac 0.0
        max-dim (apply max (p/dimensions topo))
        max-idim (apply max (p/dimensions itopo))
        arfs (avg-receptive-field-size sg topo itopo)
        ;; columns in this range will have some overlap of inputs
        cols-diameter (* max-dim (/ arfs max-idim))
        cols-radius (quot cols-diameter 2)]
    ;; to share a given fraction of receptive fields
    (-> (* cols-radius (- 1.0 shared-frac))
        (round)
        (max 1))))

(defn inhibit-globally
  "Returns the set of column ids which should become active given the
   map of column excitations `exc`, and the target activation rate
   `level`. Global inhibition is applied, i.e. the top N columns by
   excitation are selected."
  [exc n-on]
  (util/top-n-keys-by-value n-on exc))

(defn inhibits-exc
  "Threshold excitation level at which a cell with excitation `x`
   inhibits a neighbour cell at a distance `dist` columns away."
  ^double [^double x ^double dist ^double max-dist ^double base-dist]
  (let [z (- 1.0 (/ (max 0.0 (- dist base-dist))
                    (max 1.0 (- max-dist base-dist))))]
    (* x z)))

(defn map->vec
  [n m]
  (mapv m (range n)))

(defn vec->map
  [v]
  (persistent!
   (reduce-kv (fn [m i x]
                (if x
                  (assoc! m i x)
                  m))
              (transient {}) v)))

(defn- mask-out-inhibited-by-col
  [emask col x topo inh-radius inh-base-dist]
  (let [coord (p/coordinates-of-index topo col)
        x (double x)
        inh-radius (double inh-radius)
        inh-base-dist (double inh-base-dist)]
    (loop [nbs (p/neighbours topo coord (int inh-radius) 0)
           emask emask]
      (if-let [nb-coord (first nbs)]
        (let [nb-col (p/index-of-coordinates topo nb-coord)]
          (if-let [nb-x (emask nb-col)]
            (let [dist (double (p/coord-distance topo coord nb-coord))]
              (if (<= nb-x (inhibits-exc x dist inh-radius inh-base-dist))
                (recur (next nbs)
                       (assoc! emask nb-col nil))
                (recur (next nbs) emask)))
            ;; neighbour has no overlap or was eliminated
            (recur (next nbs) emask)))
        ;; finished with neighbours
        emask))))

(defn inhibit-locally
  "Returns the set of column ids which should become active given the
   map of column excitations `exc` and the column topology. Applies
   local inhibition to remove any columns dominated by their
   neighbours."
  [exc topo inh-radius inh-base-dist n-on]
  (loop [sel-cols ()
         more-cols (keys (sort-by val > exc))
         emask (transient (map->vec (p/size topo) exc))]
    (if (< (count sel-cols) n-on)
      (if-let [col (first more-cols)]
        (if-let [x (emask col)]
          (recur (conj sel-cols col)
                 (next more-cols)
                 (mask-out-inhibited-by-col emask col x topo inh-radius
                                            inh-base-dist))
          ;; already eliminated, skip
          (recur sel-cols (next more-cols) emask))
        ;; finished
        sel-cols)
      ;; reached target level of activation
      sel-cols)))
