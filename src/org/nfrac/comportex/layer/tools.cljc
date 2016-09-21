(ns org.nfrac.comportex.layer.tools
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.topography :as topo]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.util :as util]
            [clojure.set :as set]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]))

(defn source-of-distal-bit
  "Returns [src-id j] where src-id may be a layer key or sense key, and j is
  the index into the output of the source."
  [htm lyr-id i]
  (let [lyr (get-in htm [:layers lyr-id])
        params (cx/params lyr)
        [src-type j] (layer/id->source params (:embedding lyr) i)]
    (case src-type
      :this [lyr-id i]
      :lat (cx/source-of-incoming-bit htm lyr-id j :lat-deps))))

(defn source-of-apical-bit
  "Returns [src-id j] where src-id is a layer key, and j is the index into the
  output of the source layer."
  [htm lyr-id i]
  (cx/source-of-incoming-bit htm lyr-id i :fb-deps))

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
        ff-bits (-> active-state :in-ff-signal :bits set)
        ff-s-bits (-> active-state :in-ff-signal ::layer/stable-bits set)
        ff-b-bits (set/difference ff-bits ff-s-bits)
        distal-bits (:active-bits distal-state)
        apical-bits (:active-bits apical-state)
        ff-bits-srcs (into {}
                           (map (fn [i]
                                  (let [[k _] (cx/source-of-incoming-bit
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
                                         (syn/sources-connected-to psg ff-seg-path))
                       active-ff-b (->> (filter ff-b-bits ff-conn-sources)
                                        (zap-fewer ff-stim-thresh))
                       active-ff-s (->> (filter ff-s-bits ff-conn-sources)
                                        (zap-fewer ff-stim-thresh))
                       ff-b-by-src (frequencies (map ff-bits-srcs active-ff-b))
                       ff-s-by-src (frequencies (map ff-bits-srcs active-ff-s))
                       ;; breakdown of distal excitation by source
                       [d-seg-path _] (get (:fully-matching-segs distal-state) cell-id)
                       d-conn-sources (when d-seg-path
                                        (syn/sources-connected-to dsg d-seg-path))
                       active-d (->> (filter distal-bits d-conn-sources)
                                     (zap-fewer d-stim-thresh))
                       d-by-src (->> (frequencies (map distal-bits-srcs active-d))
                                     (util/remap #(* % distal-weight)))
                       ;; same for apical
                       [a-seg-path _] (get (:fully-matching-segs apical-state) cell-id)
                       a-conn-sources (when a-seg-path
                                        (syn/sources-connected-to asg a-seg-path))
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
