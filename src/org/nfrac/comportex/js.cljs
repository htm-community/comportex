(ns org.nfrac.comportex.js
  "Minimal public javascript API.

  Complex clojurescript objects such as encoders and HTM networks and their
  constituent layers are not converted to native javascript types.
  They are left as black box values intended to be used with other API fns."
  (:require
    [org.nfrac.comportex.core :as cx]
    [org.nfrac.comportex.encoders :as encoders]
    [org.nfrac.comportex.layer :as layer]
    [org.nfrac.comportex.util :as util]
    [clojure.spec :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defn js->params
  [params]
  ;; TODO: handle keyword values such as for :spatial-pooling key
  (js->clj params :keywordize-keys true))

(defn params->js
  [params]
  (clj->js params))

(defn js->selector
  [selector]
  (cond
   (satisfies? cx/PSelector selector) selector
   (string? selector) (keyword selector)
   (array? selector) (mapv js->selector selector)
   :else (throw (js/Error. (str "unknown selector " selector)))))

(defn ->array
  [v]
  (let [arr (array)]
    (doseq [x v]
      (.push arr x))
    arr))

(defn js->kw-keys
  [js-map]
  (reduce (fn [m k]
            (assoc m (keyword k) (aget js-map k)))
          {}
          (js-keys js-map)))

(defn js->sensors
  [sensors]
  (into {} (for [k (js-keys sensors)]
              (let [[selector encoder] (aget sensors k)]
                 [(keyword k)
                  [(js->selector selector)
                    ;; do not convert encoders
                   encoder]]))))

(defn js->linkages
  [linkages]
  (->> (js->clj linkages :keywordize-keys true)
       (util/remap (fn [deps]
                     (util/remap #(mapv keyword %) deps)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTM layer

(defn ^:export htm-layer
  [params]
  (layer/layer-of-cells (js->params params)))

(defn ^:export column-state-freqs
  "Returns a map with the frequencies of columns in states
  `active` (bursting), `predicted`, `active-predicted`. Note that
  these are distinct categories. The names are possibly misleading."
  [lyr]
  (clj->js (layer/column-state-freqs lyr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core

(defn ^:export network
  "Builds a network of layers and senses with the given linkages.
  Linkages between these nodes are given as direct dependencies:
  :ff-deps maps each layer to a list of nodes it takes feed-forward
  input from. Optionally, :fb-deps maps layers to lists of nodes to
  take feed-back input from. And :lat-deps lateral sources, which
  may also be senses (like motor senses).

  Sensors are defined to be the form `[selector encoder]`, satisfying
  protocols PSelector and PEncoder respectively.

  js: Encoders should be passed as clojure objects (as from encoder fns).
  Selectors should be passed as strings or arrays of strings, which
  select the value at that key or nested path of keys in an input value.

  For each layer, the combined dimensions of each of its feed-forward
  sources, feed-back sources, and lateral sources are calculated and
  passed on to layer-embed to allow the layer to configure itself.

  For example a feed-forward network `inp -> v1 -> v2`:

  network({v1: v1Layer,
           v2: v2Layer},
          {inp: [sel, enc]},
          {ff-deps: {v1: ['inp'],
                     v2: ['v1']}})"
  ([layers sensors]
   (cx/network (js->kw-keys layers)
               (js->sensors sensors)))
  ([layers sensors linkages]
   (cx/network (js->kw-keys layers)
               (js->sensors sensors)
               (js->linkages linkages))))

(defn ^:export layer-seq
  "Returns a js array of layers, each a clojure object."
  [htm]
  (->array (cx/layer-seq htm)))

(defn ^:export predictions
  [htm sense-id n-predictions opts]
  (clj->js (cx/predictions htm (keyword sense-id) n-predictions
                           (js->clj opts :keywordize-keys true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Protocols fns

(defn ^:export htm-step
  "Compute the next time step. Pass `htm` as a clojure object, but `inval` as
  a js value which will be converted to a clojurescript value, including
  keywordizing keys."
  [htm inval]
  (cx/htm-step htm (js->clj inval :keywordize-keys true)))

(defn ^:export params
  [lyr]
  (params->js (cx/params lyr)))

(defn ^:export encode
  [encoder x]
  (clj->js (cx/encode encoder (js->clj x))))

(defn ^:export decode
  [encoder bit-votes n]
  (clj->js (cx/decode encoder (js->clj bit-votes) n)))

(defn ^:export timestep
  [htm]
  (cx/timestep htm))

(defn ^:export layer-state
  [lyr]
  (clj->js (cx/layer-state lyr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Encoders

(defn ^:export encat
  "Returns an encoder for a sequence of values, where each is encoded
  separately before the results are concatenated into a single
  sense. Each value by index is passed to the corresponding index of
  `encoders`."
  [encoders]
  (encoders/encat encoders))

(defn ^:export ensplat
  "Returns an encoder for a sequence of values. The given encoder will
  be applied to each value, and the resulting encodings
  overlaid (splatted together), taking the union of the sets of bits."
  [encoders]
  (encoders/ensplat encoders))

(defn ^:export linear-encoder
  [dimensions n-active lower upper]
  (encoders/linear-encoder (vec dimensions) n-active [lower upper]))

(defn ^:export category-encoder
  [dimensions values]
  (encoders/category-encoder (vec dimensions) (js->clj values)))

(defn ^:export no-encoder
  [dimensions]
  (encoders/no-encoder (vec dimensions)))

(defn ^:export unique-encoder
  "This encoder generates a unique bit set for each distinct value,
  based on its hash. `dimensions` is given as a vector."
  [dimensions n-active]
  (encoders/unique-encoder (vec dimensions) n-active))

(defn ^:export linear-2d-encoder
  "Returns a simple encoder for a tuple of two numbers representing a
  position in rectangular bounds. The encoder maps input spatial
  positions to boxes of active bits in corresponding spatial positions
  of the encoded sense. So input positions close in both coordinates
  will have overlapping bit sets.

  * `dimensions` - of the encoded bits, given as a vector [nx ny].

  * `n-active` is the number of bits to be active.

  * `[x-max y-max]` gives the numeric range of input space to
  cover. The numbers will be clamped to this range, and below by
  zero."
  [dimensions n-active x-max y-max]
  (encoders/linear-2d-encoder (vec dimensions) n-active [x-max y-max]))

(defn ^:export coordinate-encoder
  "Coordinate encoder for integer coordinates, unbounded, with one,
  two or three dimensions. Expects a coordinate, i.e. a sequence of
  numbers with 1, 2 or 3 elements. These raw values will be multiplied
  by corresponding `scale-factors` to obtain integer grid
  coordinates. Each dimension has an associated radius within which
  there is some similarity in encoded SDRs."
  [dimensions n-active scale-factors radii]
  (encoders/coordinate-encoder (vec dimensions) n-active (vec scale-factors) (vec radii)))

(defn ^:export sampling-linear-encoder
  [dimensions n-active lower upper radius]
  (encoders/sampling-linear-encoder (vec dimensions) n-active [lower upper] radius))

(defn ^:export sensor-cat
  [& sensors]
  (encoders/sensor-cat sensors))
