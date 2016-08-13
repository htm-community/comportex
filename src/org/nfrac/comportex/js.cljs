(ns org.nfrac.comportex.js
  (:require
    [org.nfrac.comportex.core :as core]
    [org.nfrac.comportex.encoders :as encoders]
    [org.nfrac.comportex.protocols :as p]))

;; Minimal public API

;; Complex clojurescript objects such as encoders and HTM models and their
;; constituent regions and layers are not converted to native javascript types.
;; They are left as black box values intended to be used with other API fns.

;;  `spec` is the parameter specification map.

(defn js->spec
  [spec]
  ;; TODO: handle keyword values such as for :spatial-pooling key
  (js->clj spec :keywordize-keys true))

(defn js->selector
  [selector]
  (cond
   (satisfies? p/PSelector selector) selector
   (string? selector) (keyword selector)
   (array? selector) (mapv js->selector selector)
   :else (throw (js/Error. (str "unknown selector " selector)))))

(defn ->array
  [v]
  (let [arr (array)]
    (doseq [x v]
      (.push arr x))
    arr))

;; .core

;; ===================================================================

(defn ^:export regions-in-series
  "Constructs an HTM network consisting of n regions in a linear
  series. The regions are given keys :rgn-0, :rgn-1, etc. Senses feed
  only to the first region. Their sensors are given in a map with
  keyword keys. Sensors are defined to be the form `[selector encoder]`.
  Encoders should be passed as clojure objects (as from encoder fns).
  Selectors should be passed as strings or arrays of strings, which
  select the value at that key or nested path of keys in an input value."
  [n specs sensors]
  (let [build-region core/sensory-region
        specs (map js->spec specs)
        sensors (into {} (for [k (js-keys sensors)]
                           (let [[selector encoder] (aget sensors k)]
                             [(keyword k)
                              [(js->selector selector)
                               ;; do not convert encoders
                               encoder]])))]
    (core/regions-in-series n build-region specs sensors)))

;; ===================================================================

(defn ^:export region-seq
  "Returns a js array of regions, each a clojure object."
  [htm]
  (->array (core/region-seq htm)))

(defn ^:export layer-seq
  "Returns a js array of layers in a region, each a clojure object.
   If an HTM model is passed instead, returns layers from all regions flattened
   into a single array."
  [rgn-or-htm]
  (if (:regions rgn-or-htm)
    (->array (mapcat layer-seq (core/region-seq rgn-or-htm)))
    (->array (map #(get rgn-or-htm %) (core/layers rgn-or-htm)))))

(defn ^:export bursting-columns
  "The set of bursting column ids."
  [lyr]
  (clj->js (p/bursting-columns lyr)))

(defn ^:export active-columns
  "The set of active column ids."
  [lyr]
  (clj->js (p/active-columns lyr)))

(defn ^:export active-cells
  "The set of active cell ids."
  [lyr]
  (clj->js (p/active-cells lyr)))

(defn ^:export predictive-cells
  "The set of predictive cell ids derived from the current active
  cells. If the depolarise phase has not been applied yet, returns
  nil."
  [lyr]
  (clj->js (p/predictive-cells lyr)))

(defn ^:export prior-predictive-cells
  [lyr]
  (clj->js (p/prior-predictive-cells lyr)))

;; ===================================================================

(defn ^:export column-state-freqs
  "Returns a map with the frequencies of columns in states
  `active` (bursting), `predicted`, `active-predicted`. Note that
  these are distinct categories. The names are possibly misleading."
  [rgn]
  (clj->js (core/column-state-freqs rgn)))

;; ===================================================================

(defn ^:export predictions
  [htm sense-id n-predictions]
  (clj->js (core/predictions htm (keyword sense-id) n-predictions)))

;; protocols
;; mostly just protocols (ie. interfaces)

(defn ^:export htm-step
  "Compute the next time step. Pass `htm` as a clojure object, but `inval` as
  a js value which will be converted to a clojurescript value, including
  keywordizing keys."
  [htm inval]
  (p/htm-step htm (js->clj inval :keywordize-keys true)))

(defn ^:export encode
  [encoder x]
  (clj->js (p/encode encoder (js->clj x))))

(defn ^:export decode
  [encoder bit-votes n]
  (clj->js (p/decode encoder (js->clj bit-votes) n)))

(defn ^:export timestep
  [htm]
  (p/timestep htm))

;; encoders

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
