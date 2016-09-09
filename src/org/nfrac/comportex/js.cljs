(ns org.nfrac.comportex.js
  "Minimal public javascript API.

  Complex clojurescript objects such as encoders and HTM models and their
  constituent layers are not converted to native javascript types.
  They are left as black box values intended to be used with other API fns."
  (:require
    [org.nfrac.comportex.hierarchy :as hier]
    [org.nfrac.comportex.encoders :as encoders]
    [org.nfrac.comportex.protocols :as p]
    [org.nfrac.comportex.layer :as layer]
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

(defn js->sensors
  [sensors]
  (into {} (for [k (js-keys sensors)]
              (let [[selector encoder] (aget sensors k)]
                 [(keyword k)
                  [(js->selector selector)
                    ;; do not convert encoders
                   encoder]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy

(defn ^:export layers-in-series
  "Constructs an HTM network consisting of n layers in a linear
  series. The layers are given keys :layer-a, :layer-b, etc. Senses feed
  only to the first layer. Their sensors are given in a map with
  keyword keys. Sensors are defined to be the form `[selector encoder]`.

  Encoders should be passed as clojure objects (as from encoder fns).
  Selectors should be passed as strings or arrays of strings, which
  select the value at that key or nested path of keys in an input value."
  ([n paramseq sensors]
   (layers-in-series n paramseq sensors nil))
  ([n paramseq main-sensors motor-sensors]
   (let [build-layer layer/layer-of-cells
         paramseq (map js->params paramseq)
         main-sensors (js->sensors main-sensors)
         motor-sensors (js->sensors motor-sensors)]
     (hier/layers-in-series n build-layer paramseq main-sensors motor-sensors))))

(defn ^:export layer-seq
  "Returns a js array of layers, each a clojure object."
  [htm]
  (->array (hier/layer-seq htm)))

(defn ^:export column-state-freqs
  "Returns a map with the frequencies of columns in states
  `active` (bursting), `predicted`, `active-predicted`. Note that
  these are distinct categories. The names are possibly misleading."
  [lyr]
  (clj->js (hier/column-state-freqs lyr)))

(defn ^:export predictions
  [htm sense-id n-predictions]
  (clj->js (hier/predictions htm (keyword sense-id) n-predictions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Protocols fns

(defn ^:export htm-step
  "Compute the next time step. Pass `htm` as a clojure object, but `inval` as
  a js value which will be converted to a clojurescript value, including
  keywordizing keys."
  [htm inval]
  (p/htm-step htm (js->clj inval :keywordize-keys true)))

(defn ^:export params
  [lyr]
  (params->js (p/params lyr)))

(defn ^:export encode
  [encoder x]
  (clj->js (p/encode encoder (js->clj x))))

(defn ^:export decode
  [encoder bit-votes n]
  (clj->js (p/decode encoder (js->clj bit-votes) n)))

(defn ^:export timestep
  [htm]
  (p/timestep htm))

(defn ^:export bursting-columns
  "The set of bursting column ids."
  [lyr]
  (clj->js (:bursting-columns (p/layer-state lyr))))

(defn ^:export active-columns
  "The set of active column ids."
  [lyr]
  (clj->js (:active-columns (p/layer-state lyr))))

(defn ^:export active-cells
  "The set of active cell ids."
  [lyr]
  (clj->js (:active-cells (p/layer-state lyr))))

(defn ^:export predictive-cells
  "The set of predictive cell ids derived from the current active
  cells. If the depolarise phase has not been applied yet, returns
  nil."
  [lyr]
  (clj->js (:predictive-cells (p/layer-state lyr))))

(defn ^:export prior-predictive-cells
  [lyr]
  (clj->js (:prior-predictive-cells (p/layer-state lyr))))

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
