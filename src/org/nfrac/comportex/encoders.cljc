(ns org.nfrac.comportex.encoders
  "Methods of encoding data as distributed bit sets, for feeding as
  input to HTM layers."
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.topography :as topo]
            [org.nfrac.comportex.util :as util :refer [abs spec-finite]]
            [clojure.test.check.random :as random]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]))

(s/def ::n-active-bits
  (-> pos-int? (s/with-gen #(s/gen (s/int-in 0 600)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selectors
;;; Implemented as values not functions for serializability.

(extend-protocol cx/PSelector
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core.Keyword)
  (extract [this state]
    (get state this))
  #?(:clj clojure.lang.IPersistentVector
     :cljs cljs.core.PersistentVector)
  (extract [this state]
    (get-in state this)))

(defrecord VecSelector
    [selectors]
  cx/PSelector
  (extract [_ state]
    (mapv cx/extract selectors (repeat state))))

(defn vec-selector
  [& selectors]
  (->VecSelector selectors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoding

(defn prediction-stats
  [x-bits bit-votes total-votes]
  ;; calculate overlaps of prediction `x` bits with all votes
  (let [o-votes (select-keys bit-votes x-bits)
        total-o-votes (apply + (vals o-votes))
        o-bits (keys o-votes)]
    {:bit-coverage (/ (count o-bits)
                      (max 1 (count x-bits)))
     :bit-precision (/ (count o-bits)
                       (max 1 (count bit-votes)))
     :votes-frac (/ total-o-votes
                    (max 1 total-votes))
     :votes-per-bit (/ total-o-votes
                       (max 1 (count x-bits)))}))

(defn decode-by-brute-force
  [e try-values bit-votes]
  (let [total-votes (apply + (vals bit-votes))]
    (when (pos? total-votes)
      (->> try-values
           (map (fn [x]
                  (let [x-bits (cx/encode e x)]
                    (-> (prediction-stats x-bits bit-votes total-votes)
                        (assoc :value x)))))
           (filter (comp pos? :votes-frac))
           (sort-by (juxt :votes-frac :bit-coverage :bit-precision))
           reverse))))

(defn unaligned-bit-votes
  [widths aligned]
  (let [[is vs] (->> aligned
                     (into (sorted-map))
                     ((juxt keys vals)))
        partitioned-is (util/unalign-indices widths is)
        partitioned-vs (util/splits-at (map count partitioned-is) vs)]
    (map zipmap partitioned-is partitioned-vs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; concat encoder

(defn concat-encode
  [xs encoders]
  (let [bit-widths (map cx/size-of encoders)]
    (->> xs
         (map cx/encode encoders)
         (util/align-indices bit-widths))))

(s/fdef concat-encode
        :args (-> (s/cat :xs coll?
                         :encoders (s/coll-of ::cx/encoder))
                  (s/and #(= (count (:xs %)) (count (:encoders %)))))
        :ret ::cx/bits)

(defrecord ConcatEncoder
    [encoders]
  cx/PTopographic
  (topography [_]
    (let [dim (->> (map cx/dims-of encoders)
                   (apply topo/combined-dimensions))]
      (topo/make-topography dim)))
  cx/PEncoder
  (encode* [_ xs]
   (concat-encode xs encoders))
  (decode*
    [_ bit-votes n-values]
    (let [bit-widths (map cx/size-of encoders)]
      (map #(cx/decode % %2 n-values)
           encoders
           (unaligned-bit-votes bit-widths bit-votes))))
  (input-generator
   [_]
   (apply gen/tuple (map cx/input-generator encoders))))

(defn encat
  "Returns an encoder for a sequence of values, where each is encoded
  separately before the results are concatenated into a single
  sense. Each value by index is passed to the corresponding index of
  `encoders`."
  [encoders]
  (->ConcatEncoder encoders))

(s/fdef encat
        :args (s/cat :encoders (s/coll-of ::cx/encoder :min-count 1))
        :ret ::cx/encoder)

(defmethod cx/encoder-spec ConcatEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var encat))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; splat encoder

(defn splat-encode
  [xs encoder]
  (->> xs
       (mapcat (partial cx/encode encoder))
       (distinct)))

(defrecord SplatEncoder
    [encoder]
  cx/PTopographic
  (topography [_]
    (cx/topography encoder))
  cx/PEncoder
  (encode* [_ xs]
    (splat-encode xs encoder))
  (decode* [_ bit-votes n-values]
    (cx/decode encoder bit-votes n-values))
  (input-generator [_]
    (cx/input-generator encoder)))

(defn ensplat
  "Returns an encoder for a sequence of values. The given encoder will
  be applied to each value, and the resulting encodings
  overlaid (splatted together), taking the union of the sets of bits."
  [encoder]
  (->SplatEncoder encoder))

(s/fdef ensplat
        :args (s/cat :encoder ::cx/encoder)
        :ret ::cx/encoder)

(defmethod cx/encoder-spec ConcatEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var ensplat))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linear encoder

(defn linear-encode
  "truncates"
  [x lower upper n-bits n-active]
  (assert (number? x))
  (let [x (double x)
        span (double (- upper lower))
        x (-> x (max lower) (min upper))
        z (/ (- x lower) span)
        i (long (* z (- n-bits n-active)))]
    (range i (+ i n-active))))

(defn linear-periodic-encode
  "wraps"
  [x lower upper n-bits n-active]
  (assert (number? x))
  (let [x (double x)
        span (double (- upper lower))
        z (/ (- x lower) span)
        z (mod z 1.0)
        i (long (* z n-bits))
        i-end (+ i n-active)]
    (concat
     ;; overflow
     (when (> i-end n-bits)
       (range (- i-end n-bits)))
     (range i (min i-end n-bits)))))

(defrecord LinearEncoder
    [topo n-active lower upper periodic?]
  cx/PTopographic
  (topography [_]
    topo)
  cx/PEncoder
  (encode*
    [_ x]
    (if x
      (let [n-bits (topo/size topo)]
        (if periodic?
          (linear-periodic-encode x lower upper n-bits n-active)
          (linear-encode x lower upper n-bits n-active)))
      (sequence nil)))
  (decode*
    [this bit-votes n]
    (let [span (double (- upper lower))
          values (range lower upper (if (< 5 span 250)
                                      1
                                      (/ span 50)))]
      (->> (decode-by-brute-force this values bit-votes)
           (take n))))
  (input-generator
   [_]
   (gen/double* {:min lower
                 :max upper
                 :NaN? false})))

(defn linear-encoder
  "Returns a simple encoder for a single number. It encodes a number
  by its position on a continuous scale within a numeric range.

  * `dimensions` is the size of the encoder in bits along one or more
    dimensions, a vector e.g. [500].

  * `n-active` is the number of bits to be active.

  * `[lower upper]` gives the numeric range to cover. The input number
    will be clamped or wrapped to this range."
  ([dimensions n-active [lower upper]]
   (linear-encoder dimensions n-active [lower upper] false))
  ([dimensions n-active [lower upper] periodic?]
   (let [topo (topo/make-topography dimensions)]
     (map->LinearEncoder {:topo topo
                          :n-active n-active
                          :lower lower
                          :upper upper
                          :periodic? periodic?}))))

(s/fdef linear-encoder
        :args (s/and
               (s/cat :dimensions ::topo/pos-dimensions
                      :n-active ::n-active-bits
                      :lower-upper (s/and (s/tuple (spec-finite) (spec-finite))
                                          (fn [[a b]] (< a b)))
                      :periodic? (s/? boolean?))
               #(< (:n-active %) (reduce * (:dimensions %))))
        :ret ::cx/encoder)

(defmethod cx/encoder-spec LinearEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var linear-encoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; category encoder

(defn category-encode
  [x value->index n-bits]
  (assert (contains? value->index x))
  (if-not (nil? x)
    (let [idx (util/getx value->index x)
          n-active (quot n-bits (count value->index))
          i (* idx n-active)]
      (range i (+ i n-active)))
    (sequence nil)))

(defrecord CategoryEncoder
    [topo value->index]
  cx/PTopographic
  (topography [_]
    topo)
  cx/PEncoder
  (encode*
    [_ x]
    (if (nil? x)
      ()
      (category-encode x value->index (topo/size topo))))
  (decode*
    [this bit-votes n]
    (->> (decode-by-brute-force this (keys value->index) bit-votes)
         (take n)))
  (input-generator
   [_]
   (gen/elements (keys value->index))))

(defn category-encoder
  [dimensions values]
  (let [topo (topo/make-topography dimensions)]
    (map->CategoryEncoder {:topo topo
                           :value->index (zipmap values (range))})))

(s/fdef category-encoder
        :args (s/cat :dimensions ::topo/pos-dimensions
                     :values (s/coll-of (s/with-gen some? #(gen/simple-type-printable))
                                        :min-count 1 :distinct true))
        :ret ::cx/encoder)

(defmethod cx/encoder-spec CategoryEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var category-encoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; no encoder (pass-through)

(defrecord NoEncoder
    [topo]
  cx/PTopographic
  (topography [_]
    topo)
  cx/PEncoder
  (encode*
    [_ x]
    (s/assert (let [n (topo/size topo)]
                (s/every (s/and ::cx/bit #(< % n))))
              x))
  (decode*
    [this bit-votes n]
    [(keys bit-votes)])
  (input-generator
   [_]
   (let [n (topo/size topo)]
     (gen/vector-distinct (gen/large-integer* {:min 0 :max (dec n)})
                          {:min-elements 1}))))

(defn no-encoder
  [dimensions]
  (let [topo (topo/make-topography dimensions)]
    (map->NoEncoder {:topo topo})))

(s/fdef no-encoder
        :args (s/cat :dimensions ::topo/pos-dimensions)
        :ret ::cx/encoder)

(defmethod cx/encoder-spec NoEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var no-encoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unique encoder

(defn- NaN?
  [x]
  #?(:clj (when (float? x) (Double/isNaN (double x)))
     :cljs (when (number? x) (js/isNaN x))))

(defn unique-sdr
  [x n-bits n-active]
  (let [rngs (-> (random/make-random (hash x))
                 (random/split-n (long (* n-active ;; allow for collisions:
                                          1.25))))]
    (into (list)
          (comp (map #(util/rand-int % n-bits))
                (distinct)
                (take n-active))
          rngs)))

(defn unique-encode
  [x n-bits n-active cache]
  (if (nil? x)
    (sequence nil)
    (or (get @cache x)
        (let [sdr (unique-sdr x n-bits n-active)]
          (get (swap! cache assoc x sdr)
               x)))))

(defrecord UniqueEncoder
    [topo n-active cache]
  cx/PTopographic
  (topography [_]
    topo)
  cx/PEncoder
  (encode*
    [_ x]
    (assert (not (NaN? x)))
    (unique-encode x (topo/size topo) n-active cache))
  (decode*
    [this bit-votes n]
    (->> (decode-by-brute-force this (keys @cache) bit-votes)
         (take n)))
  (input-generator
   [_]
   ;; really anything except nil, but `any` is messy to print
   (gen/such-that #(not (NaN? %))
                  (gen/simple-type-printable))))

(defn unique-encoder
  "This encoder generates a unique bit set for each distinct value,
  based on its hash. `dimensions` is given as a vector."
  [dimensions n-active]
  (let [topo (topo/make-topography dimensions)]
    (map->UniqueEncoder {:topo topo
                         :n-active n-active
                         :cache (atom {})})))

(s/fdef unique-encoder
        :args (s/and
               (s/cat :dimensions ::topo/pos-dimensions
                      :n-active ::n-active-bits)
               #(< (:n-active %) (reduce * (:dimensions %))))
        :ret ::cx/encoder)

(defmethod cx/encoder-spec UniqueEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var unique-encoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linear 2d encoder

(defn linear-2d-encode
  [[x y] topo n-active x-max y-max]
  (if x
    (let [[w h] (topo/dimensions topo)
          x (-> x (max 0) (min x-max))
          y (-> y (max 0) (min y-max))
          xz (/ x x-max)
          yz (/ y y-max)
          xi (long (* xz w))
          yi (long (* yz h))
          coord [xi yi]
          idx (topo/index-of-coordinates topo coord)]
      (->> (range 10)
           (mapcat (fn [radius]
                     (topo/neighbours-indices topo idx radius (dec radius))))
           (take n-active)))
    (sequence nil)))

(defrecord Linear2DEncoder
    [topo n-active x-max y-max]
  cx/PTopographic
  (topography [_]
    topo)
  cx/PEncoder
  (encode*
    [_ [x y]]
    (linear-2d-encode [x y] topo n-active x-max y-max))
  (decode*
    [this bit-votes n]
    (let [values (for [x (range x-max)
                       y (range y-max)]
                   [x y])]
      (->> (decode-by-brute-force this values bit-votes)
           (take n))))
  (input-generator
   [_]
   (gen/tuple (gen/double* {:min 0 :max x-max :NaN? false})
              (gen/double* {:min 0 :max y-max :NaN? false}))))

(defn linear-2d-encoder
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
  [dimensions n-active [x-max y-max]]
  (let [topo (topo/make-topography dimensions)]
    (map->Linear2DEncoder {:topo topo
                           :n-active n-active
                           :x-max x-max
                           :y-max y-max})))

(s/fdef linear-2d-encoder
        :args (s/and
               (s/cat :dimensions (s/and ::topo/pos-dimensions #(= 2 (count %)))
                      :n-active ::n-active-bits
                      :xy-maxs (s/tuple (s/and (spec-finite :min 0) pos?)
                                        (s/and (spec-finite :min 0) pos?)))
               #(< (:n-active %) (reduce * (:dimensions %))))
        :ret ::cx/encoder)

(defmethod cx/encoder-spec Linear2DEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var linear-2d-encoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; coordinate encoder

;; we only support up to 3D. beyond that, perf will be bad anyway.
(defn coordinate-neighbours
  [coord radii]
  (case (count coord)
    1 (let [[cx] coord
            [rx] radii]
        (for [x (range (- cx rx) (+ cx rx 1))]
          [x]))
    2 (let [[cx cy] coord
            [rx ry] radii]
        (for [x (range (- cx rx) (+ cx rx 1))
              y (range (- cy ry) (+ cy ry 1))]
          [x y]))
    3 (let [[cx cy cz] coord
            [rx ry rz] radii]
        (for [x (range (- cx rx) (+ cx rx 1))
              y (range (- cy ry) (+ cy ry 1))
              z (range (- cz rz) (+ cz rz 1))]
          [x y z]))))

(s/fdef coordinate-neighbours
        :args (s/cat :coord (s/coll-of (spec-finite) :min-count 1 :max-count 3)
                     :radii (s/coll-of (spec-finite) :min-count 1 :max-count 3))
        :ret (s/coll-of (spec-finite) :min-count 1 :max-count 3 :kind vector?)
        :fn #(apply = (count (-> % :args :coord))
                    (count (-> % :args :radii))
                    (map count (:ret %))))

(defn coordinate-order
  [coord]
  ;; NOTE it is not enough to take (hash coord) as the seed here,
  ;; (because of hash defn for vectors in cljs?) this leads to the first
  ;; element of the coordinate vector dominating, so e.g. big shifts
  ;; in y coordinate have little effect on encoded bits.
  (-> (random/make-random (hash (str coord)))
      (random/rand-double)))

(defn coordinate-bit
  [size coord]
  ;; take second-split random value to distinguish from coordinate-order
  ;; (otherwise highest orders always have highest bits!)
  (-> (random/make-random (hash (str coord)))
      (random/split)
      (second) ;; impl detail? this is independent from the pre-split rng
      (util/rand-int size)))

(defn coordinate-encode
  [coord n-bits n-active scale-factors radii]
  (when (first coord)
    (let [int-coord (map (comp util/round *) coord scale-factors)
          neighs (coordinate-neighbours int-coord radii)]
      (->> (zipmap neighs (map coordinate-order neighs))
           (util/top-n-keys-by-value n-active)
           (map (partial coordinate-bit n-bits))
           (distinct)))))

(defrecord CoordinateEncoder
    [topo n-active scale-factors radii]
  cx/PTopographic
  (topography [_]
    topo)
  cx/PEncoder
  (encode*
    [_ coord]
    (coordinate-encode coord (topo/size topo) n-active scale-factors radii))
  (input-generator
   [_]
   (apply gen/tuple
     (map (fn [scale radius]
            (gen/double* {:min 0 :max (/ (* radius 10) (abs scale)) :NaN? false}))
          scale-factors
          radii))))

(defn coordinate-encoder
  "Coordinate encoder for integer coordinates, unbounded, with one,
  two or three dimensions. Expects a coordinate, i.e. a sequence of
  numbers with 1, 2 or 3 elements. These raw values will be multiplied
  by corresponding `scale-factors` to obtain integer grid
  coordinates. Each dimension has an associated radius within which
  there is some similarity in encoded SDRs."
  [dimensions n-active scale-factors radii]
  (let [topo (topo/make-topography dimensions)]
    (map->CoordinateEncoder {:topo topo
                             :n-active n-active
                             :scale-factors scale-factors
                             :radii radii})))

(s/fdef coordinate-encoder
        :args (s/and
               (s/cat :dimensions ::topo/pos-dimensions
                      :n-active ::n-active-bits
                      :scale-factors (s/coll-of (s/and (spec-finite)
                                                       (complement zero?))
                                                :min-count 1 :max-count 3)
                      :radii (s/coll-of (s/and (spec-finite :min 0 :max 100) pos?)
                                        :min-count 1 :max-count 3))
               #(< (:n-active %) (reduce * (:dimensions %)))
               #(= (count (:scale-factors %)) (count (:radii %))))

        :ret ::cx/encoder)

(defmethod cx/encoder-spec CoordinateEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var coordinate-encoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sampling Linear Encoder
;;; see http://mrcslws.com/gorilla/?path=hotgym.clj#a_third_way

(defn middle-out-range
  "By example:
  Given 7.2, returns (7, 8, 6, 9, 5, 10, ...),
  Given 7.7, returns (8, 7, 9, 6, 10, 5, ...)"
  [v]
  (let [start (-> v float Math/round long)
        rounded-down? (> v start)
        up (iterate inc start)
        down (iterate dec start)]
    (if rounded-down?
      (interleave down (drop 1 up))
      (interleave up (drop 1 down)))))

(defn multiples-within-radius
  [center radius multiples-of]
  (let [lower-bound (- center radius)
        upper-bound (+ center radius)]
    (->> (middle-out-range (/ center multiples-of))
         (map (partial * multiples-of))
         (take-while #(<= lower-bound
                          %
                          upper-bound)))))

(s/fdef multiples-within-radius
        :args (s/cat :center nat-int?
                     :radius (s/and (spec-finite :min 0) pos?)
                     :multiples-of pos-int?)
        :ret (s/every int?))

(defn handle-multiples-at-edges
  [periodic? n-bits multiples-of coll]
  (if-not periodic?
    (filter #(<= 0 % (dec n-bits)) coll)
    (let [m-wrap (-> (dec n-bits)
                     (quot multiples-of)
                     (inc)
                     (* multiples-of))]
      (map #(mod % m-wrap) coll))))

(s/fdef handle-multiples-at-edges
        :args (s/cat :periodic? boolean?
                     :n-bits pos-int?
                     :multiples-of pos-int?
                     :coll (s/every int?))
        :ret (s/every nat-int?))

(defn into-bounded
  "Move items from `from` to `coll` until its size reaches `max-size`
  or we run out of items. Specifically supports sets and maps, which don't
  always grow when an item is added."
  [coll max-size from]
  (loop [coll coll
         from from]
    (let [n-remaining (- max-size (count coll))]
      (if (and (pos? n-remaining)
               (not-empty from))
        (let [[taken untaken] (split-at n-remaining from)]
          (recur (into coll taken)
                 untaken))
        coll))))

(s/fdef into-bounded
        :args (s/cat :coll coll?
                     :max-size pos-int?
                     :from seqable?)
        :ret coll?)

(defn sampled-window
  "Place a bit in the center.
  Distribute bits around the center until we've used half of the remainder.
  Double the density. Distribute again until we've used half of the remainder.
  Double the density. ...
  Continue until all active bits are distributed or all bits are active.

  Strategically choose bit positions so that the intersections between
  various ranges will select the same bits."
  [center n-bits target-n-active bit-radius periodic?]
  (loop [chosen #{center}
         density (/ (- target-n-active (count chosen))
                    (* 2 bit-radius)
                    2)]
    (let [remaining (- target-n-active (count chosen))]
      (if (and (pos? remaining)
               (pos? (long (/ 1 density))))
        (let [multiples-of (long (/ 1 density))
              half-remaining (quot remaining 2)
              n-take (if (== 1 remaining)
                       remaining
                       half-remaining)]
          (recur (->> (multiples-within-radius center bit-radius multiples-of)
                      (handle-multiples-at-edges periodic? n-bits multiples-of)
                      (into-bounded chosen (+ n-take (count chosen))))
                 (* density 2)))
        chosen))))

(s/fdef sampled-window
        :args (s/and
               (s/cat :center nat-int?
                      :n-bits pos-int?
                      :target-n-active pos-int?
                      :bit-radius (spec-finite :min 0)
                      :periodic? boolean?)
               #(>= (:bit-radius %)
                    (/ (:target-n-active %) 4)))
        :ret (s/every nat-int?))

(defn sampling-linear-encode
  [x n-bits n-active lower upper radius periodic?]
  (if x
    (let [domain-width (- upper lower)
          z (/ (- x lower)
               domain-width)
          center (if periodic?
                   (-> (mod z 1.0)
                       (* (dec n-bits))
                       (long))
                   (-> z (max 0.0) (min 1.0)
                       (* (dec n-bits))
                       (long)))
          bit-radius (* radius
                        (/ n-bits domain-width))] ;; why this?
      (sampled-window center n-bits n-active bit-radius periodic?))
    (sequence nil)))

(defrecord SamplingLinearEncoder
  [topo n-active lower upper radius periodic?]
  cx/PTopographic
  (topography
    [_]
    topo)
  cx/PEncoder
  (encode*
    [_ x]
    (sampling-linear-encode x (topo/size topo) n-active lower upper radius periodic?))
  (decode*
    [this bit-votes n]
    (let [span (double (- upper lower))
          values (range lower upper (if (< 5 span 250)
                                      1
                                      (/ span 50)))]
      (->> (decode-by-brute-force this values bit-votes)
           (take n))))
  (input-generator
   [_]
   (let [span (- upper lower)]
     (gen/double* {:min (- lower (/ span 2))
                   :max (+ upper (/ span 2))
                   :NaN? false}))))

(defn sampling-linear-encoder
  "A linear encoder that samples the surrounding radius, rather than
  activating all of it. Sampling density decreases as distance increases.

  * `dimensions` is the size of the encoder in bits along one or more
    dimensions, a vector e.g. [500].

  * `n-active` is the number of bits to be active.

  * `[lower upper]` gives the numeric range to cover. The input number
    will be clamped or wrapped to this range.

  * `radius` describes the range to sample.

  Recommendations:

  * `lower` and `upper` should be `radius` below and above the actual
    lower and upper bounds. Otherwise the radius will extend off the
    number line, creating representations that behave a bit differently
    from the rest."
  ([dimensions n-active [lower upper] radius]
   (sampling-linear-encoder dimensions n-active [lower upper] radius false))
  ([dimensions n-active [lower upper] radius periodic?]
   (let [topo (topo/make-topography dimensions)]
     (map->SamplingLinearEncoder {:topo topo
                                  :n-active n-active
                                  :lower lower
                                  :upper upper
                                  :radius radius
                                  :periodic? periodic?}))))

(s/def ::sampling-linear-encoder-args
  #_"Args spec for sampling-linear-encoder, without the radius constraint.
  Given an id here for generator use."
  (s/and
   (s/cat :dimensions ::topo/pos-dimensions
          :n-active ::n-active-bits
          :lower-upper (s/and (s/tuple (spec-finite :min -1e12 :max 1e12)
                                       (spec-finite :min -1e12 :max 1e12))
                              (fn [[a b]] (< a b)))
          :radius (s/and (spec-finite :min 0 :max 1e12)
                         pos?)
          :periodic? (s/? boolean?))
   #(< (:n-active %) (reduce * (:dimensions %)))))

(defn- sle-radius-range
  [dimensions n-active [lower upper]]
  (let [n-bits (reduce * dimensions)
        span (- upper lower)
        fuzz 0.001
        radius-min (* span (/ n-active n-bits 4)
                      (+ 1 fuzz))]
    [radius-min (* span 10)]))

(s/fdef
 sampling-linear-encoder
 :args (->
        (s/and ::sampling-linear-encoder-args
               #(let [[rmin rmax] (sle-radius-range (:dimensions %) (:n-active %)
                                                    (:lower-upper %))]
                  (<= rmin (:radius %) rmax)))
        (s/with-gen
          (fn []
            (gen/bind
             (s/gen ::sampling-linear-encoder-args)
             (fn [[dimensions n-active lower-upper _ periodic? :as args]]
               (let [[rmin rmax] (sle-radius-range dimensions n-active
                                                   lower-upper)
                     radius-gen (gen/double* {:min rmin :max rmax :NaN? false})]
                 (gen/fmap #(-> (take 3 args) (vec) (conj %) (conj periodic?))
                           radius-gen)))))))
 :ret ::cx/encoder)

(defmethod cx/encoder-spec SamplingLinearEncoder [_]
  (s/with-gen (s/keys) #(util/fn->generator (var sampling-linear-encoder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sensors

(defn sensor-cat
  [& sensors]
  (let [selectors (map first sensors)
        encoders (map second sensors)]
    [(apply vec-selector selectors)
     (encat encoders)]))

(s/fdef sensor-cat
        :args (s/coll-of ::cx/sensor :min-count 1)
        :ret ::cx/sensor)
