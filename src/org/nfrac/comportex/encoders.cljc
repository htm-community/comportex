(ns org.nfrac.comportex.encoders
  "Methods of encoding data as distributed bit sets, for feeding as
   input to a cortical region."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util]
            [clojure.test.check.random :as random]))

;;; # Selectors
;;; Implemented as values not functions for serializability.

(extend-protocol p/PSelector
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
  p/PSelector
  (extract [_ state]
    (mapv p/extract selectors (repeat state))))

(defn vec-selector
  [& selectors]
  (->VecSelector selectors))

;;; # Decoding

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
                  (let [x-bits (p/encode e x)]
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

;;; # Encoders

(defrecord ConcatEncoder
    [encoders]
  p/PTopological
  (topology [_]
    (let [dim (->> (map p/dims-of encoders)
                   (apply topology/combined-dimensions))]
      (topology/make-topology dim)))
  p/PEncoder
  (encode
    [_ xs]
    (let [bit-widths (map p/size-of encoders)]
      (->> xs
           (map p/encode encoders)
           (util/align-indices bit-widths))))
  (decode
    [_ bit-votes n-values]
    (let [bit-widths (map p/size-of encoders)]
      (map #(p/decode % %2 n-values)
           encoders
           (unaligned-bit-votes bit-widths bit-votes)))))

(defn encat
  "Returns an encoder for a sequence of values, where each is encoded
  separately before the results are concatenated into a single
  sense. Each value by index is passed to the corresponding index of
  `encoders`."
  [encoders]
  (->ConcatEncoder encoders))

(defrecord SplatEncoder
    [encoder]
  p/PTopological
  (topology [_]
    (p/topology encoder))
  p/PEncoder
  (encode
    [_ xs]
    (->> xs
         (mapcat (partial p/encode encoder))
         (distinct)))
  (decode [_ bit-votes n-values]
    (p/decode encoder bit-votes n-values)))

(defn ensplat
  "Returns an encoder for a sequence of values. The given encoder will
  be applied to each value, and the resulting encodings
  overlaid (splatted together), taking the union of the sets of bits."
  [encoder]
  (->SplatEncoder encoder))

(defn linear-bits
  "truncates"
  [x lower upper n-bits n-active]
  (let [span (double (- upper lower))
        x (-> x (max lower) (min upper))
        z (/ (- x lower) span)
        i (long (* z (- n-bits n-active)))]
    (range i (+ i n-active))))

(defn periodic-linear-bits
  "wraps"
  [x lower upper n-bits n-active]
  (let [span (double (- upper lower))
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
  p/PTopological
  (topology [_]
    topo)
  p/PEncoder
  (encode
    [_ x]
    (if x
      (let [n-bits (p/size topo)]
        (if periodic?
          (periodic-linear-bits x lower upper n-bits n-active)
          (linear-bits x lower upper n-bits n-active)))
      (sequence nil)))
  (decode
    [this bit-votes n]
    (let [span (double (- upper lower))
          values (range lower upper (if (< 5 span 250)
                                      1
                                      (/ span 50)))]
      (->> (decode-by-brute-force this values bit-votes)
           (take n)))))

(defn linear-encoder
  "Returns a simple encoder for a single number. It encodes a number
  by its position on a continuous scale within a numeric range.

  * `dimensions` is the size of the encoder in bits along one or more
    dimensions, a vector e.g. [500].

  * `n-active` is the number of bits to be active.

  * `[lower upper]` gives the numeric range to cover. The input number
    will be clamped to this range."
  ([dimensions n-active [lower upper]]
   (linear-encoder dimensions n-active [lower upper] false))
  ([dimensions n-active [lower upper] periodic?]
   (let [topo (topology/make-topology dimensions)]
     (map->LinearEncoder {:topo topo
                          :n-active n-active
                          :lower lower
                          :upper upper
                          :periodic? periodic?}))))

(defrecord CategoryEncoder
    [topo value->index]
  p/PTopological
  (topology [_]
    topo)
  p/PEncoder
  (encode
    [_ x]
    (if-let [idx (value->index x)]
      (let [n-bits (p/size topo)
            n-active (quot n-bits (count value->index))
            i (* idx n-active)]
        (range i (+ i n-active)))
      (sequence nil)))
  (decode
    [this bit-votes n]
    (->> (decode-by-brute-force this (keys value->index) bit-votes)
         (take n))))

(defn category-encoder
  [dimensions values]
  (let [topo (topology/make-topology dimensions)]
    (map->CategoryEncoder {:topo topo
                           :value->index (zipmap values (range))})))

(defrecord NoEncoder
    [topo]
  p/PTopological
  (topology [_]
    topo)
  p/PEncoder
  (encode
    [_ x]
    x)
  (decode
    [this bit-votes n]
    [(keys bit-votes)]))

(defn no-encoder
  [dimensions]
  (let [topo (topology/make-topology dimensions)]
    (map->NoEncoder {:topo topo})))

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

(defrecord UniqueEncoder
    [topo n-active cache]
  p/PTopological
  (topology [_]
    topo)
  p/PEncoder
  (encode
    [_ x]
    (if (nil? x)
      (sequence nil)
      (or (get @cache x)
          (let [sdr (unique-sdr x (p/size topo) n-active)]
            (get (swap! cache assoc x sdr)
                 x)))))
  (decode
    [this bit-votes n]
    (->> (decode-by-brute-force this (keys @cache) bit-votes)
         (take n))))

(defn unique-encoder
  "This encoder generates a unique bit set for each distinct value,
  based on its hash. `dimensions` is given as a vector."
  [dimensions n-active]
  (let [topo (topology/make-topology dimensions)]
    (map->UniqueEncoder {:topo topo
                         :n-active n-active
                         :cache (atom {})})))

(defrecord Linear2DEncoder
    [topo n-active x-max y-max]
  p/PTopological
  (topology [_]
    topo)
  p/PEncoder
  (encode
    [_ [x y]]
    (if x
      (let [[w h] (p/dimensions topo)
            x (-> x (max 0) (min x-max))
            y (-> y (max 0) (min y-max))
            xz (/ x x-max)
            yz (/ y y-max)
            xi (long (* xz w))
            yi (long (* yz h))
            coord [xi yi]
            idx (p/index-of-coordinates topo coord)]
        (->> (range 10)
             (mapcat (fn [radius]
                       (p/neighbours-indices topo idx radius (dec radius))))
             (take n-active)))
      (sequence nil)))
  (decode
    [this bit-votes n]
    (let [values (for [x (range x-max)
                       y (range y-max)]
                   [x y])]
      (->> (decode-by-brute-force this values bit-votes)
           (take n)))))

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
  (let [topo (topology/make-topology dimensions)]
    (map->Linear2DEncoder {:topo topo
                           :n-active n-active
                           :x-max x-max
                           :y-max y-max})))

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

(defrecord CoordinateEncoder
    [topo n-active scale-factors radii]
  p/PTopological
  (topology [_]
    topo)
  p/PEncoder
  (encode
    [_ coord]
    (when (first coord)
      (let [int-coord (map (comp util/round *) coord scale-factors)
            neighs (coordinate-neighbours int-coord radii)]
        (->> (zipmap neighs (map coordinate-order neighs))
             (util/top-n-keys-by-value n-active)
             (map (partial coordinate-bit (p/size topo)))
             (distinct))))))

(defn coordinate-encoder
  "Coordinate encoder for integer coordinates, unbounded, with one,
  two or three dimensions. Expects a coordinate, i.e. a sequence of
  numbers with 1, 2 or 3 elements. These raw values will be multiplied
  by corresponding `scale-factors` to obtain integer grid
  coordinates. Each dimension has an associated radius within which
  there is some similarity in encoded SDRs."
  [dimensions n-active scale-factors radii]
  (let [topo (topology/make-topology dimensions)]
    (map->CoordinateEncoder {:topo topo
                             :n-active n-active
                             :scale-factors scale-factors
                             :radii radii})))


;;; # Sampling Linear Encoder
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

(defn handle-multiples-at-edges
  [periodic? n-bits multiples-of coll]
  (if-not periodic?
    (filter #(<= 0 % (dec n-bits)) coll)
    (let [m-wrap (-> (dec n-bits)
                     (quot multiples-of)
                     (inc)
                     (* multiples-of))]
      (map #(mod % m-wrap) coll))))

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
    (let [remaining (- target-n-active (count chosen))
          multiples-of (long (/ 1 density))]
      (if (and (pos? remaining)
               (pos? multiples-of))
        (let [half-remaining (quot remaining 2)
              n-take (if (== 1 remaining)
                       remaining
                       half-remaining)]
          (recur (->> (multiples-within-radius center bit-radius multiples-of)
                      (handle-multiples-at-edges periodic? n-bits multiples-of)
                      (into-bounded chosen (+ n-take (count chosen))))
                 (* density 2)))
        chosen))))

(defrecord SamplingLinearEncoder
  [topo n-active lower upper radius periodic?]
  p/PTopological
  (topology
    [_]
    topo)
  p/PEncoder
  (encode
    [_ x]
    (if x
      (let [n-bits (p/size topo)
            domain-width (- upper lower)
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
                          (/ (p/size topo) domain-width))]
        (sampled-window center n-bits n-active bit-radius periodic?))
      (sequence nil)))
  (decode
    [this bit-votes n]
    (let [span (double (- upper lower))
          values (range lower upper (if (< 5 span 250)
                                      1
                                      (/ span 50)))]
      (->> (decode-by-brute-force this values bit-votes)
           (take n)))))

(defn sampling-linear-encoder
  "A linear encoder that samples the surrounding radius, rather than
  activating all of it. Sampling density decreases as distance increases.

  * `dimensions` is the size of the encoder in bits along one or more
    dimensions, a vector e.g. [500].

  * `n-active` is the number of bits to be active.

  * `[lower upper]` gives the numeric range to cover. The input number
    will be clamped to this range.

  * `radius` describes the range to sample.

  Recommendations:

  * `lower` and `upper` should be `radius` below and above the actual
    lower and upper bounds. Otherwise the radius will extend off the
    number line, creating representations that behave a bit differently
    from the rest."
  ([dimensions n-active [lower upper] radius]
   (sampling-linear-encoder dimensions n-active [lower upper] radius false))
  ([dimensions n-active [lower upper] radius periodic?]
   (let [topo (topology/make-topology dimensions)]
     (map->SamplingLinearEncoder {:topo topo
                                  :n-active n-active
                                  :lower lower
                                  :upper upper
                                  :radius radius
                                  :periodic? periodic?}))))

;;; # Sensors

(defn sensor-cat
  [& sensors]
  (let [selectors (map first sensors)
        encoders (map second sensors)]
    [(apply vec-selector selectors)
     (encat encoders)]))
