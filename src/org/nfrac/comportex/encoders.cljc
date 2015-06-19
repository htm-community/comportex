(ns org.nfrac.comportex.encoders
  "Methods of encoding data as distributed bit sets, for feeding as
   input to a cortical region."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util]
            [cemerick.pprng :as rng]))

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

(defn pre-transform
  "Returns an encoder wrapping another encoder `e`, where the function
   `f` is applied to input values prior to encoding by `e`."
  [f e]
  (reify
    p/PTopological
    (topology [_]
      (p/topology e))
    p/PEncodable
    (encode
      [_ x]
      (p/encode e (f x)))
    (decode
      [_ bit-votes n]
      (p/decode e bit-votes n))))

(defn ensplat
  "A higher-level encoder for a sequence of values. The given encoder
   will be applied to each value, and the resulting encodings
   overlaid (splatted together), taking the union of the sets of
   bits."
  [e]
  (reify
    p/PTopological
    (topology [_]
      (p/topology e))
    p/PEncodable
    (encode
      [_ xs]
      (->> xs
           (mapcat (partial p/encode e))
           (distinct)))))

(defn encat
  "A higher-level encoder for a sequence of `n` values. The given
   encoder will be applied to each value, and the resulting encodings
   concatenated; i.e. the bit width of the combined encoding is the
   sum of the component bit widths. If multiple (`n`) encoders are
   given they will be applied to corresponding elements of the input
   collection."
  ([n e]
     (let [e-dim (p/dims-of e)
           dim (update-in e-dim [0] * n)
           topo (topology/make-topology dim)]
       (reify
         p/PTopological
         (topology [_]
           topo)
         p/PEncodable
         (encode
           [_ xs]
           (->> xs
                (map (partial p/encode e))
                (util/align-indices (repeat (p/size-of e)))))
         (decode
           [_ bit-votes n-values]
           (map #(p/decode e % n-values)
                (unaligned-bit-votes (repeat n (p/size-of e))
                                     bit-votes))))))
  ([n e & more]
     (let [es (list* e more)
           ws (map p/size-of es)
           dim (apply topology/combined-dimensions (map p/dims-of es))
           topo (topology/make-topology dim)]
       (reify
         p/PTopological
         (topology [_]
           topo)
         p/PEncodable
         (encode
           [_ xs]
           (->> xs
                (map p/encode es)
                (util/align-indices ws)))
         (decode
           [_ bit-votes n-values]
           (map #(p/decode % %2 n-values)
                es (unaligned-bit-votes ws bit-votes)))))))

(defn linear-encoder
  "Returns a simple encoder for a single number. It encodes a
   number by its position on a continuous scale within a numeric
   range. It does not represent any other features of a number (its
   component digits, integral/fractional parts, factors, etc).

  * `bit-width` is the number of bits for the full (dense)
    representation.

  * `on-bits` is the number of bits to be active.

  * `[lower upper]` gives the numeric range to cover. The input number
    will be clamped to this range."
  [bit-width on-bits [lower upper]]
  (let [topo (topology/make-topology [bit-width])
        span (double (- upper lower))]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ x]
        (if x
          (let [x (-> x (max lower) (min upper))
                z (/ (- x lower) span)
                i (long (* z (- bit-width on-bits)))]
            (range i (+ i on-bits)))
          (sequence nil)))
      (decode
        [this bit-votes n]
        (let [values (range lower upper (if (< 5 span 250)
                                          1
                                          (/ span 50)))]
          (->> (decode-by-brute-force this values bit-votes)
               (take n)))))))

(defn category-encoder
  [bit-width values]
  (let [n (count values)
        on-bits (/ bit-width n)
        val-to-int (zipmap values (range))
        int-e (linear-encoder bit-width on-bits [0 (dec n)])]
    (reify
      p/PTopological
      (topology [_]
        (p/topology int-e))
      p/PEncodable
      (encode
        [_ x]
        (p/encode int-e (val-to-int x)))
      (decode
        [this bit-votes n]
        (->> (decode-by-brute-force this values bit-votes)
             (take n))))))

(defn unique-encoder
  "This encoder gives a unique, persistent bit set to any value when
   it is encountered. `input-dim` is the dimensions as a vector."
  [input-dim on-bits]
  (let [topo (topology/make-topology input-dim)
        bit-width (p/size topo)
        cached-bits (atom {})
        gen (fn [x]
              (let [RNG (rng/rng (hash x))]
                (->> (repeatedly #(rng/int RNG bit-width))
                     (distinct)
                     (take on-bits))))]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ x]
        (if (nil? x)
          (sequence nil)
          (or (get @cached-bits x)
              (get (swap! cached-bits assoc x (gen x)) x))))
      (decode
        [this bit-votes n]
        (->> (decode-by-brute-force this (keys @cached-bits) bit-votes)
             (take n))))))

(defn linear-2d-encoder
  "Returns a simple encoder for a pair of numbers. It encodes each
   number by its position on a continuous scale within a numeric
   range.

  * `input-size` is the number of bits along [x y] axes.

  * `on-bits` is the number of bits to be active.

  * `[x-max y-max]` gives the numeric range to cover. The input number
    will be clamped to this range."
  [input-size on-bits [x-max y-max]]
  (let [topo (topology/make-topology input-size)
        [w h] input-size]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ [x y]]
        (if x
          (let [x (-> x (max 0) (min x-max))
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
                 (take on-bits)))
          (sequence nil)))
      (decode
        [this bit-votes n]
        (let [values (for [x (range x-max)
                           y (range y-max)]
                       [x y])]
          (->> (decode-by-brute-force this values bit-votes)
               (take n)))))))

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
  (let [RNG (rng/rng (hash coord))]
    (rng/double RNG)))

(defn coordinate-bit
  [size coord]
  ;; need a different seed from coordinate-order here;
  ;; otherwise highest orders always have highest bits!
  ;; (in cljs, (rng/int) is just a scaling of (rng/double).)
  ;; TODO: splittable random number generator
  (let [seedval (if (= 1 (count coord))
                  (let [[x] coord] (reverse (str x)))
                  (reverse coord))
        RNG (rng/rng (hash seedval))]
    (rng/int RNG size)))

(defn coordinate-encoder
  "Coordinate encoder over integer coordinates, unbounded (up to
   platform limit), with one, two or three dimensions. Each dimension
   has an associated radius within which there is some similarity in
   encoded SDRs. Looks up keys :coord and :radii from the input value."
  [input-dim on-bits]
  (let [topo (topology/make-topology input-dim)
        size (p/size topo)]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ {:keys [coord radii]}]
        (when coord
          (let [neighs (coordinate-neighbours coord radii)]
            (->> (zipmap neighs (map coordinate-order neighs))
                 (util/top-n-keys-by-value on-bits)
                 (map (partial coordinate-bit size))
                 (distinct)))))
      )))
