(ns org.nfrac.comportex.topography
  (:require [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]))

(s/def ::dimensions
  (s/coll-of nat-int? :kind vector? :min-count 1 :max-count 3
             :gen (fn []
                    (s/gen (s/and (s/coll-of (s/int-in 0 2048) :kind vector?
                                             :min-count 1 :max-count 3)
                                  #(<= (reduce * %) 2048))))))

(s/def ::pos-dimensions
  (s/and ::dimensions #(>= (reduce * %) 1)))

(defprotocol PTopography
  "Operating on a regular grid of certain dimensions, where each
   coordinate is an n-tuple vector---or integer for 1D---and also has
   a unique integer index."
  (dimensions [this])
  (coordinates-of-index [this idx])
  (index-of-coordinates [this coord])
  (neighbours* [this coord outer-r inner-r])
  (coord-distance [this coord-a coord-b]))

(declare make-topography)

(s/def ::topography
  (-> #(satisfies? PTopography %)
      (s/with-gen #(gen/fmap make-topography (s/gen ::dimensions)))))

(defn size
  "The total number of elements indexed in the topography."
  [topo]
  (reduce * (dimensions topo)))

(defn neighbours
  "Returns the coordinates away from `coord` at distances `inner-r` (exclusive)
  out to `outer-r` (inclusive). The default inner-r is 0 which excludes coord
  itself. To include coord, use inner-r -1."
  ([topo coord radius]
   (neighbours* topo coord radius 0))
  ([topo coord outer-r inner-r]
   (neighbours* topo coord outer-r inner-r)))

(defn- cvec [[k v]] (case k :int [v] :vec v))

(s/fdef neighbours
        :args (s/and
               (s/cat :topo ::topography
                      :coord (s/or :vec ::dimensions
                                   :int nat-int?)
                      :outer-r (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 9))))
                      :inner-r (s/? (-> (s/int-in -1 1e9)
                                        (s/with-gen #(s/gen (s/int-in -1 8))))))
               #(= (count (cvec (:coord %))) (count (dimensions (:topo %))))
               #(every? identity (map < (cvec (:coord %)) (dimensions (:topo %))))
               #(< (:outer-r %) (* 3 (apply max (dimensions (:topo %))))))
        :ret (s/every ::dimensions))

(defn neighbours-indices
  "Same as `neighbours` but taking and returning indices instead of
   coordinates."
  ([topo idx radius]
   (neighbours-indices topo idx radius 0))
  ([topo idx outer-r inner-r]
   (->> (neighbours* topo (coordinates-of-index topo idx)
                     outer-r inner-r)
        (map (partial index-of-coordinates topo)))))

(s/fdef neighbours-indices
        :args (s/and
               (s/cat :topo ::topography
                      :idx nat-int?
                      :outer-r (-> nat-int? (s/with-gen #(s/gen (s/int-in 0 9))))
                      :inner-r (s/? (-> (s/int-in -1 1e9)
                                        (s/with-gen #(s/gen (s/int-in -1 8))))))
               #(< (:idx %) (size (:topo %)))
               #(< (:outer-r %) (* 3 (apply max (dimensions (:topo %))))))
        :ret (s/every nat-int?)
        :fn (fn [m]
              (let [n (-> m :args :topo size)]
                (every? #(<= 0 % (dec n)) (:ret m)))))

(defn- abs
  [x]
  (if (neg? x) (- x) x))

(defrecord OneDTopography
    [size]
  PTopography
  (dimensions [_]
    [size])
  (coordinates-of-index [_ idx]
    idx)
  (index-of-coordinates [_ coord]
    coord)
  (neighbours*
    [this coord outer-r inner-r]
    (concat (range (min (+ coord inner-r 1) (dec size))
                   (min (+ coord outer-r 1) size))
            (range (max (- coord outer-r) 0)
                   (max (- coord inner-r) 0))))
  (coord-distance
    [_ coord-a coord-b]
    (abs (- coord-b coord-a))))

(defn one-d-topography
  [size]
  (->OneDTopography size))

(defrecord TwoDTopography
    [width height]
  ;; Represents coordinates [x y], with x changing faster over
  ;; indices: they are layed out by row, like pixels in an image.
  ;; Uses Chebyshev distance (max coord diff) for neighbours and
  ;; distance.
  PTopography
  (dimensions [_]
    [width height])
  (coordinates-of-index
    [_ idx]
    [(rem idx width) (quot idx width)])
  (index-of-coordinates
    [_ coord]
    (let [[cx cy] coord]
      (+ cx (* cy width))))
  (neighbours*
    [this coord outer-r inner-r]
    (let [[cx cy] coord]
      ;; by Chebyshev distance
      (for [x (range (max (- cx outer-r) 0)
                     (min (+ cx outer-r 1) width))
            y (range (max (- cy outer-r) 0)
                     (min (+ cy outer-r 1) height))
            :when (or (> (abs (- x cx)) inner-r)
                      (> (abs (- y cy)) inner-r))]
        [x y])))
  (coord-distance
    [_ coord-a coord-b]
    (let [[xa ya] coord-a
          [xb yb] coord-b]
      ;; Chebyshev distance
      (max (abs (- xb xa))
           (abs (- yb ya))))))

(defn two-d-topography
  [width height]
  (->TwoDTopography width height))

(defrecord ThreeDTopography
    [width height depth]
  ;; Represents coordinates [x y z], with x changing fastest over
  ;; indices. Like a stack of images. The indexing is as in a
  ;; concatenation of multiple 2D xy images.
  ;; Uses Chebyshev distance (max coord diff) for neighbours and
  ;; distance.
  PTopography
  (dimensions [_]
    [width height depth])
  (coordinates-of-index
    [_ idx]
    (let [z (quot idx (* width height))
          z-rem (rem idx (* width height))]
      [(rem z-rem width) (quot z-rem width) z]))
  (index-of-coordinates
    [_ coord]
    (let [[cx cy cz] coord]
      (+ cx (* cy width) (* cz width height))))
  (neighbours*
    [this coord outer-r inner-r]
    (let [[cx cy cz] coord]
      ;; by Chebyshev distance
      (for [x (range (max (- cx outer-r) 0)
                     (min (+ cx outer-r 1) width))
            y (range (max (- cy outer-r) 0)
                     (min (+ cy outer-r 1) height))
            z (range (max (- cz outer-r) 0)
                     (min (+ cz outer-r 1) depth))
            :when (or (> (abs (- x cx)) inner-r)
                      (> (abs (- y cy)) inner-r)
                      (> (abs (- z cz)) inner-r))]
        [x y z])))
  (coord-distance
    [_ coord-a coord-b]
    (let [[xa ya za] coord-a
          [xb yb zb] coord-b]
      ;; Chebyshev distance
      (max (abs (- xb xa))
           (abs (- yb ya))
           (abs (- zb za))))))

(defn three-d-topography
  [w h d]
  (->ThreeDTopography w h d))

(defn make-topography
  [dims]
  (let [[w h d q] dims]
    (case (count dims)
      0 (one-d-topography 0)
      1 (one-d-topography w)
      2 (two-d-topography w h)
      3 (three-d-topography w h d)
      4 (three-d-topography w h (* d q)))))

(s/fdef make-topography
        :args (s/cat :dims ::dimensions)
        :ret ::topography)

(def empty-topography
  (make-topography [0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Combining

(defn squash-last-dimension
  "Project n dimensions to n-1 dimensions by eliminating the last dimension.

  This removes potentially-valuable structure.
  Example: in dimensions [8 7 6], the points [0 0 0] [0 1 0] are adjacent.
  After squashing to [8 42], these points [0 0] [0 6] are much further apart."
  [dims]
  (-> dims
      (update-in [(- (count dims) 2)]
                 * (last dims))
      butlast
      vec))

(defn split-first-dimension
  "Project n dimensions to n+1 dimensions by dividing the first dimension
  into cross sections.

  This artificially adds structure. It can also disrupt existing structure.
  Example: In dimensions [64] the points [7] and [8] are adjacent.
  After splitting to [8 8], these points [0 7] [1 0] are further apart."
  [dims xsection-length]
  (when-let [[x & rest] dims]
    (when (zero? (rem x xsection-length))
      (into [(quot x xsection-length)]
            (assoc dims 0 xsection-length)))))

;; We will pour the concatenated indices (and offsets) into this
;; combined topography. Note that layer output is its cells, so will
;; add another dimension to column topography. e.g. 2D columns [x y]
;; becomes [x y z], where z = cell depth.

(defn combined-dimensions
  "Align n topographies along the x axis into a single topography.
  If the topographies don't stack neatly, force compatibility via two
  strategies:

  1. Add dimensions to the lower-dimensional topography by splitting its first
  dimension into cross sections. This is analogous to summing numbers encoded
  in a mixed radix. If the sum of `higher` and `lower` can be expressed by only
  changing the first digit of `higher`, then the two can be stacked in
  `higher`'s radix (i.e. dimensions).

  Default behavior: don't redistribute / mangle `lower`'s lower dimensions
  (i.e. [y, z, ...]). To force mangling, provide a 1-dimensional `lower`.

  2. Remove dimensions from the higher-dimension topography by squashing its
  last two dimensions into one.

  It's best to hand-pick compatible topographies if topography matters."
  ([]
   [0])
  ([& all-dims]
   (reduce (fn [dims1 dims2]
             (let [[lower higher] (->> [dims1 dims2]
                                       (map #(if (empty? %) [0] %))
                                       (sort-by count))
                   disparity (- (count higher) (count lower))
                     ;; match all dimensions except x
                   [to-match must-already-match] (->> (rest higher)
                                                      (split-at disparity))]
               (if-let [compatible (when (= (vec (rest lower))
                                            (vec must-already-match))
                                     (reduce split-first-dimension lower
                                             (reverse to-match)))]
                   ;; now that everything except x matches, sum the xs
                 (update-in higher [0] + (first compatible))
                 (recur (squash-last-dimension higher) lower))))
           all-dims)))

(s/fdef combined-dimensions
        :args (s/* ::dimensions)
        :ret ::dimensions)

(defn topo-union
  [topos]
  (make-topography
    (apply combined-dimensions
           (map dimensions topos))))

(s/fdef topo-union
        :args (s/cat :topos (s/coll-of ::topography))
        :ret ::topography)
