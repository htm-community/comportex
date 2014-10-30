(ns org.nfrac.comportex.topology
  (:require [org.nfrac.comportex.protocols :as p]))

(defn abs
  [x]
  (if (neg? x) (- x) x))

(defrecord OneDTopology
    [size]
  p/PTopology
  (dimensions [_]
    [size])
  (coordinates-of-index [_ idx]
    idx)
  (index-of-coordinates [_ coord]
    coord)
  (neighbours*
    [this coord outer-r inner-r]
    (concat (range (min (+ coord inner-r 1) size)
                   (min (+ coord outer-r 1) size))
            (range (max (- coord outer-r) 0)
                   (max (- coord inner-r) 0))))
  (coord-distance
    [_ coord-a coord-b]
    (abs (- coord-b coord-a))))

(defn one-d-topology
  [size]
  (->OneDTopology size))

(defrecord TwoDTopology
    [width height]
  ;; Represents coordinates [x y], with y changing faster over indices.
  ;; Uses Manhattan distance for neighbours and distance.
  p/PTopology
  (dimensions [_]
    [width height])
  (coordinates-of-index
    [_ idx]
    [(quot idx height) (rem idx height)])
  (index-of-coordinates
    [_ coord]
    (let [[cx cy] coord]
      (+ (* cx height) cy)))
  (neighbours*
    [this coord outer-r inner-r]
    (let [[cx cy] coord]
      ;; Manhattan distance
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
      ;; Manhattan distance
      (+ (abs (- xb xa))
         (abs (- yb ya))))))

(defn two-d-topology
  [width height]
  (->TwoDTopology width height))

(defrecord ThreeDTopology
    [width height depth]
  ;; Represents coordinates [x y z], with z changing fastest over indices.
  ;; Uses Manhattan distance for neighbours and distance.
  p/PTopology
  (dimensions [_]
    [width height depth])
  (coordinates-of-index
    [_ idx]
    (let [x (quot idx (* height depth))
          x-rem (rem idx (* height depth))]
      [x (quot x-rem depth) (rem x-rem depth)]))
  (index-of-coordinates
    [_ coord]
    (let [[cx cy cz] coord]
      (+ (* cx height depth) (* cy height) cz)))
  (neighbours*
    [this coord outer-r inner-r]
    (let [[cx cy cz] coord]
      ;; Manhattan distance
      (for [x (range (max (- cx outer-r) 0)
                     (min (+ cx outer-r 1) width))
            y (range (max (- cy outer-r) 0)
                     (min (+ cy outer-r 1) height))
            z (range (max (- cz outer-r) 0)
                     (min (+ cz outer-r 1) depth))
            :when (or (> (abs (- x cx)) inner-r)
                      (> (abs (- y cy)) inner-r)
                      (> (abs (- y cz)) inner-r))]
        [x y z])))
  (coord-distance
    [_ coord-a coord-b]
    (let [[xa ya za] coord-a
          [xb yb zb] coord-b]
      ;; Manhattan distance
      (+ (abs (- xb xa))
         (abs (- yb ya))
         (abs (- zb za))))))

(defn three-d-topology
  [w h d]
  (three-d-topology w h d))

(defn make-topology
  [dims]
  (let [[w h d q] dims]
    (case (count dims)
      0 (one-d-topology 0)
      1 (one-d-topology w)
      2 (two-d-topology w h)
      3 (three-d-topology w h d)
      4 (three-d-topology w h (* d q))
      )))

(def empty-topology
  (make-topology [0]))

;; We will pour the concatenated indices (and offsets) into this
;; combined topology. Note that region output is its cells, so will
;; add another dimension to column topology. e.g. 2D columns [x y]
;; becomes [x y z], where z = cell depth.

(defn combined-dimensions
  ([]
     empty-topology)
  ([dims]
     dims)
  ([dims1 dims2]
     (let [d1 (count dims1)
           d2 (count dims2)
           [x1 y1 z1] dims1
           [x2 y2 z2] dims2]
       (cond
        ;; ensure higher dimensional one comes first
        (> d2 d1)
        (recur dims2 dims1)
        ;; any empty
        (or (zero? d2) (some zero? dims2))
        dims1
        ;; 1D & 1D
        (and (== 1 d1) (== 1 d2))
        [(+ x1 x2)]
        ;; 2D & 2D compatible - direct case
        (and (== 2 d1) (== 2 d2)
             (== y1 y2))
        [(+ x1 x2) y1]
        ;; 2D & 2D incompatible - project to 2D & 1D
        (and (== 2 d1) (== 2 d2))
        (recur dims1 [(* x2 y2)])
        ;; 2D & 1D compatible
        (and (== 2 d1) (== 1 d2)
             (zero? (rem x2 y1)))
        [(+ x1 (quot x2 y1)) y1]
        ;; 2D & 1D incompatible - project to 1D & 1D
        (and (== 2 d1) (== 1 d2))
        [(+ (* x1 y1) x2)]
        ;; 3D & 3D compatible - direct case
        (and (== 3 d1) (== 3 d2)
             (== z1 z2)
             (== y1 y2))
        [(+ x1 x2) y1 z1]
        ;; 3D & 3D incompatible - project to 3D & 2D
        (and (== 3 d1) (== 3 d2))
        (recur dims1 [x2 (* y2 z2)])
        ;; 3D & 2D compatible - direct case 1
        (and (== 3 d1) (== 2 d2)
             (== y1 x2)
             (== z1 y2))
        [(+ x1 1) y1 z1]
        ;; 3D & 2D incompatible - project to 2D & 2D
        (and (== 3 d1) (== 2 d2))
        (recur [x1 (* y1 z1)] dims2)
        ;; 3D & 1D compatible
        (and (== 3 d1) (== 1 d2)
             (zero? (rem x2 z1)))
        [x1 (+ y1 (quot x2 z1)) z1]
        ;; 3D & 1D incompatible - project to 2D & 1D
        (and (== 3 d1) (== 1 d2))
        (recur [x1 (* y1 z1)] dims1)
        )))
  ([dims1 dims2 & more-dims]
     (reduce combined-dimensions
             (combined-dimensions dims1 dims2)
             more-dims)))
