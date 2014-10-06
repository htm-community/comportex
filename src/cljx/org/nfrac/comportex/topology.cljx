(ns org.nfrac.comportex.topology
  (:require [org.nfrac.comportex.protocols :as p]))

(defn abs
  [x]
  (if (neg? x) (- x) x))

(defn one-d-ring
  [n i outer-r inner-r]
  (concat (range (min (+ i inner-r 1) n)
                 (min (+ i outer-r 1) n))
          (range (max (- i outer-r) 0)
                 (max (- i inner-r) 0))))

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
    (one-d-ring size coord outer-r inner-r))
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
      (for [x (one-d-ring width cx outer-r inner-r)
            y (one-d-ring height cy outer-r inner-r)]
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

(defn make-topology
  [dims]
  (let [[w h] dims]
    (case (count dims)
      1 (one-d-topology w)
      2 (two-d-topology w h))))
