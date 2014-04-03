(ns org.nfrac.comportex.encoders
  (:require [clojure.set :as set]))

(defn number-linear
  [bits [lower upper] width]
  (let [span (double (- upper lower))
        bit-width (max 1.0 (* bits (/ width span)))
        bit-radius (quot bit-width 2)]
    (fn [x]
      (let [x (-> x (max lower) (min upper))
            z (/ (- x lower) span)
            i (long (* z (- (dec bits) bit-radius)))]
        (if false
          (take bits
                (concat (repeat i false)
                        (repeat bit-width true)
                        (repeat false)))
          (set (range i (+ i bit-width))))))))

(defn union-encoder
    [enc-fn]
    (fn [xs]
      (apply set/union (map enc-fn xs))))
