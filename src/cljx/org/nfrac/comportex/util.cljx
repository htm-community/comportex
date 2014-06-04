(ns org.nfrac.comportex.util
  (:require [cemerick.pprng :as rng])
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle]))

(defn round
  [x]
  (Math/round (double x)))

(defn mean
  [xs]
  (/ (apply + xs) (double (count xs))))

(def RNG (rng/rng))

(defn rand
  ([]
     (rand 0 1))
  ([lower upper]
     {:pre [(< lower upper)]}
     (+ lower (* (rng/double RNG)
                 (- upper lower)))))

(defn rand-int
  [lower upper]
  (+ lower (rng/int RNG (- upper lower))))

(defn rand-nth
  [xs]
  (nth xs (rand-int 0 (count xs))))

(defn shuffle
  [xs]
  (let [xrs (map list (repeatedly #(rng/double RNG)) xs)]
    (map second (sort-by first xrs))))

(defn quantile
  [xs p]
  (nth (sort xs) (long (* p (dec (count xs))))))

(defn triangular
  "Returns a function transforming uniform randoms in [0 1] to variates on a
   Triangular distribution. http://en.wikipedia.org/wiki/Triangular_distribution

   * a - lower bound

   * b - upper bound

   * c - peak of probability density (within bounds)"
  [a b c]
  (let [Fc (/ (- c a)
              (- b a))]
    (fn [u]
      (if (< u Fc)
        (+ a (Math/sqrt (* u (- b a) (- c a))))
        (- b (Math/sqrt (* (- 1 u) (- b a) (- b c))))))))
