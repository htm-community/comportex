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

(defn set-seed!
  [seed]
  #+cljs
  :not-implemented
  #+clj
  (alter-var-root (var RNG)
                  (fn [_] (rng/rng seed))))

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

(defn count-filter
  "Same as `(count (filter pred coll))`, but faster."
  [pred coll]
  (reduce (fn [sum x]
            (if (pred x) (inc sum) sum))
          0 coll))

(defn group-by-maps
  "Like the built-in group-by, but building maps instead of vectors
   for the groups, and tuned for performance with many values per key."
  [f kvs]
  (->> kvs
       ;; create a transient map of transient maps
       (reduce (fn [m [k v]]
                 (let [g (f k v)
                       items (get m g (transient {}))]
                   (assoc! m g (assoc! items k v))))
               (transient {}))
       ;; make the outer map persistent (can't seq it)
       (persistent!)
       ;; make the inner maps persistent within a transient outer map
       (reduce (fn [m [g items]]
                 (assoc! m g (persistent! items)))
               (transient {}))
       ;; make the outer map persistent
       (persistent!)))

(defn update-each
  "Transforms a map or vector `m` applying function `f` to the values
   under keys `ks`."
  [m ks f]
  (->> ks
       (reduce (fn [m k]
                 (assoc! m k (f (get m k))))
               (transient m))
       (persistent!)))

(defn remap
  "Transforms a map `m` applying function `f` to each value."
  [f m]
  (->> m
       (mapv (fn [[k v]] [k (f v)]))
       (into (empty m))))
