(ns org.nfrac.comportex.util
  (:require [cemerick.pprng :as rng])
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle]))

(defn abs
  [x]
  (if (neg? x) (- x) x))

(defn round
  ([x]
     (Math/round (double x)))
  ([x n]
     (let [z (Math/pow 10.0 n)]
       (-> x
           (* z)
           (round)
           (/ z)
           (double)))))

(defn mean
  [xs]
  (/ (apply + xs) (double (count xs))))

;; convenience wrappers for RNG. actually should associate RNG with models?

(def RNG (rng/rng))

(defn set-seed!
  [seed]
  #?(:cljs
     :not-implemented
     :clj
     (alter-var-root (var RNG)
                     (fn [_] (rng/rng seed)))))

(defn rand
  ([]
     (rand 0 1))
  ([lower upper]
     {:pre [(< lower upper)]}
     (+ lower (* (rng/double RNG)
                 (- upper lower)))))

(defn rand-int
  "Uniform integer between lower (inclusive) and upper (exclusive)."
  ([upper]
     (rng/int RNG upper))
  ([lower upper]
     (+ lower (rng/int RNG (- upper lower)))))

(defn rand-nth
  [xs]
  (nth xs (rand-int (count xs))))

;; copied from
;; https://github.com/clojure/data.generators/blob/bf2eb5288fb59045041aec01628a7f53104d84ca/src/main/clojure/clojure/data/generators.clj
(defn ^:private fisher-yates
  "http://en.wikipedia.org/wiki/Fisher–Yates_shuffle#The_modern_algorithm"
  [coll]
  (let [as (object-array coll)]
    (loop [i (dec (count as))]
      (if (<= 1 i)
        (let [j (rand-int (inc i))
              t (aget as i)]
          (aset as i (aget as j))
          (aset as j t)
          (recur (dec i)))
        (into (empty coll) (seq as))))))

(defn shuffle
  [coll]
  (fisher-yates coll))

;; copied from
;; https://github.com/clojure/data.generators/blob/bf2eb5288fb59045041aec01628a7f53104d84ca/src/main/clojure/clojure/data/generators.clj
(defn reservoir-sample
  "Reservoir sample ct items from coll."
  [ct coll]
  (loop [result (transient (vec (take ct coll)))
         n ct
         coll (drop ct coll)]
    (if (seq coll)
      (let [pos (rand-int n)]
        (recur (if (< pos ct)
                 (assoc! result pos (first coll))
                 result)
               (inc n)
               (rest coll)))
      (persistent! result))))

(defn sample
  "Sample ct items with replacement (i.e. possibly with duplicates) from coll."
  [ct coll]
  (repeatedly ct #(rand-nth coll)))

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
  "Like the built-in group-by, but taking key-value pairs and building
   maps instead of vectors for the groups. It is tuned for performance
   with many values per key. `f` is a function taking 2 arguments, the
   key and value."
  ([f kvs]
     (group-by-maps f kvs {}))
  ([f kvs init-m]
     (->> kvs
          ;; create a transient map of transient maps
          (reduce (fn [m [k v]]
                    (let [g (f k v)
                          items (get m g (transient init-m))]
                      (assoc! m g (assoc! items k v))))
                  (transient {}))
          ;; make the outer map persistent (can't seq it)
          (persistent!)
          ;; make the inner maps persistent within a transient outer map
          (reduce (fn [m [g items]]
                    (assoc! m g (persistent! items)))
                  (transient {}))
          ;; make the outer map persistent
          (persistent!))))

(defn update-each
  "Transforms a map or vector `m` applying function `f` to the values
   under keys `ks`."
  [m ks f]
  (if (empty? ks)
    m
    (->> ks
         (reduce (fn [m k]
                   (assoc! m k (f (get m k))))
                 (transient m))
         (persistent!))))

(defn remap
  "Transforms a map `m` applying function `f` to each value."
  [f m]
  (->> m
       (mapv (fn [[k v]] [k (f v)]))
       (into (or (empty m) {}))))

(defn top-n-keys-by-value
  "Like `(reverse (take n (keys (sort-by val > m))))` but faster."
  [n m]
  (cond
   (<= n 0) []
   (== n 1) [(key (apply max-key val (seq m)))]
   :else
   (loop [ms (seq m)
          am (sorted-map-by #(compare [(m %1) %1] [(m %2) %2]))
          curr-min -1.0]
     (if (empty? ms)
       (keys am)
       (let [[k v] (first ms)]
         (cond
          ;; just initialising the set
          (empty? am)
          (recur (next ms)
                 (assoc am k v)
                 (double v))
          ;; filling up the set
          (< (count am) n)
          (recur (next ms)
                 (assoc am k v)
                 (double (min curr-min v)))
          ;; include this one, dominates previous min
          (> v curr-min)
          (let [new-am (-> (dissoc am (first (keys am)))
                           (assoc k v))]
            (recur (next ms)
                   new-am
                   (double (first (vals new-am)))))
          ;; exclude this one
          :else
          (recur (next ms) am curr-min)))))))

(defn align-indices
  "Using the provided widths and a coll of colls of indices, lazily adjust
  each index so that each coll of indices starts where the previous coll ended.
  Lazily concat all results."
  ([widths]
     ;; reserving arity -- this could become a transducer
     :not-implemented)
  ([widths collcoll]
     (let [[leftmost & others] collcoll
           offs (reductions + widths)]
       (concat leftmost
               (mapcat #(map (partial + %) %2) offs others)))))

(def empty-queue
  #?(:cljs cljs.core.PersistentQueue.EMPTY
     :clj clojure.lang.PersistentQueue/EMPTY))

(defn keep-history-middleware
  "Returns a function that adds a metadata key `meta-key` to its
   argument, being a #queue of the last `keep-n` values extracted
   using `value-fn`."
  [keep-n value-fn meta-key]
  (let [hist (atom empty-queue)]
    (fn [x]
      (vary-meta x assoc meta-key
                 (swap! hist (fn [h]
                               (let [h2 (conj h (value-fn x))]
                                 (if (>= (count h) keep-n) (pop h2) h2))))))))

(defn frequencies-middleware
  "Returns a function that adds a metadata key `meta-key` to its
   argument, being a map of the frequencies of values extracted
   using `value-fn`."
  [value-fn meta-key]
  (let [freqs (atom {})]
    (fn [x]
      (->> (fn [m]
             (update-in m [(value-fn x)] (fnil inc 0)))
           (swap! freqs)
           (vary-meta x assoc meta-key)))))
