(ns org.nfrac.comportex.util
  (:require [clojure.test.check.random :as random]
            [clojure.set :as set])
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle]))

;; copied from
;; https://github.com/Datomic/simulant/blob/d681b2375c3e0ea13a0df3caffeb7b3d8a20c6a3/src/simulant/util.clj#L24-L37

(defn getx
  "Like two-argument get, but throws an exception if the key is
   not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info "Missing required key" {:map m :key k})))))

(defn getx-in
  "Like two-argument get-in, but throws an exception if the key is
   not found."
  [m ks]
  (reduce getx m ks))

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

(defn rand
  [rng lower upper]
  {:pre [(<= lower upper)]}
  (-> (random/rand-double rng)
      (* (- upper lower))
      (+ lower)))

(defn rand-int
  "Uniform integer between lower (inclusive) and upper (exclusive)."
  ([rng upper]
   (-> (random/rand-double rng)
       (* upper)
       (Math/floor)
       (long)))
  ([rng lower upper]
   (-> (random/rand-double rng)
       (* (- upper lower))
       (+ lower)
       (Math/floor)
       (long))))

(defn rand-nth
  [rng xs]
  (nth xs (rand-int rng (count xs))))

;; copied from
;; https://github.com/clojure/data.generators/blob/bf2eb5288fb59045041aec01628a7f53104d84ca/src/main/clojure/clojure/data/generators.clj
;; adapted to splittable RNG

(defn ^:private fisher-yates
  "http://en.wikipedia.org/wiki/Fisher–Yates_shuffle#The_modern_algorithm"
  [rng coll]
  (let [as (object-array coll)]
    (loop [i (dec (count as))
           r rng]
      (if (<= 1 i)
        (let [[r1 r2] (random/split r)
              j (rand-int r1 (inc i))
              t (aget as i)]
          (aset as i (aget as j))
          (aset as j t)
          (recur (dec i) r2))
        (into (empty coll) (seq as))))))

(defn shuffle
  [rng coll]
  (fisher-yates rng coll))

;; copied from
;; https://github.com/clojure/data.generators/blob/bf2eb5288fb59045041aec01628a7f53104d84ca/src/main/clojure/clojure/data/generators.clj
;; adapted to splittable RNG

(defn reservoir-sample
  "Reservoir sample ct items from coll."
  [rng ct coll]
  (loop [result (transient (vec (take ct coll)))
         n ct
         coll (drop ct coll)
         r rng]
    (if (seq coll)
      (let [[r1 r2] (random/split r)
            pos (rand-int r1 n)]
        (recur (if (< pos ct)
                 (assoc! result pos (first coll))
                 result)
               (inc n)
               (rest coll)
               r2))
      (persistent! result))))

(defn sample
  "Sample ct items with replacement (i.e. possibly with duplicates) from coll."
  [rng ct coll]
  (when (pos? ct)
    (->> (random/split-n rng ct)
         (mapv #(rand-nth % coll)))))

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

(defn update-each!
  "Transforms a transient map or vector `m` applying function `f` to
  the values under keys `ks`."
  [m ks f]
  (if (empty? ks)
    m
    (reduce (fn [m k]
              (assoc! m k (f (get m k))))
            m
            ks)))

(defn update-each
  "Transforms a map or vector `m` applying function `f` to the values
  under keys `ks`."
  [m ks f]
  (if (empty? ks)
    m
    (persistent!
     (update-each! (transient m) ks f))))

(defn- mapish? [m] (or (nil? m) (map? m)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? mapish? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? mapish? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn remap
  "Transforms a map `m` applying function `f` to each value."
  [f m]
  (into (or (empty m) {})
        (map (fn [[k v]] [k (f v)]))
        m))

(defn top-n-keys-by-value
  "Like `(reverse (take n (keys (sort-by val > m))))` but faster."
  [n m]
  (cond
   (<= n 0) []
   (empty? m) []
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

(defn splits-at
  "Returns a collection of
  `[(take w0 coll) (take w1 (drop w0 coll)) ...`
  and ending with a sequence containing the remainder."
  [ws coll]
  (reduce (fn [subcolls w]
            (concat (drop-last subcolls)
                    (split-at w (last subcolls))))
          [coll] ws))

(defn splits-with
  "Returns a collection of
  `[(take-while pred0 coll) (take-while pred1 (drop-while pred0 coll)) ...`
  and ending with a sequence containing the remainder."
  [preds coll]
  (reduce (fn [subcolls pred]
            (concat (drop-last subcolls)
                    (split-with pred (last subcolls))))
          [coll] preds))

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

(defn unalign-indices
  "Partition a sorted seq of indices into `(count widths)` seqs of unshifted
  indices. Determine boundaries via `widths`. `aligned-is` must be sorted."
  [widths aligned-is]
  (let [offs (->> widths (reductions + 0) (drop 1))
        [leftmost & others] (->> aligned-is
                                 (splits-with (map (partial partial >) offs)))
        shifted (apply vector leftmost
                       (map (fn [section offset]
                              (map #(- % offset) section))
                            others offs))]
    (assert (empty? (last shifted))
            "No indices should be beyond the final offset.")
    (drop-last shifted)))

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

(defn set-similarity
  [sdr1 sdr2]
  (/ (count (set/intersection sdr1 sdr2))
     (max 1 (count sdr1) (count sdr2))))
