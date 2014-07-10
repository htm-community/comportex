(ns org.nfrac.comportex.encoders
  "Methods of encoding data as distributed bit sets, for feeding as
   input to a cortex region. An encoder here is a function that takes
   some data value and returns a set of integers, which are the
   indices of the active bits in its representation. In addition, the
   returned set should have a metadata key `::bit-width` which gives
   the number of bits in the full encoded representation: a larger
   number than the count of active bits."
  (:require [clojure.set :as set]))

(defn linear-number-encoder
  "Returns an encoding function for a single number. It encodes a
   number by its position on a continuous scale within a numeric
   range. It does not represent any other features of a number (its
   component digits, integral/fractional parts, factors, etc).

  * `bit-width` is the number of bits for the full (dense)
    representation.

  * `on-bits` is the number of bits to be active.

  * `[lower upper]` gives the numeric range to cover. The input number
    will be clamped to this range."
  [bit-width on-bits [lower upper]]
  (let [span (double (- upper lower))]
    (fn [x]
      (-> (if x
            (let [x (-> x (max lower) (min upper))
                  z (/ (- x lower) span)
                  i (long (* z (- bit-width on-bits)))]
              (set (range i (+ i on-bits))))
            #{})
          (with-meta {::bit-width bit-width})))))

(defn superpose-encoder
  "Returns an encoding function for a sequence of values. The same
   encoding function will be applied to each value, and the resulting
   encodings superposed; i.e. the union of the sets of bits."
  [enc-fn]
  (fn [xs]
    (let [bs (map enc-fn xs)]
      (-> (apply set/union bs)
          (with-meta (meta (first bs)))))))

(defn juxtapose-encoder
  "Returns an encoding function for a sequence of values. The same
   encoding function will be applied to each value, and the resulting
   encodings juxtaposed; i.e. the bit width of the combined encoding
   is the sum of the component bit widths."
  ([enc-fn]
     (fn [xs]
       (->>
        (map enc-fn xs)
        (reduce (fn [bset b]
                  (let [offset (::bit-width (meta bset))
                        bw (::bit-width (meta b))]
                    (-> (into bset (map (partial + offset) b))
                        (with-meta {::bit-width (+ offset bw)}))))))))
  ([enc-fn & more-fns]
     (fn [xs]
       (->>
        (map (fn [f x] (f x))
             (list* enc-fn more-fns) xs)
        (reduce (fn [bset b]
                  (let [offset (::bit-width (meta bset))
                        bw (::bit-width (meta b))]
                    (-> (into bset (map (partial + offset) b))
                        (with-meta {::bit-width (+ offset bw)})))))))))

(defn category-encoder
  [bit-width values]
  (let [n (count values)
        on-bits (/ bit-width n)
        val-to-int (zipmap values (range))
        int-efn (linear-number-encoder bit-width on-bits [0 (dec n)])]
    (fn [x]
      (int-efn (val-to-int x)))))
