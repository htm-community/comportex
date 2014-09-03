(ns org.nfrac.comportex.encoders
  "Methods of encoding data as distributed bit sets, for feeding as
   input to a cortical region."
  (:require [clojure.set :as set]))

(defprotocol PEncoder
  (encoder-bit-width [this])
  (encode [this offset x]))

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
  (let [span (double (- upper lower))]
    (reify PEncoder
      (encoder-bit-width [_] bit-width)
      (encode [_ offset x]
        (if x
          (let [x (-> x (max lower) (min upper))
                z (/ (- x lower) span)
                i (long (* z (- bit-width on-bits)))]
            (set (range (+ offset i)
                        (+ offset i on-bits))))
          #{})))))

(defn category-encoder
  [bit-width values]
  (let [n (count values)
        on-bits (/ bit-width n)
        val-to-int (zipmap values (range))
        int-e (linear-encoder bit-width on-bits [0 (dec n)])]
    (reify PEncoder
      (encoder-bit-width [_]
        bit-width)
      (encode [_ offset x]
        (encode int-e offset (val-to-int x))))))

(defn ensplat
  "Returns a higher-level encoder for a sequence of values. The given
   encoder will be applied to each value, and the resulting encodings
   overlaid (splatted together), taking the union of the sets of bits."
  [e]
  (reify PEncoder
    (encoder-bit-width [_]
      (encoder-bit-width e))
    (encode [_ offset xs]
      (->> xs
           (map (fn [x] (encode e offset x)))
           (apply set/union)))))

(defn encat
  "Returns a higher-level encoder for a sequence of `n` values. The
   given encoder will be applied to each value, and the resulting
   encodings concatenated; i.e. the bit width of the combined encoding
   is the sum of the component bit widths. If multiple (`n`) encoders
   are given they will be applied to corresponding elements of the
   input collection."
  ([n e]
     (let [w1 (encoder-bit-width e)]
       (reify PEncoder
         (encoder-bit-width [_]
           (* n w1))
         (encode [_ offset xs]
           (->> xs
                (map-indexed (fn [i x]
                               (encode e (+ offset (* i w1)) x)))
                (apply set/union))))))
  ([n e & more]
     (let [es (list* e more)
           ws (map encoder-bit-width es)
           os (list* 0 (reductions + ws))]
       (reify PEncoder
         (encoder-bit-width [_]
           (apply + ws))
         (encode [_ offset xs]
           (->> (map (fn [e o x]
                       (encode e (+ offset o) x))
                     es os xs)
                (apply set/union)))))))

(defn pre-transform
  "Returns an encoder wrapping another encoder `e`, where the function
   `f` is applied to input values prior to encoding by `e`."
  [f e]
  (reify PEncoder
    (encoder-bit-width [_] (encoder-bit-width e))
    (encode [_ offset x] (encode e offset (f x)))))
