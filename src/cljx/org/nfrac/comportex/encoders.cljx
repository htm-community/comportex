(ns org.nfrac.comportex.encoders
  "Methods of encoding data as distributed bit sets, for feeding as
   input to a cortical region."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util]
            [clojure.set :as set]))

(defn pre-transform
  "Returns an encoder wrapping another encoder `e`, where the function
   `f` is applied to input values prior to encoding by `e`."
  [f e]
  (reify
    p/PTopological
    (topology [_]
      (p/topology e))
    p/PEncodable
    (encode
      [_ offset x]
      (p/encode e offset (f x)))))

(defn ensplat
  "A higher-level encoder for a sequence of values. The given encoder
   will be applied to each value, and the resulting encodings
   overlaid (splatted together), taking the union of the sets of
   bits." [e]
  (reify
    p/PTopological
    (topology [_]
      (p/topology e))
    p/PEncodable
    (encode
      [_ offset xs]
      (->> xs
           (map (fn [x] (p/encode e offset x)))
           (apply set/union)))))

(defn encat
  "A higher-level encoder for a sequence of `n` values. The given
   encoder will be applied to each value, and the resulting encodings
   concatenated; i.e. the bit width of the combined encoding is the
   sum of the component bit widths. If multiple (`n`) encoders are
   given they will be applied to corresponding elements of the input
   collection."
  ([n e]
     (let [e-dim (p/dims-of e)
           e-w (p/size-of e)
           dim (update-in e-dim [0] * n)
           topo (topology/make-topology dim)]
       (reify
         p/PTopological
         (topology [_]
           topo)
         p/PEncodable
         (encode
           [_ offset xs]
           (->> xs
                (map-indexed (fn [i x]
                               (p/encode e (+ offset (* i e-w)) x)))
                (apply set/union))))))
  ([n e & more]
     (let [es (list* e more)
           ws (map p/size-of es)
           os (list* 0 (reductions + ws))
           dim (apply topology/combined-dimensions (map p/dims-of es))
           topo (topology/make-topology dim)]
       (reify
         p/PTopological
         (topology [_]
           topo)
         p/PEncodable
         (encode
           [_ offset xs]
           (->> (map (fn [e o x]
                       (p/encode e (+ offset o) x))
                     es os xs)
                (apply set/union)))))))

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
  (let [topo (topology/make-topology [bit-width])
        span (double (- upper lower))]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ offset x]
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
    (reify
      p/PTopological
      (topology [_]
        (p/topology int-e))
      p/PEncodable
      (encode
        [_ offset x]
        (p/encode int-e offset (val-to-int x))))))

(defn unique-encoder
  "This encoder gives a unique, persistent bit set to any value when
   it is encountered."
  [bit-width on-bits]
  (let [topo (topology/make-topology [bit-width])
        x-bits (atom {})
        gen #(set (take on-bits (util/shuffle (range bit-width))))]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ offset x]
        (if (nil? x)
          #{}
          (or (get @x-bits x)
              (get (swap! x-bits assoc x (gen)) x)))))))
