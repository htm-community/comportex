(ns org.nfrac.comportex.encoders)

(defn number-linear
  [lower upper width bits x]
  (let [x (-> x (max lower) (min upper))
        range (double (- upper lower))
        bit-width (Math/max 1.0 (* bits (/ width range)))
        z (/ (- x lower) range)
        i (* z (- (dec bits) (quot bit-width 2)))]
    (take bits
          (concat (repeat i false)
                  (repeat bit-width true)
                  (repeat false)))))


