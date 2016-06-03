(ns org.nfrac.comportex.columns
  "Handling of column-level proximal synapses - initialisation and boosting."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util
             :refer [abs round mean count-filter remap]]
            [clojure.test.check.random :as random]))

(defn uniform-ff-synapses
  "Generates feed-forward synapses connecting columns to the input bit
   array.

   Connections are made locally by scaling the input space to the
   column space. Potential synapses are chosen within a radius in
   input space of `ff-potential-radius` fraction of the longest single
   dimension, and of those, `ff-init-frac` are chosen from a
   uniform random distribution.

   Initial permanence values are uniformly distributed between
   `ff-perm-init-lo` and `ff-perm-init-hi`."
  [topo itopo spec rng]
  (let [p-hi (:ff-perm-init-hi spec)
        p-lo (:ff-perm-init-lo spec)
        global? (>= (:ff-potential-radius spec) 1.0)
        ;; radius in input space, fraction of longest dimension
        radius (long (* (:ff-potential-radius spec)
                        (apply max (p/dimensions itopo))))
        frac (:ff-init-frac spec)
        input-size (p/size itopo)
        n-cols (p/size topo)
        one-d? (or (== 1 (count (p/dimensions topo)))
                   (== 1 (count (p/dimensions itopo))))
        [cw ch cdepth] (p/dimensions topo)
        [iw ih idepth] (p/dimensions itopo)
        ;; range of coordinates usable as focus (adjust for radius at edges)
        focus-ix (fn [frac width]
                   (-> frac
                       (* (- width (* 2 radius)))
                       (+ radius)
                       (round)))
        ;; range of z coordinates usable as focus for radius
        focus-izs (when idepth
                    (if (<= idepth (inc (* 2 radius)))
                      (list (quot idepth 2))
                      (range radius (- idepth radius))))]
    (if global?
      (let [n-syns (round (* frac input-size))]
        (->> (random/split-n rng n-cols)
             (mapv (fn [col-rng]
                     (into {}
                           (map (fn [rng]
                                  (let [[rng1 rng2] (random/split rng)]
                                    [(util/rand-int rng1 input-size)
                                     (util/rand rng2 p-lo p-hi)])))
                           (random/split-n col-rng n-syns))))))
      (->> (random/split-n rng n-cols)
           (mapv (fn [col col-rng]
                   (let [focus-i (if one-d?
                                   (round (* input-size (/ col n-cols)))
                                   ;; use corresponding positions in 2D
                                   (let [[cx cy _] (p/coordinates-of-index topo col)
                                         ix (focus-ix (/ cx cw) iw)
                                         iy (focus-ix (/ cy ch) ih)
                                         ;; in 3D, choose z coordinate from range
                                         iz (when idepth
                                              (nth focus-izs (mod col (count focus-izs))))
                                         icoord (if idepth [ix iy iz] [ix iy])]
                                     (p/index-of-coordinates itopo icoord)))
                         all-ids (vec (p/neighbours-indices itopo focus-i radius -1))
                         n (round (* frac (count all-ids)))
                         [rng1 rng2] (random/split col-rng)
                         ids (cond
                               (< frac 0.4) ;; for performance:
                               (util/sample rng1 n all-ids)
                               (< frac 1.0)
                               (util/reservoir-sample rng1 n all-ids)
                               :else
                               all-ids)]
                     (into {}
                           (map (fn [id rng]
                                  [id (util/rand rng p-lo p-hi)])
                                ids
                                (random/split-n rng2 (count ids))))))
                 (range))))))

;;; ## Overlaps

(defn apply-overlap-boosting
  "Given a map `exc` of the column overlap counts, multiplies the
  excitation value by the corresponding column boosting factor."
  [exc boosts]
  (->> exc
       (reduce-kv (fn [m id x]
                    (let [[col _] id
                          b (get boosts col)]
                      (assoc! m id (* x b))))
                  (transient {}))
       (persistent!)))

;;; ## Learning

(defn ff-new-synapse-ids
  [rng ff-bits curr-ids-set col itopo focus-coord radius n-grow]
  (loop [ids ()
         on-bits (util/shuffle rng ff-bits)]
    (if (or (empty? on-bits)
            (>= (count ids) n-grow))
      ids
      (let [id (first ff-bits)]
        (if (curr-ids-set id)
          ;; already have this synapse
          (recur ids (next on-bits))
          ;; check distance
          (let [coord (p/coordinates-of-index itopo id)
                dist (p/coord-distance itopo coord focus-coord)]
            (if (< dist radius)
              ;; ok, choose this for a new synapse
              ;; TODO - ff-potential-frac
              (recur (conj ids id) (next on-bits))
              ;; out of radius
              (recur ids (next on-bits)))))))))

(defn grow-new-synapses
  [rng ff-sg col ff-bits itopo radius n-cols n-grow pinit]
  (let [input-size (p/size itopo)
        focus-i (round (* input-size (/ col n-cols)))
        focus-coord (p/coordinates-of-index itopo focus-i)
        new-ids (ff-new-synapse-ids rng ff-bits
                                    (p/in-synapses ff-sg col)
                                    col itopo
                                    focus-coord
                                    radius n-grow)]
    [col new-ids]))



;;; ## Boosting

(defn boost-factor
  "y is the duty cycle value."
  [y neighbour-max crit-ratio max-boost]
  (let [crit-y (double (* neighbour-max crit-ratio))
        maxb max-boost]
    (-> (- maxb (* (- maxb 1)
                   (/ y crit-y)))
        (max 1.0))))

(defn boost-factors-global
  [ys spec]
  (let [crit-ratio (:boost-active-duty-ratio spec)
        max-boost (:max-boost spec)
        max-y (apply max 0 ys)]
    (mapv (fn [y]
            (boost-factor y max-y crit-ratio max-boost))
          ys)))

(defn boost-factors-local
  [ys topo inh-radius spec]
  (let [crit-ratio (:boost-active-duty-ratio spec)
        max-boost (:max-boost spec)]
    (mapv (fn [col y]
            (let [nb-is (p/neighbours-indices topo col inh-radius 0)
                  max-y (apply max 0 (map ys nb-is))]
              (boost-factor y max-y crit-ratio max-boost)))
          (range)
          ys)))

(defn boost-active
  "Recalculates boost factors for each column based on its frequency
   of activation (active duty cycle) compared to the maximum from its
   neighbours."
  [lyr]
  (if-not (pos? (:boost-active-duty-ratio (:spec lyr)))
    ;; disabled
    lyr
    (let [global? (>= (:ff-potential-radius (:spec lyr)) 1)]
      (assoc lyr :boosts
             (if global?
               (boost-factors-global (:active-duty-cycles lyr)
                                     (:spec lyr))
               (boost-factors-local (:active-duty-cycles lyr)
                                    (:topology lyr)
                                    (:inh-radius lyr)
                                    (:spec lyr)))))))

(defn adjust-overlap-global
  [sg ys spec]
  (let [crit-ratio (:adjust-overlap-duty-ratio spec)
        max-y (apply max 0 ys)
        crit-y (double (* max-y crit-ratio))
        upds (keep (fn [[col y]]
                     (when (<= y crit-y)
                       (syn/seg-update [col 0 0] :reinforce nil nil)))
                   (map vector (range) ys))
        pcon (:perm-connected (:proximal spec))]
    (p/bulk-learn sg upds (constantly true) (* 0.1 pcon) 0 0)))

(defn adjust-overlap-local
  [sg ys topo inh-radius spec]
  ;; TODO:
  (adjust-overlap-global sg ys spec))

(defn adjust-overlap
  [lyr]
  (if-not (pos? (:adjust-overlap-duty-ratio (:spec lyr)))
    ;; disabled
    lyr
    (let [global? (>= (:ff-potential-radius (:spec lyr)) 1)]
      (update-in
       lyr [:proximal-sg]
       (fn [sg]
         (if global?
           (adjust-overlap-global sg
                                  (:overlap-duty-cycles lyr)
                                  (:spec lyr))
           (adjust-overlap-local sg
                                 (:overlap-duty-cycles lyr)
                                 (:topology lyr)
                                 (:inh-radius lyr)
                                 (:spec lyr))))))))

(defn float-overlap-global
  [sg ys spec]
  (let [ref-y (:activation-level spec)
        lo-z (:float-overlap-duty-ratio spec)
        hi-z (:float-overlap-duty-ratio-hi spec)
        weaks (keep (fn [[col y]]
                      (let [z (/ y ref-y)]
                        (when (< z lo-z)
                          (syn/seg-update [col 0 0] :reinforce nil nil))))
                    (map vector (range) ys))
        strongs (keep (fn [[col y]]
                        (let [z (/ y ref-y)]
                          (when (> z hi-z)
                            (syn/seg-update [col 0 0] :punish nil nil))))
                      (map vector (range) ys))
        pcon (:perm-connected (:proximal spec))]
    (p/bulk-learn sg (concat weaks strongs)
                  (constantly true) (* 0.1 pcon) (* 0.1 pcon) 0)
    )
  )

(defn layer-float-overlap
  [lyr]
  (if-not (pos? (:float-overlap-duty-ratio (:spec lyr)))
    ;; disabled
    lyr
    (update-in lyr [:proximal-sg]
               (fn [sg]
                 (float-overlap-global sg
                                       (:active-duty-cycles lyr)
                                       (:spec lyr))))))

(defn update-duty-cycles
  "Records a set of events with indices `is` in the vector `v`
   according to duty cycle period `period`. As in NuPIC, the formula
   is

<pre>
y[t] = (period-1) * y[t-1]  +  1
       --------------------------
         period
</pre>"
  [v is period]
  (let [d (/ 1.0 period)
        decay (* d (dec period))]
    (-> (mapv #(* % decay) v)
        (util/update-each is #(+ % d)))))
