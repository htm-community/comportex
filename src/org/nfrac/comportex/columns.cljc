(ns org.nfrac.comportex.columns
  "Handling of column-level proximal synapses - initialisation and boosting."
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util
             :refer [abs round mean count-filter remap]]))

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
  [topo itopo spec]
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
        [cw ch] (p/dimensions topo)
        [iw ih] (p/dimensions itopo)]
    (if global?
      (->> (range n-cols)
           (mapv (fn [col]
                   (let [n (round (* frac input-size))
                         ids (repeatedly n #(util/rand-int (dec input-size))) ;; ignore dups
                         perms (repeatedly n #(util/rand p-lo p-hi))]
                     (zipmap ids perms)))))
      (->> (range n-cols)
           (mapv (fn [col]
                   (let [focus-i (if one-d?
                                   (round (* input-size (/ col n-cols)))
                                   (let [[cx cy] (p/coordinates-of-index topo col)]
                                     (p/index-of-coordinates itopo [(round (* iw (/ cx cw)))
                                                                    (round (* ih (/ cy ch)))])))
                         all-ids (vec (p/neighbours-indices itopo focus-i radius))
                         n (round (* frac (count all-ids)))
                         ids (if (< frac 0.4) ;; for performance:
                               (util/sample n all-ids)
                               (util/reservoir-sample n all-ids))
                         perms (repeatedly n #(util/rand p-lo p-hi))]
                     (zipmap ids perms))))))))

;;; ## Overlaps

(defn apply-overlap-boosting
  "Given a map `exc` of the column overlap counts, filters it down
  to those meeting parameter `ff-stimulus-threshold`, and
  multiplies the excitation value by the column boosting factor."
  [exc boosts]
  (->> exc
       (reduce-kv (fn [m [col _] x]
                    (let [b (get boosts col)]
                      (assoc! m col (* x b))))
                  (transient {}))
       (persistent!)))

;;; ## Learning

(defn ff-new-synapse-ids
  [ff-bits curr-ids-set col itopo focus-coord radius n-grow]
  (loop [ids ()
         on-bits (util/shuffle ff-bits)]
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
  [ff-sg col ff-bits itopo radius n-cols n-grow pinit]
  (let [input-size (p/size itopo)
        focus-i (round (* input-size (/ col n-cols)))
        focus-coord (p/coordinates-of-index itopo focus-i)
        new-ids (ff-new-synapse-ids ff-bits
                                    (p/in-synapses ff-sg col)
                                    col itopo
                                    focus-coord
                                    radius n-grow)]
    [col new-ids]))



;;; ## Boosting

(defn boost-active-global
  [ads spec]
  (let [a-th (:boost-active-duty-ratio spec)
        maxb (:max-boost spec)
        max-ad (apply max 0 ads)
        crit-ad (double (* a-th max-ad))]
    (mapv (fn [ad]
            (-> (- maxb (* (- maxb 1)
                           (/ ad crit-ad)))
                (max 1.0)))
          ads)))

(defn boost-active
  "Recalculates boost factors for each column based on its frequency
   of activation (active duty cycle) compared to the maximum from its
   neighbours."
  [lyr]
  (let [global? (>= (:ff-potential-radius (:spec lyr)) 1)]
    ;; TODO for local case, partition the column space based on radius...
    (if-not (pos? (:boost-active-duty-ratio (:spec lyr)))
      lyr
      (assoc lyr :boosts
             (boost-active-global (:active-duty-cycles lyr) (:spec lyr))))))

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
