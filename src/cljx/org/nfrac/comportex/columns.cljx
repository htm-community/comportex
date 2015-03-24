(ns org.nfrac.comportex.columns
  "Handling of column excitation through proximal dendrite segment
   synapses.

   **Argument name conventions:**

   * `col` -- a column id, an integer index in the region.
   * `a-cols` -- the set of ids of active columns.
   * `om` -- overlap scores in a map keyed by column id.
   * `ff-bits` -- the set of indexes of any active feed-forward input bits.
"
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
  "Given a map `om` of the raw overlap counts, finds the columns with
  an overlap count above parameter `ff-stimulus-threshold`, and
  returns a map of their column ids to excitations. That is, the
  number of input bits connected to, multiplied by the column boosting
  factor."
  [om-raw boosts spec]
  (let [th (:ff-stimulus-threshold spec)]
    (->> om-raw
         (reduce-kv (fn [om col x]
                      (if (< x th)
                        om
                        (let [b (get boosts col)]
                          (assoc! om col (* x b)))))
                    (transient {}))
         (persistent!))))

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
    (p/conj-synapses ff-sg col new-ids pinit)))

(defn learn-proximal
  "Adapt feed-forward synapses to focus on observed input patterns.
   Given the set of input bits `ff-bits`, adjusts the permanence
   values of all potential feed-forward synapses in the active columns
   `a-cols`."
  [sg itopo topo a-cols ff-bits om spec]
  (let [pinc (:ff-perm-inc spec)
        pdec (:ff-perm-dec spec)
        p-lo (:ff-perm-init-lo spec)
        p-hi (:ff-perm-init-hi spec)
        grow-and-die? (:ff-grow-and-die? spec)
        grow-up-to (:ff-grow-up-to-count spec)
        max-syns (:ff-max-synapse-count spec)
        ;; radius in input space, fraction of longest dimension
        radius (long (* (:ff-potential-radius spec)
                        (apply max (p/dimensions itopo))))
        n-cols (p/size topo)]
    (reduce (fn [sg col]
              (let [n-on (om col)
                    n-grow (max 0 (- grow-up-to n-on))]
                (cond->
                 (p/reinforce-in-synapses sg col (constantly false)
                                          ff-bits pinc pdec)
                 (and grow-and-die? (pos? n-grow))
                 (grow-new-synapses col ff-bits itopo radius n-cols
                                    n-grow (util/rand p-lo p-hi)))))
            sg a-cols)))

;;; ## Boosting

(defn boost-overlap-global
  [cf]
  (let [spec (:spec cf)
        o-th (:boost-overlap-duty-ratio spec)
        pinc (:ff-perm-inc spec)
        ods (:overlap-duty-cycles cf)
        max-od (apply max 0 ods)
        crit-od (* o-th max-od)]
    (update-in cf [:proximal-sg]
               (fn [psg]
                 (reduce (fn [psg col]
                           (let [od (get ods col)]
                             (if (< od crit-od)
                               (p/reinforce-in-synapses psg col (constantly false)
                                                        (constantly true) pinc 0)
                               psg)))
                         psg
                         (range (count ods)))))))

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
  [cf]
  (let [global? (>= (:ff-potential-radius (:spec cf)) 1)]
    ;; TODO for local case, partition the column space based on radius...
    (if-not (pos? (:boost-active-duty-ratio (:spec cf)))
      cf
      (assoc cf :boosts
             (boost-active-global (:active-duty-cycles cf) (:spec cf))))))

(defn boost-overlap
  "Increases all synapse permanences connected to a column if its
   frequency of overlap with input (overlap duty cycle) is low
   compared to the maximum from its neighbours."
  [cf]
  (let [global? (>= (:ff-potential-radius (:spec cf)) 1)]
    ;; TODO for local case, partition the column space based on radius...
    (if-not (pos? (:boost-overlap-duty-ratio (:spec cf)))
      cf
      (boost-overlap-global cf))))

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
