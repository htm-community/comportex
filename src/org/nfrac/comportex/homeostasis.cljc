(ns org.nfrac.comportex.homeostasis
  "Homeostasis algorithms, including boosting."
  (:require [org.nfrac.comportex.topography :as topo]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.util :as util]
            [clojure.test.check.random :as random]))

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

(defn boost-factor
  "y is the duty cycle value."
  [y neighbour-max crit-ratio max-boost]
  (if (zero? neighbour-max)
    max-boost
    (let [crit-y (double (* neighbour-max crit-ratio))
          maxb max-boost]
      (-> (- maxb (* (- maxb 1)
                     (/ y crit-y)))
          (max 1.0)))))

(defn boost-factors-global
  [ys params]
  (let [crit-ratio (:boost-active-duty-ratio params)
        max-boost (:max-boost params)
        max-y (apply max 0 ys)]
    (mapv (fn [y]
            (boost-factor y max-y crit-ratio max-boost))
          ys)))

(defn boost-factors-local
  [ys topo inh-radius params]
  (let [crit-ratio (:boost-active-duty-ratio params)
        max-boost (:max-boost params)]
    (mapv (fn [col y]
            (let [nb-is (topo/neighbours-indices topo col inh-radius 0)
                  max-y (apply max 0 (map ys nb-is))]
              (boost-factor y max-y crit-ratio max-boost)))
          (range)
          ys)))

(defn boost-active
  "Recalculates boost factors for each column based on its frequency
   of activation (active duty cycle) compared to the maximum from its
   neighbours."
  [lyr]
  (if-not (pos? (:boost-active-duty-ratio (:params lyr)))
    ;; disabled
    lyr
    (let [params (:params lyr)
          global? (>= (:ff-potential-radius params) 1)
          col-topo (topo/make-topography (:column-dimensions params))]
      (assoc lyr :boosts
             (if global?
               (boost-factors-global (:active-duty-cycles lyr)
                                     (:params lyr))
               (boost-factors-local (:active-duty-cycles lyr)
                                    col-topo
                                    (:inh-radius lyr)
                                    (:params lyr)))))))

(defn adjust-overlap-global
  [sg ys params]
  (let [crit-ratio (:adjust-overlap-duty-ratio params)
        max-y (apply max 0 ys)
        crit-y (double (* max-y crit-ratio))
        upds (keep (fn [[col y]]
                     (when (<= y crit-y)
                       (syn/seg-update [col 0 0] :reinforce nil nil)))
                   (map vector (range) ys))
        pcon (:perm-connected (:proximal params))]
    (syn/bulk-learn sg upds (constantly true) (* 0.1 pcon) 0.0 0.0)))

(defn adjust-overlap-local
  [sg ys topo inh-radius params]
  ;; TODO:
  (adjust-overlap-global sg ys params))

(defn adjust-overlap
  [lyr]
  (if-not (pos? (:adjust-overlap-duty-ratio (:params lyr)))
    ;; disabled
    lyr
    (let [params (:params lyr)
          global? (>= (:ff-potential-radius params) 1)
          col-topo (topo/make-topography (:column-dimensions params))]
      (update-in
       lyr [:proximal-sg]
       (fn [sg]
         (if global?
           (adjust-overlap-global sg
                                  (:overlap-duty-cycles lyr)
                                  (:params lyr))
           (adjust-overlap-local sg
                                 (:overlap-duty-cycles lyr)
                                 col-topo
                                 (:inh-radius lyr)
                                 (:params lyr))))))))

(defn float-overlap-global
  [sg ys params]
  (let [ref-y (:activation-level params)
        lo-z (:float-overlap-duty-ratio params)
        hi-z (:float-overlap-duty-ratio-hi params)
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
        pcon (:perm-connected (:proximal params))]
    (syn/bulk-learn sg (concat weaks strongs)
                    (constantly true) (* 0.1 pcon) (* 0.1 pcon) 0.0)))



(defn layer-float-overlap
  [lyr]
  (if-not (pos? (:float-overlap-duty-ratio (:params lyr)))
    ;; disabled
    lyr
    (update-in lyr [:proximal-sg]
               (fn [sg]
                 (float-overlap-global sg
                                       (:active-duty-cycles lyr)
                                       (:params lyr))))))

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
