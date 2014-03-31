(ns org.nfrac.comportex.pooling
  (:require [clojure.data.generators :as gen]
            [clojure.set :as set]))

(def spatial-pooler-defaults
  {:ncol 100 ;; TODO dimensions of columns can be 2d, even 3d
   :input-size 300
   :potential-radius 100
   :potential-pct 0.5
   :active-per-inh-area 10
   :sp-perm-inc 0.1
   :sp-perm-dec 0.01
   :sp-perm-connected 0.1
   :stimulus-threshold 5
   :boost-overlap-duty-ratio 0.001
   :boost-active-duty-ratio 0.001
   :duty-cycle-period 1000
   :max-boost 10.0
   })

(defn rand-in
  [lower upper]
  {:pre [(<= lower upper)]}
  (+ lower (* (gen/double)
              (- upper lower))))

(defn into-connected-disconnected
  [pcon synapses]
  (-> (group-by (fn [[id perm]]
                  (if (>= perm pcon) :connected :disconnected))
                synapses)
      (update-in [:connected] #(into {} %))
      (update-in [:disconnected] #(into {} %))))

(defn in-synapses
  [column-id
   {:as spec
    :keys [ncol
           input-size
           potential-radius
           potential-pct
           sp-perm-connected
           sp-perm-inc]}]
  (let [input-focus (* input-size (/ column-id ncol))
        idseq (range (max 0 (- input-focus potential-radius))
                     (min input-size (+ input-focus potential-radius)))
        n (* potential-pct (count idseq))
        ids (take n (gen/shuffle idseq))
        perms (repeatedly n #(rand-in (- sp-perm-connected sp-perm-inc)
                                      (+ sp-perm-connected sp-perm-inc)))]
    (->> (map vector ids perms)
         (into-connected-disconnected sp-perm-connected))))

(defn column
  [id spec]
  {:id id
   :in-synapses (in-synapses id spec)
   :neighbours #{} ;; cache
   :boost 1.0
   :active-history (sorted-set) ;; set of timesteps
   :overlap-history (sorted-set)
   :active? false})

(declare update-neighbours)

(defn region
  [{:as spec
    :keys [ncol]}]
  (-> {:columns (mapv column (range ncol) (repeat spec))
       :spec spec
       :active-columns #{}
       :timestep 0}
      (update-neighbours)))

(defn mean
  [xs]
  (/ (apply + xs) (double (count xs))))

(defn absint
  [x]
  (Math/abs (long x)))

(defn column-receptive-field-size
  [col]
  (let [idxs (keys (:connected (:in-synapses col)))
        centr (mean idxs)]
    (mean (map #(absint (- % centr)) idxs))))

(defn avg-receptive-field-size
  [rgn]
  (mean (map column-receptive-field-size (:columns rgn))))

(defn neighbours
  [col-id ncol radius]
  (let [r (Math/round (double radius))]
   (disj (set (range (max 0 (- col-id r))
                     (min ncol (+ col-id r 1))))
         col-id)))

(defn update-neighbours
  [rgn]
  (let [ncol (count (:columns rgn))
        radius (avg-receptive-field-size rgn)
        cols (mapv (fn [col]
                     (assoc col :neighbours
                            (neighbours (:id col) ncol radius)))
                   (:columns rgn))]
    (assoc rgn :columns cols
           :avg-receptive-field-size radius)))

(defn overlapping
  [col in-set]
  (select-keys (:connected (:in-synapses col)) in-set))

(defn column-update-overlap
  [col in-set t stimulus-threshold]
  (let [os (overlapping col in-set)
        overlap? (>= (count os) stimulus-threshold)
        x (if overlap?
            (* (count os) (:boost col))
            0.0)
        col (assoc col :overlap x)]
    (if overlap?
      (update-in col [:overlap-history] conj t)
      col)))

(defn compute-overlaps
  [rgn in-set t]
  (let [th (:stimulus-threshold (:spec rgn))
        cols (mapv (fn [col]
                     (column-update-overlap col in-set t th))
                   (:columns rgn))]
    (assoc rgn :columns cols)))

(defn activate
  [col t]
  (-> col
      (assoc :active? true)
      (update-in [:active-history] conj t)))

(defn deactivate
  [col]
  (assoc col :active? false))

(defn column-update-activation
  [col overlaps t activity-limit]
  (if (zero? (:overlap col))
    (deactivate col)
    (let [ns (:neighbours col)
          nso (select-keys overlaps ns)]
      (if (< (count nso) activity-limit)
        ;; no inhibition within neighbourhood
        (activate col t)
        ;; inhibition within neighbourhood
        (let [ovals (sort > (vals nso))]
          (if (< (nth ovals (dec activity-limit))
                 (+ (:overlap col) ;; break ties:
                    (rand-in -0.1 0.1)))
            (activate col t)
            (deactivate col)))))))

(defn compute-activations
  [rgn t]
  (let [th (:active-per-inh-area (:spec rgn))
        overlaps (into {} (keep (fn [col]
                                  (when-not (zero? (:overlap col))
                                    [(:id col) (:overlap col)]))
                                (:columns rgn)))
        cols (mapv (fn [col]
                     (column-update-activation col overlaps t th))
                   (:columns rgn))
        act (set (keep (fn [col]
                         (when (:active? col) (:id col)))
                       cols))]
    (assoc rgn :columns cols
           :active-columns act)))

(defn column-update-permanences
  [col in-set pinc pdec pcon]
  (let [syns (:in-synapses col)
        nsyns (->> (concat (:connected syns)
                           (:disconnected syns))
                   (mapv (fn [[id perm]]
                           (let [newp (if (in-set id)
                                        (min 1.0 (+ perm pinc))
                                        (max 0.0 (- perm pdec)))]
                             [id newp])))
                   (into-connected-disconnected pcon))]
    (assoc col :in-synapses nsyns)))

(defn learn
  [rgn in-set]
  (let [pinc (:sp-perm-inc (:spec rgn))
        pdec (:sp-perm-dec (:spec rgn))
        pcon (:sp-perm-connected (:spec rgn))
        cols (reduce (fn [cols i]
                       (update-in cols [i]
                                  (fn [col]
                                    (column-update-permanences col in-set pinc pdec pcon))))
                     (:columns rgn) (:active-columns rgn))]
    (assoc rgn :columns cols)))

(defn column-truncate-duty-cycle-history
  [col t-horizon]
  (let [trunc (fn [ss]
                (let [t0 (first ss)]
                  (if (and t0 (< t0 t-horizon))
                    (recur (disj ss t0))
                    ss)))]
    (-> col
        (update-in [:active-history] trunc)
        (update-in [:overlap-history] trunc))))

(defn column-increase-permanences
  [col pcon]
  (let [syns (:in-synapses col)
        nsyns (->> (concat (:connected syns)
                           (:disconnected syns))
                   (mapv (fn [[id perm]]
                           (let [newp (min 1.0 (+ perm (* pcon 0.1)))]
                             [id newp])))
                   (into-connected-disconnected pcon))]
    (assoc col :in-synapses nsyns)))

(defn column-update-boosting
  [col ods-m ads-m o-th a-th maxb pcon]
  (let [ns (:neighbours col)
        max-ods (apply max 1 (vals (select-keys ods-m ns)))
        max-ads (apply max 1 (vals (select-keys ads-m ns)))
        crit-ods (* o-th max-ods)
        crit-ads (* a-th max-ads)
        ods (count (:overlap-history col))
        ads (count (:active-history col))
        nboost (-> (- maxb (* (- maxb 1)
                              (/ ads crit-ads)))
                   (max 1.0)
                   (double))]
    (->
     (if (< ods crit-ods)
       (column-increase-permanences col pcon)
       col)
     (assoc :boost nboost))))

(defn update-boosting
  [rgn t]
  (let [o-th (:boost-overlap-duty-ratio (:spec rgn))
        a-th (:boost-active-duty-ratio (:spec rgn))
        period (:duty-cycle-period (:spec rgn))
        maxb (:max-boost (:spec rgn))
        pcon (:sp-perm-connected (:spec rgn))
        rolling (fn [ss] (count (subseq ss >= (- t period))))
        ads-m (into {} (map (juxt :id (comp rolling :active-history)) (:columns rgn)))
        ods-m (into {} (map (juxt :id (comp rolling :overlap-history)) (:columns rgn)))
        cols (mapv (fn [col]
                     (column-update-boosting col ods-m ads-m o-th a-th maxb pcon))
                   (:columns rgn))]
    (assoc rgn :columns cols)))

(defn pooling-step
  [rgn in-set]
  (let [t (inc (:timestep rgn))
        dcp (:duty-cycle-period (:spec rgn))
        boost? (zero? (mod t (quot dcp 2)))
        neigh? (zero? (mod t (quot dcp 2)))]
    (cond-> rgn
            true (compute-overlaps in-set t)
            true (compute-activations t)
            true (learn in-set)
            boost? (update-boosting t)
            neigh? (update-neighbours)
            true (assoc :timestep t))))
