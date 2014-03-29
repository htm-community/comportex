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
                                      (+ sp-perm-connected sp-perm-inc)))
        syns (map vector ids perms)
        conn (filter (fn [[_ p]] (>= p sp-perm-connected)) syns)
        disc (remove (fn [[_ p]] (>= p sp-perm-connected)) syns)]
    {:connected (into {} conn)
     :disconnected (into {} disc)}))

(defn column
  [id spec]
  {:id id
   :in-synapses (in-synapses id spec)
   :neighbours #{} ;; cache
   :boost 1.0
   :active-history (sorted-set 0)
   :overlap-history (sorted-set 0)
   :active? false
   })

(defn region
  [{:as spec
    :keys [ncol input-size
           stimulus-threshold]}]
  {:columns (mapv column (range ncol) (repeat spec))
   :input-size input-size
   :spec spec
   :avg-receptive-field 1.0
   :active-cols #{}
   })

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

(defn update-avg-receptive-field-size
  [rgn]
  (assoc rgn :avg-receptive-field
         (mean (map column-receptive-field-size (:columns rgn)))))

(defn neighbours
  [col-id ncol radius]
  (disj (set (range (max 0 (- col-id radius))
                    (min ncol (+ col-id radius))))
        col-id))

(defn update-neighbours
  [rgn]
  (let [ncol (count (:columns rgn))
        radius (:avg-receptive-field rgn)
        cols (mapv (fn [col]
                     (assoc col :neighbours
                            (neighbours (:id col) ncol radius)))
                   (:columns rgn))]
   (assoc rgn :columns cols)))

(defn overlapping
  [col in-set]
  (select-keys (:connected (:synapses col)) in-set))

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

(defn column-update-activation
  [col overlaps t activity-limit]
  (if (zero? (:overlap col))
    col
    (let [ns (:neighbours col)
          nso (select-keys overlaps ns)]
      (if (< (count nso) activity-limit)
        ;; no inhibition within neighbourhood
        (activate col t)
        ;; inhibition within neighbourhood
        (let [ovals (sort > (vals nso))]
          (if (< (nth ovals activity-limit)
                 (+ (:overlap col) ;; break ties:
                    (rand-in -0.1 0.1)))
            (activate col t)
            col))))))

(defn compute-activations
  [rgn t]
  (let [th (:active-per-inh-area)
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
        nsyns (reduce (fn [m [id perm]]
                        (let [newp (if (in-set id)
                                     (min 1.0 (+ perm pinc))
                                     (max 0.0 (- perm pdec)))
                              k (if (>= newp pcon) :connected :disconnected)]
                          (assoc-in m [k id] newp)))
                      {:connected {} ;; TODO transients?
                       :disconnected {}}
                      (concat (:connected syns) (:disconnected syns)))]
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

(defn truncate-duty-cycle-history
  [col t-horizon]
  (let [trunc1 (fn [ss]
                 (let [t0 (first ss)]
                   (if (< t0 t-horizon)
                     (disj ss t0)
                     ss)))]
    (-> col
        (update-in [:active-history] trunc1)
        (update-in [:overlap-history] trunc1))))

(defn column-increase-permanences
  [col pcon]
  (let [inc-vals (fn [m] (into (empty m) (map (fn [[k v]]
                                               [k (min 1.0 (+ v (* pcon 0.1)))])
                                             m)))]
    (-> col
        (update-in [:in-synapses :connected] inc-vals)
        (update-in [:in-synapses :disconnected] inc-vals))))

(defn column-update-boosting
  [col ods-m ads-m o-th a-th maxb pcon]
  (let [ns (:neighbours col)
        max-ods (apply max (vals (select-keys ods-m ns)))
        max-ads (apply max (vals (select-keys ads-m ns)))
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
        a-ds (into {} (map (juxt :id (comp count :active-history)) (:columns rgn)))
        o-ds (into {} (map (juxt :id (comp count :overlap-history)) (:columns rgn)))
        cols (mapv (fn [col]
                     (column-update-boosting col o-ds a-ds o-th a-th maxb pcon))
                   (:columns rgn))
        ]
    (-> (assoc rgn :columns cols)
        (truncate-duty-cycle-history (- t period))
        ;; TODO not every iteration:
        (update-avg-receptive-field-size)
        (update-neighbours))))

(defn pooling-step
  [rgn in-set t]
  (-> rgn
      (compute-overlaps in-set t)
      (compute-activations t)
      (learn in-set)
      (update-boosting t)))
