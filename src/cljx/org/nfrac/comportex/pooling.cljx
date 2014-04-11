(ns org.nfrac.comportex.pooling
  (:require [org.nfrac.comportex.util :as util :refer [round mean]]
            [clojure.set :as set]))

(def spatial-pooler-defaults
  {:ncol 400
   :input-size 400
   :potential-radius 80
   :potential-pct 0.5
   :global-inhibition true
   :activation-level 0.04
   :sp-perm-inc 0.05
   :sp-perm-dec 0.01
   :sp-perm-connected 0.1
   :stimulus-threshold 3
   :boost-overlap-duty-ratio 0.001
   :boost-active-duty-ratio 0.001
   :duty-cycle-period 1000
   :max-boost 10.0
   })

;; CONSTRUCTION

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
        ids (take n (util/shuffle idseq))
        perms (repeatedly n #(util/rand (- sp-perm-connected sp-perm-inc)
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
   :overlap-history (sorted-set)})

(declare update-neighbours)

(defn region
  [{:as spec
    :keys [ncol]}]
  (-> {:columns (mapv column (range ncol) (repeat spec))
       :spec spec
       :active-columns #{}}
      (update-neighbours)))

;; NEIGHBOURING COLUMNS

(defn column-receptive-field-size
  [col]
  (let [idxs (keys (:connected (:in-synapses col)))]
    (if (seq idxs)
      (- (apply max idxs) (apply min idxs))
      0)))

(defn avg-receptive-field-size
  [rgn]
  (mean (map column-receptive-field-size (:columns rgn))))

(defn neighbours
  [col-id ncol r]
  (disj (set (range (max 0 (- col-id r))
                    (min ncol (+ col-id r 1))))
        col-id))

(defn update-neighbours
  [rgn]
  (let [spec (:spec rgn)
        nin (:input-size spec)
        ncol (:ncol spec)
        arfs (avg-receptive-field-size rgn)
        diameter (round (* ncol (/ arfs nin)))
        radius (quot diameter 2)
        cols (mapv (fn [col]
                     (assoc col :neighbours
                            (neighbours (:id col) ncol radius)))
                   (:columns rgn))]
    (assoc rgn :columns cols
           :avg-receptive-field-size radius)))

;; OVERLAPS

(defn column-overlap
  [col in-set stimulus-threshold]
  ;; pooler is 10% faster with reduce here, vs select-keys
  (let [ov (reduce (fn [sum id]
                     (if (in-set id) (inc sum) sum))
                   0 ;; init
                   (keys (:connected (:in-synapses col))))]
    (if (>= ov stimulus-threshold)
      (* ov (:boost col))
      0.0)))

(defn overlaps
  [rgn in-set]
  (let [th (:stimulus-threshold (:spec rgn))]
    (into {} (keep (fn [col]
                     (let [ov (column-overlap col in-set th)]
                       (when-not (zero? ov)
                         [(:id col) ov])))
                   (:columns rgn)))))

(defn column-record-overlap
  [col t]
  (update-in col [:overlap-history] conj t))

(defn update-overlaps
  [rgn in-set t]
  (let [om (overlaps rgn in-set)]
    (->
     (reduce (fn [r [id _]]
               (update-in r [:columns id] column-record-overlap t))
             rgn om)
     (assoc :overlaps om))))

;; ACTIVATION

(defn column-active-with-local-inhibition?
  [col om level]
  (when-let [o-val (om (:id col))]
    (let [ns (:neighbours col)
          nom (select-keys om ns)
          activity-limit (-> (* level (inc (count ns)))
                             (round)
                             (max 1))]
      (if (< (count nom) activity-limit)
        ;; no inhibition within neighbourhood
        true
        ;; inhibition within neighbourhood;
        ;; check number of neighbouring columns dominating col
        (let [o-val* (+ o-val (util/rand -0.1 0.1)) ;; break ties
              n (count (filterv #(> % o-val*) (vals nom)))]
          (< n activity-limit))))))

(defn active-columns-with-local-inhibition
  [rgn om level]
  (set (keep (fn [col]
               (when (column-active-with-local-inhibition? col om level)
                 (:id col)))
             (:columns rgn))))

(defn active-columns-with-global-inhibition
  [rgn om level]
  (let [activity-limit (-> (* level (count (:columns rgn)))
                           (round)
                           (max 1))]
    (if (<= (count om) activity-limit)
      ;; no inhibition
      (set (keys om))
      ;; inhibition
      (->> om
           (sort-by val >)
           (take activity-limit)
           (map first)
           (set)))))

(defn active-columns
  [rgn om]
  (let [spec (:spec rgn)
        level (:activation-level spec)]
    (if (:global-inhibition spec)
      (active-columns-with-global-inhibition rgn om level)
      (active-columns-with-local-inhibition rgn om level))))

(defn column-record-activation
  [col t]
  (update-in col [:active-history] conj t))

(defn update-active-columns
  [rgn t]
  (let [as (active-columns rgn (:overlaps rgn))]
    (->
     (reduce (fn [r id]
               (update-in r [:columns id] column-record-activation t))
             rgn as)
     (assoc :active-columns as))))

;; LEARNING

(defn column-update-in-synapses
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
                                    (column-update-in-synapses col in-set pinc pdec pcon))))
                     (:columns rgn) (:active-columns rgn))]
    (assoc rgn :columns cols)))

;; BOOSTING

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
        rollcnt (fn [ss] (count (subseq ss >= (- t period))))
        ads-m (into {} (map (juxt :id (comp rollcnt :active-history)) (:columns rgn)))
        ods-m (into {} (map (juxt :id (comp rollcnt :overlap-history)) (:columns rgn)))
        cols (mapv (fn [col]
                     (column-update-boosting col ods-m ads-m o-th a-th maxb pcon))
                   (:columns rgn))]
    (assoc rgn :columns cols)))

;; not currently used; just to limit memory use
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

;; ORCHESTRATION

(defn pooling-step
  [rgn in-set]
  (let [t (inc (:timestep rgn 0))
        dcp (:duty-cycle-period (:spec rgn))
        boost? (zero? (mod t (quot dcp 2)))
        neigh? (zero? (mod t (quot dcp 2)))]
    (cond-> rgn
            true (assoc :timestep t)
            true (update-overlaps in-set t)
            true (update-active-columns t)
            true (learn in-set)
            boost? (update-boosting t)
            neigh? (update-neighbours))))
