(ns org.nfrac.comportex.demos.mixed-gaps-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]))

(def bit-width 400)
(def on-bits 25)
(def numb-max 15)
(def numb-domain [0 numb-max])

(def patterns
  {:run-0-5 [0 1 2 3 4 5]
   :rev-5-1 [5 4 3 2 1]
   :run-6-10 [6 7 8 9 10]
   :twos [0 2 4 6 8 10 12 14]
   :reps-0-5 [0 0 1 1 2 2 3 3 4 4 5 5]
   :jump-7-11 [5 6 7 11 12]
   :saw-10-15 [10 12 11 13 12 14 13 15]})

(def gap-range
  (->> (vals patterns) (map count) (reduce +) (long)))

(defn initial-input
  []
  (mapv (fn [[k xs]]
          {:name k, :seq xs, :index nil,
           :gap-countdown (util/rand-int 0 gap-range)})
        patterns))

(defn input-transform
  [ms]
  (mapv (fn [m]
          (cond
           ;; reached end of sequence; begin gap
           (= (:index m) (dec (count (:seq m))))
           (assoc m :index nil
                  :gap-countdown (util/rand-int 0 gap-range))
           ;; in gap
           (and (not (:index m))
                (pos? (:gap-countdown m)))
           (update-in m [:gap-countdown] dec)
           ;; reached end of gap; restart sequence
           (and (not (:index m))
                (zero? (:gap-countdown m)))
           (assoc m :index 0)
           ;; in sequence
           :else
           (update-in m [:index] inc)))
        ms))

(defn current-value
  [m]
  (when (:index m)
    (get (:seq m) (:index m))))

(def encoder
  (enc/ensplat
   (enc/pre-transform current-value
                      (enc/linear-encoder bit-width on-bits numb-domain))))

(def spec
  {:ncol 1000
   :potential-radius 128
   :activation-level 0.03
   :global-inhibition true
   :stimulus-threshold 3
   :sp-perm-inc 0.05
   :sp-perm-dec 0.01
   :sp-perm-connected 0.20
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 8
   :new-synapse-count 12
   :activation-threshold 9
   :min-threshold 7
   :connected-perm 0.20
   :initial-perm 0.16
   :permanence-inc 0.05
   :permanence-dec 0.01
   })

(defn ^:export model
  []
  (let [gen (core/input-generator (initial-input) input-transform encoder)]
    (core/tree core/cla-region (assoc spec :ncol 500)
               [(core/tree core/cla-region spec
                           [gen])])))



(comment
  (require :reload 'org.nfrac.comportex.demos.mixed-gaps-1d)
  (in-ns 'org.nfrac.comportex.demos.mixed-gaps-1d)
  (use 'clojure.repl)

(defn get-regions
  [state]
  (->> (tree-seq :subs :subs state)
       (keep :region)
       (reverse) ;; put in bottom to top order, for first path down tree
       (vec)))

(defn get-input
  [state]
  (->> (tree-seq :subs :subs state)
       (remove :region)
       ;; for now, assume only one input)
       (first)))
  
  (def m1k
    (->> (iterate core/feed-forward-step (model))
         (map (fn [m]
                (let [[p1 p2] (get-regions m)]
                  {:input (core/domain-value (get-input m))
                   :p1-freqs (core/column-state-freqs p1)
                   :p2-freqs (core/column-state-freqs p2)
                   :p1-sac (:signal-cells p1)
                   :p1-ac (:active-cells p1)
                   :p2-ac (:active-cells p2)
                   :p2-tpc (keys (:temporal-pooling-scores p2))})))
         (take 1000)))

  (time (count m1k))

  ;; check that numbers of correctly predicted columns are increasing
  (use 'clojure.pprint)
  (pprint
   (for [layer [:p1-freqs :p2-freqs]
         state [:active-predicted :active]]
     (->> m1k
          (map layer)
          (map state)
          (partition 100 100)
          (map util/mean)
          (vector layer state))))

  ;; check that input bits corresponding to tails of sequences are
  ;; predicted. i.e. ignore the input bits of the unpredictable head.

  ;; shortcut: just ignore any time steps with a sequence head.
  (defn pattern-starting?
    [patt]
    (when-let [i (:index patt)] (zero? i)))

  (doseq [layer [:p1-freqs :p2-freqs]
          state [:active-predicted :active]]
    (->> m10k
         (remove #(some pattern-starting? (:input %)))
         (map layer)
         (map state)
         (partition 1000 1000)
         (map util/mean)
         (vector layer state)
         (println)))
  
  ;; look for cells in p2 that correspond to specific input sequences
  (def last1k (take-last 1000 m10k))

  (def patt-key (->> (first last1k)
                     :input
                     (map :name)
                     (map-indexed (fn [i k] [k i]))
                     (into {})))

  (defn in-body-of-pattern?
    [k input]
    (let [patt (get input (patt-key k))]
      (when-let [i (:index patt)] (pos? i))))
  
  (defn out-of-pattern?
    [k input]
    (let [patt (get input (patt-key k))]
      (nil? (:index patt))))

  (defn cells-in-out
    [ts k]
    (let [in-steps (filter #(in-body-of-pattern? k (:input %)) ts)
          out-steps (filter #(out-of-pattern? k (:input %)) ts)]
      {:name k
       :n-total (count ts)
       :n-in (count in-steps)
       :n-out (count out-steps)
       :freqs-in (frequencies (mapcat :p2-ac in-steps))
       :freqs-out (frequencies (mapcat :p2-ac out-steps))}
      ))

  (def run05 (cells-in-out last1k :run-0-5))
  (def twos (cells-in-out last1k :twos))
  (def saw1015 (cells-in-out last1k :saw-10-15))
  (:n-in run05ns)
  (:n-in twosns)
  (:n-in saw1015)
  (apply max-key val (:freqs-in run05ns))
  (apply max-key val (:freqs-in twosns))
  (apply max-key val (:freqs-in saw1015))
  
  
  
  )
