(ns org.nfrac.comportex.sequence-memory-test
  (:use clojure.test)
  (:require (org.nfrac.comportex [pooling :as p]
                                 [sequence-memory :as sm]
                                 [encoders :as enc])
            [clojure.data.generators :as gen]
            [clojure.set :as set]))

(def PATTERN_DOMAIN [0 30])
(def INPUT_BIT_WIDTH 200)

(def patterns
  {:run0 (range 5)
   :twos (range 0 10 2)
   :reps (mapcat #(repeat 2 %) (range 5))
   :run20 (range 20 25)
   :rev20 (reverse (range 20 25))
   :fives (range 0 30 5)
   :bounce (map + (repeat 10)
                (concat (range 5) (range 4) (range 3) (range 2) (range 1)))
   })

(defn repeat-with-gaps
  [xs [lower upper]]
  (let [gap #(repeat (gen/uniform lower upper) nil)]
    (apply concat (interpose xs (repeatedly gap)))))

(defn mix-patterns-with-gaps
  [patterns gap-range]
  (let [tagseqs (map (fn [[k xs]]
                       (->> (repeat-with-gaps xs gap-range)
                            (map (fn [x] {:id (if x k) :val x}))))
                     patterns)]
    (apply map (fn [& ms] {:patterns (set (keep :id ms))
                          :values (set (keep :val ms))})
           tagseqs)))


(comment
  (require 'org.nfrac.comportex.sequence-memory-test :reload-all)
  (in-ns 'org.nfrac.comportex.sequence-memory-test)
  (use 'clojure.pprint)
  (use 'clojure.repl)

  (def ps (mix-patterns-with-gaps patterns [1 50]))
  (pprint (map :patterns (take 100 ps)))
  (pprint (map :values (take 100 ps)))

  ;; TODO add noise

  (def efn (enc/union-encoder
            (enc/number-linear INPUT_BIT_WIDTH PATTERN_DOMAIN 1.2)))
  
  (pprint (map efn (map :values (take 20 ps))))
  (map count (map efn (map :values (take 100 ps))))
  
  (def r* (p/region (assoc p/spatial-pooler-defaults
                     :ncol 200
                     :input-size INPUT_BIT_WIDTH
                     :potential-radius (quot INPUT_BIT_WIDTH 5)
                     :global-inhibition false
                     :stimulus-threshold 2
                     :duty-cycle-period 500)))

  (def r (sm/with-sequence-memory r* sm/sequence-memory-defaults))

  (time
   (def r1k (reduce (fn [r in]
                      (let [r2 (p/pooling-step r in)]
                        (sm/sequence-memory-step r2 (:active-columns r2))))
                    r
                    (map efn (map :values (take 1000 ps))))))

  (sort (map (comp count :overlap-history) (:columns r1k)))
  (sort (map (comp count :active-history) (:columns r1k)))
  (sort (map :boost (:columns r1k)))

  ;; number of active columns each time step
  (for [t (range 900 1000)]
    (reduce (fn [sum col]
              (if (contains? (:active-history col) t)
                (inc sum) sum))
            0 (:columns r1k)))
  
  )
