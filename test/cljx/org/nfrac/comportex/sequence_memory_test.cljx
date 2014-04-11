(ns org.nfrac.comportex.sequence-memory-test
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.sequence-memory :as sm]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.set :as set]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.cljs.test
                           :refer (is deftest testing run-tests)]))

(def bit-width 200)
(def numb-domain [0 30])
(def numb-span 1.2)
(def ncol 200)
(def depth 5)

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
  (let [gap #(repeat (util/rand-int lower upper) nil)]
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

(defn cla-seq
  [rgn bitset-inps]
  (reductions (fn [r in]
                (let [r2 (p/pooling-step r in)]
                  (sm/sequence-memory-step r2 (:active-columns r2))))
              rgn
              bitset-inps))

(deftest sm-test
  (let [ps (mix-patterns-with-gaps patterns [1 50])
        ;; TODO add noise?
        efn (enc/merge-encoder
             (enc/number-linear bit-width numb-domain numb-span))
        inpseq (map efn (map :values ps))
        r* (p/region (assoc p/spatial-pooler-defaults
                       :ncol ncol
                       :input-size bit-width
                       :potential-radius (quot bit-width 5)
                       :global-inhibition false
                       :stimulus-threshold 2
                       :duty-cycle-period 500))
        r (sm/with-sequence-memory r* (assoc sm/sequence-memory-defaults
                                        :depth depth))]
    (testing "CLA with sequence memory runs"
      (let [r1k (time
                 (-> (cla-seq r inpseq)
                     (nth 1000)))]
        (let [ncells (map (comp count :cells) (:columns r1k))]
          (is (every? #(= depth %) ncells)
              "All columns have the specified number of cells."))
        (let [nsegs (map (fn [c] (count (mapcat :segments (:cells c))))
                         (:columns r1k))]
          (is (every? pos? nsegs)
              "All columns have grown at least one dendrite segment."))))))


(comment
  (require 'org.nfrac.comportex.sequence-memory-test :reload-all)
  (in-ns 'org.nfrac.comportex.sequence-memory-test)
  (use 'clojure.pprint)
  (use 'clojure.repl)

  (def ps (mix-patterns-with-gaps patterns [1 50]))
  (pprint (map :patterns (take 100 ps)))
  (pprint (map :values (take 100 ps)))

  ;; TODO add noise

  (def efn (enc/merge-encoder
            (enc/number-linear bit-width numb-domain numb-span)))
  
  (pprint (map efn (map :values (take 20 ps))))
  (map count (map efn (map :values (take 100 ps))))

  )
