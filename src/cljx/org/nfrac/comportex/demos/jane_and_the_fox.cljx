(ns org.nfrac.comportex.demos.jane-and-the-fox
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]))

(def separator "...then...")

(def sentences
  [
   "Jane saw something"
   "Jane saw the_fox"
   "Jane saw it"
   "Jane ate something"
   "Jane ate the_fox"
   "Jane ate it"

   "Chifung saw something"
   "Chifung saw the_fox"
   "Chifung saw it"
   "Chifung ate something"
   "Chifung ate the_fox"
   "Chifung ate it"
   
   "the_fox saw something"
   "the_fox saw Jane"
   "the_fox saw Chifung"
   "it saw something"
   "it saw Jane"
   "it saw Chifung"
   "it ate something"
   "the_fox ate something"
   "something saw Jane"
   "something saw Chifung"
   "something saw something"
   "something ate something"
   
   "Jane is Jane"
   "Chifung is Chifung"
   "the_fox is the_fox"
   "the_fox is it"
   "it is the_fox"
   "it is it"
   "it is something"
   "the_fox is something"
   "Jane is something"
   "Chifung is something"
   
   "Chifung is not Jane"
   "Jane is not Chifung"
   "the_fox is not Jane"
   "the_fox is not Chifung"
   "it is not Jane"
   "it is not Chifung"
   "Jane is not it"
   "Jane is not the_fox"
   "Chifung is not it"
   "Chifung is not the_fox"

   "the_fox saw me"
   "Chifung saw me"
   "the_fox is not me"
   "Chifung is not me"
   
   "I am not Chifung"
   "I am not the_fox"
   "I am me"
   "I am Jane"
   "I saw something"
   "I ate the_fox"
   ])

(def split-sentences
  (mapv #(str/split % #" ") sentences))

(def words
  (->> split-sentences
       (apply concat)
       (into #{separator})))

(def bit-width 400)

(def n-repeats 3)

;; [sentence index, word index, repeat number]
(def initial-input [0 0 0])

(defn input-transform
  [[i j rep]]
  (let [sen (get split-sentences i)
        n-sen (count split-sentences)]
    (if (== j (count sen))
      ;; reached the end of a sentence (+ 1 for separator)
      (if (== rep (dec n-repeats))
        ;; finished repeating this sentence, move on
        [(mod (inc i) n-sen)
         0
         0]
        ;; next repeat
        [i
         0
         (inc rep)])
      ;; continuing this sentence
      [i (inc j) rep])))

(def encoder
  (enc/pre-transform (fn [[i j _]]
                       (let [sen (get split-sentences i)]
                         (if (== j (count sen)) ;; end of sentence:
                           separator
                           (get sen j))))
                     (enc/category-encoder bit-width words)))

(def spec
  {:column-dimensions [1000]
   :ff-potential-radius-frac 0.2
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 3
   :global-inhibition false
   :activation-level 0.03
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 16
   :max-segments 5
   :seg-max-synapse-count 18
   :seg-new-synapse-count 12
   :seg-stimulus-threshold 9
   :seg-learn-threshold 7
   :distal-perm-connected 0.20
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-init 0.16
   })

(defn ^:export input-gen
  []
  (core/sensory-input initial-input input-transform encoder))

(defn ^:export n-region-model
  ([n]
     (n-region-model n spec))
  ([n spec]
     (core/regions-in-series core/sensory-region (input-gen) n spec)))
