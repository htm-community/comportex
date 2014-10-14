(ns org.nfrac.comportex.demos.simple-sentences
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]))

(def input-text
  ;; warmup to refine feed-forward synapse fields
"Jane.
Chifung.

Jane has eyes.
Jane has a head.
Jane has a mouth.
Jane has a brain.
Jane has a book.
Jane has no friends.

Chifung has eyes.
Chifung has a head.
Chifung has a mouth.
Chifung has a brain.
Chifung has no book.
Chifung has friends.

Jane is something.
Jane is alive.
Jane is a person.
Jane can talk.
Jane can walk.
Jane can eat.

Chifung is something.
Chifung is alive.
Chifung is a person.
Chifung can talk.
Chifung can walk.
Chifung can eat.

a fox has eyes.
a fox has a head.
a fox has a mouth.
a fox has a brain.
a fox has a tail.
a fox is something.
a fox is alive.
a fox is no person.
a fox can no talk.
a fox can walk.
a fox can eat.

does Jane have eyes ? yes.
does Jane have a head ? yes.
does Jane have a mouth ? yes.
does Jane have a brain ? yes.
does Jane have a book ? yes.
does Jane have friends ? no.
does Jane have a tail ? no.

does Chifung have eyes ? yes.
does Chifung have a head ? yes.
does Chifung have a mouth ? yes.
does Chifung have a brain ? yes.
does Chifung have a book ? no.
does Chifung have friends ? yes.
does Chifung have a tail ? no.

does a fox have eyes ? yes.
does a fox have a head ? yes.
does a fox have a mouth ? yes.
does a fox have a brain ? yes.
does a fox have a book ? no.
does a fox have friends ? no.
does a fox have a tail ? yes.

Jane has no tail.
Chifung has no tail.
")

(defn input-transform-fn
  "Returns an input transform function of [[i j rep]]
   [sentence index, word index, repeat number]"
  [split-sentences n-repeats]
  (fn [[i j rep]]
    (let [sen (get split-sentences i)
          n-sen (count split-sentences)]
      ;; check end of a sentence (+1 for gap)
      (if (== j (count sen))
        ;; reached the end of a sentence
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
        [i (inc j) rep]))))

(def bits-per-word 35)

(defn split-sentences
  [text]
  (->> (str/split (str/trim text) #"\s*\.\s*")
       (mapv #(str/split % #"\s+"))
       ;; need a starting state, otherwise first word always bursts
       ;; and can't learn (no prior cells) so no consistency.
       ;; need an ending state so it can be learned to be predicted.
       (mapv #(vec (concat [">"] % ["."])))))

(defn sensory-input-from-text
  [text n-repeats bits-per-word]
  (let [split-sens (split-sentences text)
        uniq-words (distinct (apply concat split-sens))
        bit-width (* bits-per-word (count uniq-words))
        encoder (enc/pre-transform (fn [[i j _]]
                                     (get-in split-sens [i j]))
                                   (enc/category-encoder bit-width uniq-words))
        xform (input-transform-fn split-sens n-repeats)]
    ;; [sentence index, word index, repeat number]
    (core/sensory-input [0 0 0] xform encoder)))

(def spec
  {:column-dimensions [1000]
   :ff-potential-radius 1.0
   :ff-potential-frac 0.3
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 3
   :global-inhibition false
   :activation-level 0.03
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 8
   :max-segments 5
   :seg-max-synapse-count 18
   :seg-new-synapse-count 12
   :seg-stimulus-threshold 9
   :seg-learn-threshold 7
   :distal-perm-connected 0.20
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-init 0.16
   :distal-punish? false
   })

(defn ^:export n-region-model
  ([text n]
     (n-region-model text n spec))
  ([text n-repeats n spec]
     (let [inp (sensory-input-from-text text n-repeats bits-per-word)]
       (core/regions-in-series core/sensory-region inp n spec))))
