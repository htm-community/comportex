(ns org.nfrac.comportex.demos.simple-sentences
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]
            #+clj [clojure.core.async :as async]
            #+cljs [cljs.core.async :as async]))

(def bits-per-word 35)

(def spec
  {:column-dimensions [1000]
   :ff-init-frac 0.3
   :ff-potential-radius 1.0
   :ff-perm-inc 0.05
   :ff-perm-dec 0.005
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 1
   :global-inhibition? true
   :activation-level 0.02
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
   :distal-vs-proximal-weight 0.5
   })

(def input-text
  ;; warmup to refine feed-forward synapse fields
"Jane.
Chifung.

Jane has eyes.
Jane has a head.
Jane has a mouth.
Jane has a brain.
Jane has a book.
Jane has no friend.

Chifung has eyes.
Chifung has a head.
Chifung has a mouth.
Chifung has a brain.
Chifung has no book.
Chifung has a friend.

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

fox has eyes.
fox has a head.
fox has a mouth.
fox has a brain.
fox has a tail.
fox is something.
fox is alive.
fox is no person.
fox can no talk.
fox can walk.
fox can eat.

does Jane have eyes ? yes.
does Jane have a head ? yes.
does Jane have a mouth ? yes.
does Jane have a brain ? yes.
does Jane have a book ? yes.
does Jane have a friend ? no.
does Jane have a tail ? no.

does Chifung have eyes ? yes.
does Chifung have a head ? yes.
does Chifung have a mouth ? yes.
does Chifung have a brain ? yes.
does Chifung have a book ? no.
does Chifung have a friend ? yes.
does Chifung have a tail ? no.

does fox have eyes ? yes.
does fox have a head ? yes.
does fox have a mouth ? yes.
does fox have a brain ? yes.
does fox have a book ? no.
does fox have a friend ? no.
does fox have a tail ? yes.

Jane has no tail.
Chifung has no tail.
")

(defn split-sentences
  [text]
  (->> (str/split (str/trim text) #"[^\w]*\.+[^\w]*")
       (mapv #(str/split % #"[^\w']+"))
       ;; add a start token, to avoid bursting the first word.
       (mapv #(vec (concat [">"] % ["."])))))

(defn word-item-seq
  "An input sequence consisting of words from the given text, with
   periods separating sentences also included as distinct words. Each
   sequence element has the form `{:word _, :index [i j]}`, where i is
   the sentence index and j is the word index into sentence j."
  [n-repeats text]
  (for [[i sen] (map-indexed vector (split-sentences text))
        rep (range n-repeats)
        [j word] (map-indexed vector sen)]
    {:word word :index [i j]}))

(defn make-encoder
  [text]
  (let [split-sens (split-sentences text)
        uniq-words (distinct (apply concat split-sens))
        bit-width (* bits-per-word (count uniq-words))]
    (enc/pre-transform :word
                       (enc/category-encoder bit-width uniq-words))))

(defn world
  "Returns a channel of sensory input values."
  [text n-repeats]
  (doto (async/chan)
    (async/onto-chan (word-item-seq n-repeats text))))

(defn n-region-model
  ([n]
     (n-region-model input-text 3 n spec))
  ([text n spec]
     (let [inp (core/sensory-input (make-encoder text))]
       (core/regions-in-series core/sensory-region inp n spec))))
