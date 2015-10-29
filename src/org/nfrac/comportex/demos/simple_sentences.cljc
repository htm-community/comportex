(ns org.nfrac.comportex.demos.simple-sentences
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]))

(def bit-width 500)
(def n-on-bits 25)

(def spec
  {:column-dimensions [1000]
   :depth 8
   :distal {:perm-init 0.21}
   :distal-vs-proximal-weight 0.2
   })

(def higher-level-spec
  (util/deep-merge
   spec
   {:column-dimensions [800]
    :proximal {:max-segments 5}}))

(def input-text
  "Jane has eyes.
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
  [text*]
  (let [text (str/lower-case (str/trim text*))]
    (->> (str/split text #"[^\w]*\.+[^\w]*")
         (mapv #(str/split % #"[^\w']+"))
         (mapv #(vec (concat % ["."]))))))

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

(def random-sensor
  [:word
   (enc/unique-encoder [bit-width] n-on-bits)])

(defn n-region-model
  ([n]
   (n-region-model n spec))
  ([n spec]
   (core/regions-in-series n core/sensory-region
                           (list* spec (repeat higher-level-spec))
                           {:input random-sensor})))
