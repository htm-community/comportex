(ns org.nfrac.comportex.demos.second-level-motor
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]
            #?(:clj [clojure.core.async :refer [<! >! go]]
               :cljs [cljs.core.async :refer [<! >!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(def bit-width 600)
(def on-bits 30)
(def motor-bit-width 30)
(def motor-on-bits 15)

(def test-text
  "one two three four.
the three little pigs.")

(defn parse-sentences
  [text]
  (->> (str/split (str/trim text) #"[^\w]*\.+[^\w]*")
       (mapv #(str/split % #"[^\w']+"))
       (mapv #(mapv vec %))))

(def higher-level-spec-diff
  {:column-dimensions [800]
   :ff-max-segments 5
   :ff-seg-new-synapse-count 12
   :ff-seg-learn-threshold 6
   })

(def spec
  {:column-dimensions [800]
   :depth 5
   :ff-perm-stable-inc 0.15
   :ff-perm-inc 0.04
   :ff-perm-dec 0.01
   :temporal-pooling-amp 3.0
   :boost-active-duty-ratio 0 ;; disable boosting
   :lateral-synapses? true
   :distal-vs-proximal-weight 0.5
   :use-feedback? false
   })

(def initial-input-val
  {:sentences (parse-sentences test-text)
   :position [0 0 0] ;; [sentence word letter]
   :next-letter-saccade -1
   :next-word-saccade -1
   :next-sentence-saccade -1
   })

(defn next-position
  [inval]
  (let [[i j k] (:position inval)]
    (cond
      (pos? (:next-sentence-saccade inval))
      [(inc i) 0 0]
      (neg? (:next-sentence-saccade inval))
      [0 0 0]
      (pos? (:next-word-saccade inval))
      [i (inc j) 0]
      (neg? (:next-word-saccade inval))
      [i 0 0]
      (pos? (:next-letter-saccade inval))
      [i j (inc k)]
      (neg? (:next-letter-saccade inval))
      [i j 0])))

(def sensory-input
  (let [e (enc/pre-transform #(get-in (:sentences %) (:position %))
                             (enc/unique-encoder [bit-width] on-bits))]
    (core/sensory-input e)))

(def letter-motor-input
  (let [e (enc/pre-transform :next-letter-saccade
                             (enc/category-encoder motor-bit-width [1 -1]))]
    (core/sensorimotor-input nil e)))

(def word-motor-input
  (let [e (enc/pre-transform :next-word-saccade
                             (enc/category-encoder motor-bit-width [1 -1]))]
    (core/sensorimotor-input nil e)))

(defn two-region-model
  ([]
   (two-region-model spec))
  ([spec]
   (core/region-network {:rgn-0 [:input :letter-motor]
                         :rgn-1 [:rgn-0 :word-motor]}
                        {:input sensory-input
                         :letter-motor letter-motor-input
                         :word-motor word-motor-input}
                        core/sensory-region
                        {:rgn-0 spec
                         :rgn-1 (merge spec higher-level-spec-diff)})))

(defn feed-world-c-with-actions!
  [in-model-steps-c in-control-c out-world-c model-atom]
  (go
    (loop [inval (assoc initial-input-val
                        :word-bursting? false
                        :sentence-bursting? false)]
      (let [[x port] (alts! [in-control-c
                             [out-world-c inval]]
                            :priority true)]
        (when x
          (if (= port in-control-c)
            ;; control channel, assume x is a function to transform input value
            (recur (x inval))
            (when-let [htm (<! in-model-steps-c)]
              (let [[i j k] (:position inval)
                    new-posn (next-position inval)
                    [ni nj nk] new-posn
                    ;; work out what the next action (saccade) should be
                    ;; TODO: this should be after htm-activate and before htm-depolarise
                    sentences (:sentences inval)
                    sentence (get sentences ni)
                    word (get sentence nj)
                    end-of-word? (= nk (dec (count word)))
                    end-of-sentence? (= nj (dec (count sentence)))
                    end-of-passage? (= ni (dec (count sentences)))
                    r0-lyr (get-in htm [:regions :rgn-0 :layer-3])
                    r1-lyr (get-in htm [:regions :rgn-1 :layer-3])
                    r0-burst-frac (/ (count (p/bursting-columns r0-lyr))
                                     (count (p/active-columns r0-lyr)))
                    r1-burst-frac (/ (count (p/bursting-columns r1-lyr))
                                     (count (p/active-columns r1-lyr)))
                    word-burst? (or (:word-bursting? inval)
                                    (>= r0-burst-frac 0.50))
                    sent-burst? (or (:sentence-bursting? inval)
                                    (>= r1-burst-frac 0.50))
                    new-in-static {:sentences sentences
                                   :position new-posn
                                   :next-letter-saccade 0
                                   :next-word-saccade 0
                                   :next-sentence-saccade 0
                                   :word-bursting? word-burst?
                                   :sentence-bursting? sent-burst?}
                    action (cond
                             ;; not yet at end of word
                             (not end-of-word?)
                             {:next-letter-saccade 1}

                             ;; end of word.
                             ;; word not yet learned, repeat word
                             word-burst?
                             {:next-letter-saccade -1
                              :word-bursting? false}

                             ;; not yet at end of sentence, go to next word
                             (not end-of-sentence?)
                             {:next-word-saccade 1
                              :word-bursting? false}

                             ;; end of sentence.
                             ;; sentence not yet learned, repeat sentence
                             sent-burst?
                             {:next-word-saccade -1
                              :word-bursting? false
                              :sentence-bursting? false}

                             ;; not yet at end of passage, go to next sentence
                             (not end-of-passage?)
                             {:next-sentence-saccade 1
                              :next-word-saccade 1
                              :word-bursting? false
                              :sentence-bursting? false}

                             ;; reached end of passage
                             :else
                             {:next-sentence-saccade -1
                              :next-word-saccade -1
                              :word-bursting? false
                              :sentence-bursting? false}
                             )]
                ;; reset first region when going on to new word
                (when (and end-of-word? (not word-burst?))
                  (swap! model-atom update-in [:regions :rgn-0] p/break))
                ;; reset second region when going on to new sentence
                (when (and end-of-sentence? (not sent-burst?))
                  (swap! model-atom update-in [:regions :rgn-1] p/break))
                ;; the next input value:
                (recur (merge new-in-static action))))))))))
