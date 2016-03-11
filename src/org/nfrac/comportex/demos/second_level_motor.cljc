(ns org.nfrac.comportex.demos.second-level-motor
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]
            #?(:clj [clojure.core.async :refer [put! >! <! go]]
               :cljs [cljs.core.async :refer [put! >! <!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(def bit-width 600)
(def n-on-bits 30)
(def motor-bit-width 10)
(def motor-n-on-bits 5)

(def test-text
  "one two three four.
the three little pigs.
6874230
1874235.
6342785
1342780.
09785341
29785346.
04358796
24358791.")

(defn parse-sentences
  [text*]
  (let [text (str/lower-case (str/trim text*))]
    (->> (str/split text #"[^\w]*\.+[^\w]*")
         (mapv #(str/split % #"[^\w']+"))
         (mapv #(mapv vec %)))))

(def spec
  {:column-dimensions [1000]
   :depth 8
   :proximal {:perm-stable-inc 0.15
              :perm-inc 0.04
              :perm-dec 0.01}
   :lateral-synapses? true
   :distal-vs-proximal-weight 0.0
   :use-feedback? true
   :apical {:learn? true}
   })

(def higher-level-spec
  (util/deep-merge
   spec
   {:column-dimensions [800]
    :stable-inbit-frac-threshold 0.5
    :ff-init-frac 0.05
    :proximal {:max-segments 5
               :new-synapse-count 12
               :learn-threshold 6}}))

(defn initial-inval
  [sentences]
  {:sentences sentences
   :position [0 0 0] ;; [sentence word letter]
   :value (get-in sentences [0 0 0])
   :action {:next-letter-saccade -1
            :next-word-saccade -1
            :next-sentence-saccade -1}
   })

(defn next-position
  [[i j k] action]
  (cond
    (pos? (:next-sentence-saccade action))
    [(inc i) 0 0]
    (neg? (:next-sentence-saccade action))
    [0 0 0]
    (pos? (:next-word-saccade action))
    [i (inc j) 0]
    (neg? (:next-word-saccade action))
    [i 0 0]
    (pos? (:next-letter-saccade action))
    [i j (inc k)]
    (neg? (:next-letter-saccade action))
    [i j 0]))

(defn apply-action
  [inval]
  (let [new-posn (next-position (:position inval) (:action inval))
        new-value (get-in (:sentences inval) new-posn)]
    (assoc inval
           :position new-posn
           :value new-value)))

(def letter-sensor
  [:value
   (enc/unique-encoder [bit-width] n-on-bits)])

(def letter-motor-sensor
  [[:action :next-letter-saccade]
   (enc/category-encoder [motor-bit-width] [1 -1])])

(def word-motor-sensor
  [[:action :next-word-saccade]
   (enc/category-encoder [motor-bit-width] [1 -1])])

(defn two-region-model
  ([]
   (two-region-model spec))
  ([spec]
   (core/region-network {:rgn-0 [:input :letter-motor]
                         :rgn-1 [:rgn-0 :word-motor]}
                        (constantly core/sensory-region)
                        {:rgn-0 spec
                         :rgn-1 higher-level-spec}
                        {:input letter-sensor}
                        {:letter-motor letter-motor-sensor
                         :word-motor word-motor-sensor}
                        )))

(defn htm-step-with-action-selection
  [world-c control-c]
  (comment
    ;; TODO: on next release of core.async, replace this go block with
    (let [inval (if-let [xf (poll! control-c)]
                  (xf inval)
                  inval)
          ]))
  (go
    (loop []
      (if-let [xf (<! control-c)]
        (let [inval (<! world-c)]
          (>! world-c (xf inval))
          (recur)))))
  (fn [htm inval]
    (let [;; do first part of step, but not depolarise yet (depends on action)
          htm-a (-> htm
                    (p/htm-sense inval :sensory)
                    (p/htm-activate)
                    (p/htm-learn))
          [i j k] (:position inval)
          ;; work out what the next action (saccade) should be
          sentences (:sentences inval)
          sentence (get sentences i)
          word (get sentence j)
          end-of-word? (= k (dec (count word)))
          end-of-sentence? (= j (dec (count sentence)))
          end-of-passage? (= i (dec (count sentences)))
          r0-lyr (get-in htm-a [:regions :rgn-0 :layer-3])
          r1-lyr (get-in htm-a [:regions :rgn-1 :layer-3])
          r0-stability (/ (count (:out-stable-ff-bits (:state r0-lyr)))
                          (count (:out-ff-bits (:state r0-lyr))))
          word-burst? (cond-> (:word-bursting? (:action inval))
                        ;; ignore burst on first letter of word
                        (pos? k) (or (< r0-stability 0.5)))
          sent-burst? (cond-> (:sentence-bursting? (:action inval))
                        ;; ignore burst on first letter of word
                        (pos? k) (or (< r0-stability 0.5)))
          action* (cond
                    ;; not yet at end of word
                    (not end-of-word?)
                    {:next-letter-saccade 1}

                    ;; end of word.

                    ;; word not yet learned, repeat word
                    word-burst?
                    {:next-letter-saccade -1
                     :word-bursting? false}

                    ;; go to next word (not yet at end of sentence)
                    ;; same letter-motor signal as when repeating a word
                    (not end-of-sentence?)
                    {:next-word-saccade 1
                     :next-letter-saccade -1
                     :word-bursting? false}

                    ;; end of sentence.

                    ;; sentence not yet learned, repeat sentence
                    sent-burst?
                    {:next-word-saccade -1
                     :next-letter-saccade -1
                     :word-bursting? false
                     :sentence-bursting? false}

                    ;; not yet at end of passage, go to next sentence
                    (not end-of-passage?)
                    {:next-sentence-saccade 1
                     :next-word-saccade 1
                     :next-letter-saccade -1
                     :word-bursting? false
                     :sentence-bursting? false}

                    ;; reached end of passage
                    :else
                    {:next-word-saccade -1
                     :next-letter-saccade -1
                     :word-bursting? false
                     :sentence-bursting? false}
                    )
          ;; next-letter-saccade represents starting a word (-1) or continuing (1)
          ;; that is all that rgn-0 knows.
          action (merge {:next-word-saccade 0
                         :next-sentence-saccade 0
                         :word-bursting? word-burst?
                         :sentence-bursting? sent-burst?}
                        action*)
          inval-with-action (assoc inval :action action
                                   :prev-action (:action inval))]
      ;; calculate the next position
      (let [new-inval (apply-action inval-with-action)]
        (put! world-c new-inval))
      ;; depolarise (predict) based on action, and update :input-value
      (cond-> htm-a
        true
        (p/htm-sense inval-with-action :motor)
        true
        (p/htm-depolarise)
        ;; break sequence when repeating word (but keep tp synapses)
        end-of-word?
        (update-in [:regions :rgn-0] p/break :tm)
        ;; reset context when going on to new word
        (and end-of-word? (not word-burst?))
        (update-in [:regions :rgn-0] p/break :syns)
        (and end-of-word? (not word-burst?))
        (update-in [:regions :rgn-1] p/break :winners)
        ))))

(comment
  (require '[clojure.core.async :refer [chan put! <!!]])
  (def world-c (chan))
  (def control-c (chan))
  (def step (htm-step-with-action-selection world-c control-c))
  (def in (initial-inval (parse-sentences test-text)))
  (def mo (two-region-model))
  (def mo2 (step mo in))
  (def in2 (<!! world-c))
  )
