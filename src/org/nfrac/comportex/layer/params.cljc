(ns org.nfrac.comportex.layer.params
  "Parameter definitions for a HTM-style layer."
  (:require [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.util :as util :refer [spec-finite]]
            [clojure.spec :as s]
            [#?(:clj clojure.spec.gen :cljs clojure.spec.impl.gen) :as gen]))

(s/def ::max-segments
  #_"maximum number of dendrites segments per cell (or column for proximal)."
  (-> (s/int-in 1 1e6)
      (s/with-gen #(s/gen (s/int-in 1 10)))))

(s/def ::max-synapse-count
  #_"maximum number of synapses per segment."
  (s/int-in 1 1e6))

(s/def ::new-synapse-count
  #_"number of synapses created on a new dendrite segment."
  (s/int-in 1 1e6))

(s/def ::stimulus-threshold
  #_"minimum number of active synapses on a segment for it to become active."
  (-> (s/int-in 0 1000)
      (s/with-gen #(s/gen (s/int-in 0 4)))))

(s/def ::learn-threshold
  #_"minimum number of active synapses on a segment for it to be reinforced and
  extended if it is the best matching."
  (-> (s/int-in 1 10000)
      (s/with-gen #(s/gen (s/int-in 1 40)))))

(s/def ::perm-inc
  #_"amount by which to increase synapse permanence to active sources when
  reinforcing a segment."
  ::syn/permanence)

(s/def ::perm-dec
  #_"amount by which to decrease synapse permanence to inactive sources when
  reinforcing a segment."
  ::syn/permanence)

(s/def ::perm-connected
  #_"permanence value at which a synapse is functionally connected."
  (s/and ::syn/permanence pos?))

(s/def ::perm-init
  #_"initial permanence value for new synapses on segments."
  ::syn/permanence)

(s/def ::perm-stable-inc
  #_"amount by which to increase synapse permanence to stable (predicted)
  sources when reinforcing a segment."
  ::syn/permanence)

(s/def ::perm-punish
  #_"amount by which to decrease synapse permanence when punishing segments in
  case of failed prediction."
  ::syn/permanence)

(s/def ::punish?
  #_"whether to reduce synapse permanence on segments incorrectly predicting
  their own activation."
  boolean?)

(s/def ::learn?
  #_"whether to apply learning rules to synapses. If false, they are static."
  boolean?)

(s/def ::synapse-graph-params
  #_"A parameter set for one synapse graph, that is typically the proximal
  dendrites, distal dendrites, or apical dendrites in one layer."
  (s/keys :req-un [::max-segments
                   ::max-synapse-count
                   ::new-synapse-count
                   ::stimulus-threshold
                   ::learn-threshold
                   ::perm-inc
                   ::perm-dec
                   ::perm-punish
                   ::perm-connected
                   ::perm-init
                   ::punish?
                   ::learn?]))

(def dendrite-parameter-defaults
  "Default parameters for distal dendrite segments. The
  same parameters are also used for proximal segments, but with
  different default values."
  {:max-segments 5
   :max-synapse-count 22
   :new-synapse-count 12
   :stimulus-threshold 9
   :learn-threshold 7
   :perm-inc 0.05
   :perm-dec 0.01
   :perm-punish 0.002
   :perm-connected 0.20
   :perm-init 0.16
   :perm-stable-inc 0.05
   :punish? true
   :learn? true})

(s/def ::column-dimensions
  #_"size of column field as a vector, one dimensional `[size]`,
  or two dimensional `[width height]`"
  (s/coll-of (s/int-in 1 1e7) :kind vector? :min-count 1 :max-count 2
             :gen (fn []
                    (s/gen (s/and (s/coll-of (s/int-in 1 2048) :kind vector?
                                             :min-count 1 :max-count 2)
                                  #(<= (reduce * %) 2048))))))

(s/def ::depth
  #_"number of cells per column. Value 1 gives first-order sequence memory."
  (-> (s/int-in 1 1e5)
      (s/with-gen #(s/gen (s/int-in 1 8)))))

(s/def ::ff-potential-radius
  #_"range of potential feed-forward synapse connections, as a fraction of the
  longest single dimension in the input space."
  (spec-finite :min (/ 1 10000) :max 1.0))

(s/def ::ff-init-frac
  #_"fraction of inputs within radius of a column that will be initialised with
  proximal synapses."
  (spec-finite :min 0.0 :max 1.0))

(s/def ::ff-perm-init
  #_"range of initial permanence values on proximal synapses."
  (s/and (s/tuple ::syn/permanence ::syn/permanence)
         (fn [[lo hi]] (<= lo hi))))

(s/def ::grow?
  #_"whether to grow new synapses on segments, like on the usual distal segments."
  boolean?)

(s/def ::proximal
  #_"parameters for proximal dendrite segments."
  (s/merge ::synapse-graph-params
           (s/keys :req-un [::perm-stable-inc
                            ::grow?])))

(s/def ::distal
  #_"parameters for distal dendrite segments."
  ::synapse-graph-params)

(s/def ::apical
  #_"parameters for apical dendrite segments."
  ::synapse-graph-params)

;; homeostasis-params

(s/def ::max-boost
  #_"ceiling on the column boosting factor used to increase activation frequency."
  (s/and ::syn/excitation-amt #(>= % 1)))

(s/def ::duty-cycle-period
  #_"number of time steps to average over when updating duty cycles and (thereby)
  column boosting measures."
  (s/and number? #(>= % 1)))

(s/def ::boost-active-duty-ratio
  #_"when a column's activation frequency is below this proportion of the
  highest of its neighbours, its boost factor is increased. 0 = disabled."
  (spec-finite :min 0.0 :max 1.0))

(s/def ::adjust-overlap-duty-ratio
  #_"when a column's overlap frequency differs from any of its neighbours by at
  least this fraction, its permanences are adjusted. 0 = disabled."
  (spec-finite :min 0.0 :max 1.0))

(s/def ::float-overlap-duty-ratio
  #_"when a column's activation frequency is below this fraction of the target
  activation rate, its permanences are adjusted up by pcon/10. 0 = disabled."
  (spec-finite :min 0.0 :max 1.0))

(s/def ::float-overlap-duty-ratio-hi
  #_"when a column's activation frequency is above this multiple of the target
  activation rate, its permanences are adjusted down by pcon/10."
  (spec-finite :min 1.0 :max 1e6))

(s/def ::boost-active-every
  #_"number of time steps between recalculating column boosting factors."
  pos-int?)

(s/def ::adjust-overlap-every
  #_"number of time steps between adjusting column permanences to stabilise
  overlap frequencies."
  pos-int?)

(s/def ::float-overlap-every
  #_"number of time steps between adjusting column permanences to stabilise
  activation frequencies."
  pos-int?)

(s/def ::homeostasis-params
  #_"The subset of parameters used in homeostasis algorithms."
  (s/keys :req-un [::max-boost
                   ::duty-cycle-period
                   ::boost-active-duty-ratio
                   ::adjust-overlap-duty-ratio
                   ::float-overlap-duty-ratio
                   ::float-overlap-duty-ratio-hi
                   ::boost-active-every
                   ::adjust-overlap-every
                   ::float-overlap-every]))

(s/def ::inh-radius-every
  #_"number of time steps between recalculating the effective inhibition radius."
  pos-int?)

(s/def ::lateral-synapses?
  #_"whether distal synapses can connect laterally to other cells in the layer."
  boolean?)

(s/def ::activation-level
  #_"fraction of columns that can be active; inhibition kicks in to reduce it to
  this level."
  (spec-finite :min (/ 1 10000) :max 1.0))

(s/def ::inhibition-base-distance
  #_"the distance in columns within which a cell will always inhibit neighbouring
  cells with lower excitation. Used by `:spatial-pooling :local-inhibition`."
  (s/int-in 0 1e5))

(s/def ::distal-vs-proximal-weight
  #_"scaling to apply to the number of active distal synapses (on the winning
  segment) before adding to the number of active proximal synapses, when
  selecting active columns. Set to zero to disable ``prediction-assisted''
  activation."
  (spec-finite :min 0.0 :max 1e6))

(s/def ::apical-bias-frac
  #_"probability of choosing a winner cell according to apical excitation when
  otherwise the choice would have been random. Generates similarity between
  cases in similar contexts."
  (spec-finite :min 0.0 :max 1.0))

(s/def ::spontaneous-activation?
  #_"if true, cells may become active with sufficient distal synapse excitation,
  even in the absence of any proximal synapse excitation."
  boolean?)

(s/def ::dominance-margin
  #_"an amount of excitation (generally measured in number of active synapses) by
  which one cell must exceed all others in the column to be considered dominant.
  And therefore to inhibit all other cells in the column."
  ::syn/excitation-amt)

(s/def ::stable-activation-steps
  #_"number of time steps that synapses remain active from cells whose activation
  was predicted and thus generated minibursts to metabotropic receptors. They
  might be curtailed earlier by a manual break."
  (s/int-in 1 1e6))

(s/def ::transition-similarity
  #_"effective time steps are delayed until the similarity (normalised column
  overlap) between successive states falls below this level. So 1.0 means every
  time step is effective - the usual behaviour."
  (spec-finite :min 0.0 :max 1.0))

(s/def ::random-seed
  #_"the random seed (for reproducible results)."
  int?)

(s/def ::spatial-pooling
  #_"keyword to look up a spatial pooling implementation of the multimethod
  `org.nfrac.comportex.cells/spatial-pooling`. An alternative is
  `:local-inhibition`, implemented in this namespace."
  (-> keyword?
      (s/with-gen #(s/gen #{:standard :local-inhibition}))))

(s/def ::temporal-pooling
  #_"keyword to look up a temporal pooling implementation of the multimethod
  #`org.nfrac.comportex.cells/temporal-pooling`."
  (-> keyword?
      (s/with-gen #(s/gen #{:standard}))))

(s/def ::params
  #_"A standard parameter set for a layer."
  (-> (s/keys :req-un [::column-dimensions
                       ::depth
                       ::ff-potential-radius
                       ::ff-init-frac
                       ::ff-perm-init
                       ::proximal
                       ::distal
                       ::apical
                       ::inh-radius-every
                       ::lateral-synapses?
                       ::activation-level
                       ::inhibition-base-distance
                       ::distal-vs-proximal-weight
                       ::apical-bias-frac
                       ::spontaneous-activation?
                       ::dominance-margin
                       ::stable-activation-steps
                       ::transition-similarity
                       ::random-seed
                       ::spatial-pooling
                       ::temporal-pooling])
      (s/merge ::homeostasis-params)))

(def parameter-defaults
  "Default parameter set for a layer."
  {:column-dimensions [1000]
   :depth 5
   :ff-potential-radius 1.0
   :ff-init-frac 0.25
   :ff-perm-init [0.10 0.25]
   :proximal {:max-segments 1
              :max-synapse-count 300
              :new-synapse-count 12
              :stimulus-threshold 2
              :learn-threshold 7
              :perm-inc 0.04
              :perm-stable-inc 0.15
              :perm-dec 0.01
              :perm-punish 0.002
              :perm-connected 0.20
              :perm-init 0.25
              :learn? true
              :punish? false
              :grow? false}
   :distal (assoc dendrite-parameter-defaults
                  :learn? true)
   :apical (assoc dendrite-parameter-defaults
                  :learn? false)
   :ilateral {:max-segments 1
              :max-synapse-count 22
              :new-synapse-count 12
              :stimulus-threshold 1
              :perm-connected 0.50
              :perm-init 0.08
              :perm-inc 0.08
              :perm-dec 0.01
              :learn? false}
   :max-boost 1.5
   :duty-cycle-period 1000
   :boost-active-duty-ratio 0
   :adjust-overlap-duty-ratio 0
   :float-overlap-duty-ratio 0
   :float-overlap-duty-ratio-hi 20.0
   :boost-active-every 100
   :adjust-overlap-every 300
   :float-overlap-every 100
   :inh-radius-every 1000
   :inh-radius-scale 1.0
   :lateral-synapses? true
   :activation-level 0.02
   :inhibition-base-distance 1
   :distal-vs-proximal-weight 0.0
   :apical-bias-frac 0.0
   :spontaneous-activation? false
   :dominance-margin 4
   :stable-activation-steps 5
   :transition-similarity 1.0
   :random-seed 42
   ;; algorithm implementations
   :spatial-pooling :standard
   :temporal-pooling :standard})


;; TODO decide on defaults (reliability vs speed), provide alternatives?
(def better-parameter-defaults
  (assoc parameter-defaults
         :column-dimensions [2048]
         :depth 16
         :distal (assoc dendrite-parameter-defaults
                        :max-segments 8
                        :max-synapse-count 32
                        :new-synapse-count 20
                        :stimulus-threshold 13
                        :learn-threshold 10)))
