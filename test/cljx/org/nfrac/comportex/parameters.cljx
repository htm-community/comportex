(ns org.nfrac.comportex.parameters)

(def small
  {:ncol 300
   :input-size 200
   :potential-radius 100
   :activation-level 0.06
   :global-inhibition true
   :stimulus-threshold 2
   :sp-perm-inc 0.04
   :sp-perm-dec 0.01
   :sp-perm-connected 0.1
   :duty-cycle-period 1000
   :max-boost 2.0
   ;; sequence memory:
   :depth 5
   :new-synapse-count 10
   :activation-threshold 7
   :min-threshold 5
   :connected-perm 0.20
   :initial-perm 0.16
   :permanence-inc 0.04
   :permanence-dec 0.01
   })
