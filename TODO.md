# TODO


* support heirarchy of regions / layers

* standardise naming
  * (CLA) region becomes layer?

* unit tests
* property-based testing (clojure.test.check)
* repeatability - store random seeds in objs?

## pooling

* allow columns arrayed in 2 dimensions

* local inhibition selecting peaks over a moving average

## sequence memory

* in choosing best-matching-segment-and-cell, break ties by permanence?

* only update synapses when cells turn on/off? (not when continuing?)

* limits
  * fixed-size CLA; and/or
  * global decay of segments       ;; avoid - long-term memory is good?
* avoid over-saturated predictive states (almost bursting)
* prediction confidence, based on duty cycle / permanences

* limit lateral synapses to within a radius 


## perf:
* p/overlaps  (column-overlap)
* p/column-update-in-synapses
* sm/predictive-cells
* p/active-columns (don't need full sort)

