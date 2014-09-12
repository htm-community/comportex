# TODO


* standardise naming
  * ff-synapses
  * (CLA) region becomes layer?

* unit tests
* property-based testing (clojure.test.check)
* repeatability - store random seeds in objs?

## pooling

* initialise ff synapses with only a small number connected;
  the other potential connections having a triangular distribution

* maintain an index from input bits to columns (by connected synapses).
  (need to track which synapses become newly dis/connected.)

* local inhibition selecting peaks over a moving average

* allow columns arrayed in 2 dimensions (PTopology)

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
* p/active-columns-with-local-inhibition
* p/active-columns (don't need full sort)
* sm/predictive-cells

