# TODO


* standardise naming
  * (CLA) region becomes layer?

* unit tests
* property-based testing (clojure.test.check)
* repeatability - store random seeds in objs?

## pooling

* don't need to store pred cells grouped by column (pcbc)

* bias activation to columns with depolarised cells

* allow columns arrayed in 2 dimensions (PTopology)

## sequence memory

* move predicted-bit-votes into pooling namespace

* in choosing best-matching-segment-and-cell, break ties by permanence?

* only update synapses when cells turn on/off? (not when continuing?)

* limits
  * fixed-size CLA; and/or
  * global decay of segments       ;; avoid - long-term memory is good?
* avoid over-saturated predictive states (almost bursting)
* prediction confidence, based on duty cycle / permanences

* limit lateral synapses to within a radius 

* start small! (#cells / sequence length)


## perf:
* restructure synapses to :connected :near :zero (so don't decrement zeros)?
* update-ff-synapses group-by-maps