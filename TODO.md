# TODO

PTopology


## perf:
* p/overlaps  (column-overlap)
* p/column-update-in-synapses
* sm/predictive-cells
* p/update-overlaps
* p/active-columns (don't need full sort)

* unit tests
* property-based testing (clojure.test.check)
* repeatability - store random seeds in objs?

## pooling

* overlap-history just a counter? (since we adjust boosting and reset)

* try spatio-temporal pooling by extending (column/cell?) activation
> "The trick to adding TP to the SP is the following. When the input to
the SP was correctly predicted in the previous layer we want the cells
in the column to remain active long enough to learn to respond to the
next input pattern."

* allow columns arrayed in 2 dimensions

* local inhibition selecting peaks over a moving average

## sequence memory

* predict only one cell per column?
* can there be multiple learn-state cells in a column?
* in choosing best-matching-segment-and-cell, break ties by permanence?
* reinforce all active segments, or only the most active, in learn-cell?
  * differentiate segments within one cell (punish if multiple active)?
* should reinforce synapses to all active cells, or just learn cells?

* only update synapses when cells turn on/off? (not when continuing?)

* limits
  * fixed-size CLA; and/or
  * global decay of segments       ;; avoid - long-term memory is good?
* avoid over-saturated predictive states (almost bursting)
* prediction confidence, based on duty cycle / permanences
* backtracking if we chose the wrong sequence?
* reset context if failing predictions too long? (why isn't bursting enough?)

* "start cell" (cell 0)?
* limit lateral synapses to within a radius 


## code architecture

* protocols

* support heirarchy of regions / layers

* track column and/or segment activation history in a top-level
  counter map rather than nested?
