# TODO

* extend depolarisation over multiple time steps?

* allow many more distal dendrite segments (avoid cols*depth*max-segs vector)

* ?speed up distal synapses?
  rather than tranforming between [col ci si] and uidx:
  (deftype Cell [rgn-uuid ^long col ^long ci ^long depth]
   PCell
   (cell-column [this])
   (index-in-column [this])
   (index-in-layer [this])
   (cell-region-uuid [this]))
  (deftype Segment [^Cell cell ^long si ^long max-segs]
   PSegment
   (index-in-cell [this])
   (segment-index-in-layer [this])
   PCell
   ...)

* separate parameter for punishment permanence decrement

* motor/top-down distal inputs to distal synapses
  * lateral synapses as an option

* higher regions need a larger potential pool because the source
  activation is so sparse.
  * assuming activation is uniform:
    * potential-span = n-local-active / (sparsity * potential-frac)
    * so for n-local-active = 3, sparsity = 0.2%, potential-span = 3000
  * it seems wrong to maintain such a big potential pool set
    * use an implicit potential pool, same as lateral activation
      * extending / shrinking the segment
    * avoids the need to specify potential-radius

* higher-levels regions only grow ff-synapses to learn-cells from below?
  (otherwise typically grow to all cells of a bursting column)

* turn off temporal pooling when no input

* probably only one predicted cell per column will fire and inhibit others?
* only one learn cell per column

* in choosing best-matching-segment-and-cell, break ties by permanence?

* only update synapses when cells turn on/off? (not when continuing?)

* limit lateral synapses to within a radius

* start small! (#cells / sequence length)

* unit tests
* property-based testing (clojure.test.check)
* repeatability - store random seeds in objs?

* boosting - apply every time step; periodically recalculate duty cycle thresholds

* clojars release

* java API?

## perf

* data.int-map
* reducers
* transducers
