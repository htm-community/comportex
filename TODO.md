# TODO

* bulk-learn records

* separate calculation vs update (building transaction vs commit) in punish-distal

* revise default parameter values

* most functions should take parameter value arguments, not whole spec

* perf!! excitations, reinforce-in-synapses

* serialisable encoders - handle general pre-transform?
* encoders should not be part of a model?

* serialisable synapse graphs - avoid closures tgt->i, i->tgt
* * go back to maps not vectors: {[col ci] p}, {[col ci si] p}
* * * also allows many more distal dendrite segments (avoid cols*depth*max-segs vector)
* * keep track of existing segment ids per cell

* continuously grow new proximal synapses
* get rid of overlap-duty-cycles

* region-network - allow different build-region fns for each region


* repl truncate printing of :state :overlaps / :active-cells / :active-cells-by-col / :proximal-exc / :out-ff-bits
* :distal-state :distal-bits / :pred-cells


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


* only update synapses when cells turn on/off? (not when continuing?)

* limit lateral synapses to within a radius

* start small! (#cells / sequence length)

* unit tests
* property-based testing (clojure.test.check)
* repeatability - store random seeds in objs?

* type / structure validation with pre conditions - Herbert?

* fully test properties of coordinate encoder

* clojars release

* java API?

## perf

* YourKit
* data.int-map
* transducers
