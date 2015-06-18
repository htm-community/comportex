# TODO

* separate parameters for permanence inc / dec from stable inputs (temporal pooling)


* can we speed up init?

* LTD

* higher level sequences demo

* update marginalia / move to codox

* most functions should take parameter value arguments, not whole spec

* serialisable encoders - handle general pre-transform?
* encoders should not be part of a model?

* keep track of existing segment ids per cell?

* continuously grow new proximal synapses
* cull zeros on proximal

* region-network - allow different build-region fns for each region


* repl truncate printing of :state :overlaps / :active-cells / :active-cells-by-col / :proximal-exc / :out-ff-bits
* :distal-state :distal-bits / :pred-cells


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
