# Change Log

## [Unreleased]
### Changed
- No more temporal pooling excitation. Instead, synapses from predicted
  cells excite their target cells over multiple time steps.
- No more union pooling; activation level is constant.
- First level layers are not treated any differently from higher layers.
  - Proximal synapses grow to any active synapses, not just stable ones.
  - Winner cells remain the same in continuing active columns unless reset;
    we may rely on an external timing signal to distinguish repeats.
  - Learn on winner cells only when they become active (even at first level).
    But might revisit this to learn auto-associatively for pattern completion.
- Inhibition radius is now full span of receptive field size, not half of it.
- Do not displace synapses when growing; instead wait for them to be culled.
- Spec :spatial-pooling :local-inhibition replaces :global-inhibition? true.

### Added
- Spec parameter :stable-activation-steps
- State key :stable-cells-buffer
- Can create 2D regions with 3D inputs.
- Can specify custom spatial pooling function via spec key :spatial-pooling.
- Adjust overlap based on overlap duty cycle, to compare to NuPIC.
- Sampling Linear Encoder
- periodic? option to linear-encoder
- core/segs-proximal-bit-votes
- Spec parameter :inh-radius-scale
- Optional instantaneous lateral synapse graph, eg for learned lateral inhibition.
- Multimethods for customisation of algorithms, selected by spec keys
  :spatial-pooling and :temporal-pooling.
- Adjust overlap by activation frequency. disable by :float-overlap-duty-ratio 0

### Removed
- Spec parameters :temporal-pooling-max-exc, -fall, -amp
- Spec parameters :activation-level-max, :stable-inbit-frac-threshold
- State keys :engaged?, :newly-engaged?
- temporal-pooling-cells protocol function.

### Fixed
- coord-distance now uses Chebyshev distance to match neighbours function.
- Local inhibition failed when inhibition-base-distance >= inh-radius.
- Bursting defined by depolarisation; distinguishes bursting when depth=1.

## [0.0.13] - 2016-01-12
### Changed
- Winner cells are not selected in layer-activate phase, only later in
  the layer-learn phase.
- LayerActiveState no longer has keys :distal-learning, :proximal-learning, etc.
- LayerOfCells now has key :learn-state which is a LayerLearnState record.
- Abandoned the bias against cells with existing inactive segments.
- :matching-[ff-]seg-paths now holds [seg-path exc] tuples instead of just seg-path.
- Removed :well-matching-seg-paths and :well-matching-ff-seg-paths.
- In LayerDistalState renamed :on-bits to :active-bits
  and :on-lc-bits to :learnable-bits
- 2D and 3D topologies now index across rows first, like image pixels.

### Added
- LayerActiveState gains a key :col-active-cells, a map keyed by col id.
- Spec parameter :apical-bias-frac
- NoEncoder to pass in sparse distributed representations directly.

### Fixed
- Segments with connected synapses below :new-synapse-count but above
  :stimulus-threshold were not automatically chosen, instead falling
  through to partial matches (including disconnected synapses).
- The wrong segment index was chosen for a partial match when some
  cells had no segments. (cell-segs was filtered)

## [0.0.12] - 2015-12-01
- Before this I didn't keep a change log.

[Unreleased]: https://github.com/htm-community/comportex/compare/v0.0.13...HEAD
[0.0.13]: https://github.com/htm-community/comportex/compare/v0.0.12...v0.0.13
[0.0.12]: https://github.com/htm-community/comportex/compare/v0.0.10...v0.0.12
