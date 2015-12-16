# Change Log

## [Unreleased]
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

### Added
- LayerActiveState gains a key :col-active-cells, a map keyed by col id.
- Spec parameter :apical-bias-frac

### Fixed
- Segments with connected synapses below :new-synapse-count but above
  :stimulus-threshold were not automatically chosen, instead falling
  through to partial matches (including disconnected synapses).
- The wrong segment index was chosen for a partial match when some
  cells had no segments. (cell-segs was filtered)

## [0.0.12] - 2015-12-01
- Before this I didn't keep a change log.

[Unreleased]: https://github.com/nupic-community/comportex/compare/v0.0.12...HEAD
[0.0.12]: https://github.com/nupic-community/comportex/compare/v0.0.10...v0.0.12
