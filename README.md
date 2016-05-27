# Comportex

<img src="https://raw.githubusercontent.com/htm-community/comportex/master/comportex-logo.png"
 alt="Comportex logo" align="right" />

Comportex is an implementation of
[Hierarchical Temporal Memory](http://numenta.org/#theory) as a
Clojure library. It is not a port of
[NuPIC](https://github.com/numenta/nupic/), it is a separate
implementation based initially on the Numenta CLA white paper but
significantly evolved.

In the spirit of Clojure, Comportex is more a library than a
framework. The user controls simulations, and decides what to do with
them. If you want to take the set of active cells and use them to
generate predictions or anomaly scores, that is up to you. It's not
going to do it automatically.

Comportex is not yet stable. Our aims are to:

1. understand the theory deeply by implementing it and experimenting with it;
2. help others to understand the theory via visual explanations; and
3. further develop the theory by attempting to apply it to new problem types.


For an applied exploration of HTM using Comportex, try the essay
[Predicting power consumptions with HTM](http://mrcslws.com/gorilla/?path=hotgym.clj)
by Marcus Lewis.


## Documentation

For now there is the
[annotated source code](http://htm-community.github.com/comportex/docs/).
The main API is in the namespaces `core`, `protocols` and `encoders`, while the
algorithms are mainly in `cells`, `synapses` and `inhibition`.

Parameter descriptions can be found on [`org.nfrac.comportex.cells/parameter-defaults`](https://github.com/htm-community/comportex/blob/master/src/org/nfrac/comportex/cells.cljc#L31).

[This blog post](http://floybix.github.io/2014/11/05/htm-protocols/)
has some explanation of the protocols.


## Usage

Get [Leiningen](http://leiningen.org/) first.

Use git to clone this repository. Then, e.g. to start an interactive session:

```
cd comportex
lein repl
```

See [A sample workflow with the
REPL](https://github.com/htm-community/comportex/wiki/A-sample-workflow-with-the-REPL).

Or, for a better experience, check out the Notebook (browser-based
REPL with super powers) in
[Sanity](https://github.com/htm-community/sanity/).


## Related projects

* [Sanity](https://github.com/htm-community/sanity/)
* [NuPIC](https://github.com/numenta/nupic/)


## Hello

* Get in touch via **HTM Forum** in the categories [HTM Theory](https://discourse.numenta.org/c/htm-theory) or [Comportex](https://discourse.numenta.org/c/htm-hackers/comportex)
* or email or github issues.


## License

Copyright © 2014-2016 Felix Andrews and Marcus Lewis.

Distributed under the
[GNU Affero General Public Licence, Version 3](http://www.gnu.org/licenses/agpl-3.0.en.html),
the same as NuPIC.

[![DOI](https://zenodo.org/badge/19464/nupic-community/comportex.svg)](https://zenodo.org/badge/latestdoi/19464/nupic-community/comportex)
