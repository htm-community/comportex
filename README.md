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


## Documentation

For an applied exploration of HTM using Comportex, try the essay
[Predicting power consumptions with HTM](http://mrcslws.com/gorilla/?path=hotgym.clj)
by Marcus Lewis.

The core API is in the namespace `core`, and it is typically used with the layer
implementation in `layer` and the encoder implementations in `encoders`.

Parameter descriptions can be found in [`org.nfrac.comportex.layer`](https://github.com/htm-community/comportex/blob/master/src/org/nfrac/comportex/layer.cljc).


## Minimum Viable Snippet

```
(require '[org.nfrac.comportex.core :as cx])
(require '[org.nfrac.comportex.layer :as layer])
(require '[org.nfrac.comportex.encoders :as enc])

(def sensor [:val (enc/unique-encoder [127] 21)])
(def params {})
(def htm
  (cx/network {:layer-a (layer/layer-of-cells params)}
              {:input sensor}))
(def htm2 (cx/htm-step htm {:val "hi"}))
(-> htm2 :layers :layer-a (cx/layer-state) :active-columns)
; #{186 641 898 830 490 805 776 819 758 627 630 485 515 618 872 143 220 392 133 38}
```


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


### Demos

A list of demos can be found in `src/org/nfrac/comportex/demos/` and, again, remember that they can also be run with Sanity.


### Javascript API

A minimal Javascript API is included, and a small demo which shows how to use it. First, compile to javascript:

```
lein cljsbuild once
```
Then load `public/comportexjs.html` in your browser.


## Related projects

* [Sanity](https://github.com/htm-community/sanity/)
* [NuPIC](https://github.com/numenta/nupic/)
* [nab-comportex](https://github.com/floybix/nab-comportex)


## Hello

* Get in touch via **HTM Forum** in the categories [HTM Theory](https://discourse.numenta.org/c/htm-theory) or [HTM Hackers](https://discourse.numenta.org/c/htm-hackers)
* or email or github issues.


## License

Copyright © 2014-2016 Felix Andrews and Marcus Lewis.

Distributed under the
[GNU Affero General Public Licence, Version 3](http://www.gnu.org/licenses/agpl-3.0.en.html),
the same as NuPIC.

[![DOI](https://zenodo.org/badge/19464/nupic-community/comportex.svg)](https://zenodo.org/badge/latestdoi/19464/nupic-community/comportex)
