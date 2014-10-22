# Comportex

Functionally composable cortex, an implementation of Hierarchical
Temporal Memory as a Clojure library.

Comportex is not yet stable. My aims are to:

1. understand the theory deeply by implementing it and playing with it;
2. help others to understand the theory via interactive visual demonstrations; and
3. further develop the theory by attempting to apply it to new problem types.


## Documentation

See the [annotated source code](http://nupic-community.github.io/comportex/).


## Usage

Get [Leiningen](http://leiningen.org/) first.

Use git to clone this repository. Then, to start an interactive session:

```
lein repl
```

For examples look in `src/cljx/org/nfrac/comportex/demos/`.

The main API is in the namespace `org.nfrac.comportex.core` and in
`org.nfrac.comportex.protocols`.


## Related projects

* [ComportexViz](http://github.com/nupic-community/comportexviz/)
* [Clortex](https://github.com/nupic-community/clortex/)
* [NuPIC](http://www.numenta.org/)


## License

Copyright © 2014 Felix Andrews

Distributed under your choice of
* the Eclipse Public License, the same as Clojure.
* the GNU Public Licence, Version 3 http://www.gnu.org/licenses/gpl.html, the same as NuPIC.
