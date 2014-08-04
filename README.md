# Comportex

Functionally composable cortex, an implementation of Hierarchical
Temporal Memory as a Clojure library.

This is highly experimental. Not even remotely stable. My aims are to:

1. understand the theory deeply by implementing it and playing with it;
2. help others to understand the theory via interactive visual demonstrations; and
3. further develop the theory by attempting to apply it to new problem types.


## Documentation

See the [annotated source code](http://floybix.github.io/comportex/)
for an overview of the data structures, as well as the algorithm in
namespaces `pooling` and `sequence-memory`.


## Usage

Get [Leiningen](http://leiningen.org/) first.

Use git to clone this repository. Then:

```
lein test
```

The main API is in the namespace `org.nfrac.comportex.core`.

See also [ComportexViz](http://github.com/floybix/comportexviz/).


## Related projects

* [Clortex](https://github.com/fergalbyrne/clortex/)
* [NuPIC](http://www.numenta.org/)


## License

Copyright © 2014 Felix Andrews

Distributed under your choice of
* the Eclipse Public License, the same as Clojure.
* the GNU Public Licence, Version 3 http://www.gnu.org/licenses/gpl.html, the same as NuPIC.
