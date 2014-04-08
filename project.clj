(defproject org.nfrac/comportex "0.1.0-SNAPSHOT"
  :description "Functionally composable cortex, an implementation of Hierarchical Temporal Memory"
  :url "http://github.com/floybix/comportex/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.cemerick/pprng "0.0.2"]]
  :jar-exclusions [#"\.cljx"]
  :jvm-opts ["-server" "-Xmx2g"])
