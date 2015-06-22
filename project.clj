(defproject org.nfrac/comportex "0.0.9-SNAPSHOT"
  :description "Functionally composable cortex, an implementation of Hierarchical Temporal Memory"
  :url "http://github.com/nupic-community/comportex/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/data.int-map "0.1.0"]
                 [com.cemerick/pprng "0.0.3"]
                 [clj-http "1.1.1"]
                 [cljs-http "0.1.30"]]

  :jvm-opts ^:replace ["-server" "-Xmx2500m"]

  :cljsbuild {:builds [{:jar true
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced}}]}

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-3211"]
                                  [criterium "0.4.3"]]
                   :plugins [[lein-cljsbuild "1.0.5"]
                             [com.cemerick/clojurescript.test "0.3.3"]
                             [com.cemerick/austin "0.1.6"]
                             [lein-marginalia "0.8.0"]]}
             :repl {:source-paths ["dev" "src"]}})
