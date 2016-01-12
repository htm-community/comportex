(defproject org.nfrac/comportex "0.0.14-SNAPSHOT"
  :description "Functionally composable cortex, an implementation of Hierarchical Temporal Memory"
  :url "http://github.com/nupic-community/comportex/"
  :license {:name "GNU Affero General Public Licence"
            :url "http://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0-RC2"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/data.int-map "0.2.1"]
                 [org.clojure/test.check "0.9.0"]
                 [clj-http "2.0.0"]
                 [cljs-http "0.1.38"]]

  :jvm-opts ^:replace ["-server" "-Xmx2500m"]

  :cljsbuild {:builds [{:jar true
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced}}]}

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.7.189"]
                                  [criterium "0.4.3"]]
                   :plugins [[lein-cljsbuild "1.1.0"]
                             [com.cemerick/clojurescript.test "0.3.3"]
                             ;[lein-marginalia "0.8.1-SNAPSHOT"]
                             ]}
             :repl {:source-paths ["dev" "src"]}})
