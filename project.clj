(defproject org.nfrac/comportex "0.0.12-SNAPSHOT"
  :description "Functionally composable cortex, an implementation of Hierarchical Temporal Memory"
  :url "http://github.com/nupic-community/comportex/"
  :license {:name "GNU Affero General Public Licence"
            :url "http://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/data.int-map "0.1.0"]
                 [org.clojure/test.check "0.8.2"]
                 [clj-http "1.1.1"]
                 [cljs-http "0.1.30"]]

  :jvm-opts ^:replace ["-server" "-Xmx2500m"]

  :cljsbuild {:builds [{:jar true
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced}}]}

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.7.107"]
                                  [criterium "0.4.3"]]
                   :plugins [[lein-cljsbuild "1.1.0"]
                             [com.cemerick/clojurescript.test "0.3.3"]
                             ;[lein-marginalia "0.8.1-SNAPSHOT"]
                             ]}
             :repl {:source-paths ["dev" "src"]}})
