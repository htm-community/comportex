(defproject org.nfrac/comportex "0.0.15-SNAPSHOT"
  :description "Functionally composable cortex, an implementation of Hierarchical Temporal Memory"
  :url "http://github.com/htm-community/comportex/"
  :license {:name "GNU Affero General Public Licence"
            :url "http://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/data.int-map "0.2.1"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/clojurescript "1.9.36"]
                 [clj-http "2.0.0"]
                 [cljs-http "0.1.38"]]

  :jvm-opts ^:replace ["-server" "-Xmx2500m"]

  :plugins [[lein-cljsbuild "1.1.3"]]

  :clean-targets ["public/comportex.js" "public/out"]

  :cljsbuild {:builds
              {:main
               {:source-paths ["src"]
                :jar true
                :compiler {:output-dir "public/out"
                           :output-to "public/comportex.js"}}}}

  :profiles {:dev {:dependencies [[criterium "0.4.3"]]
                   ;:plugins [[lein-marginalia "0.9.0"]]
                   :cljsbuild {:builds
                               {:main
                                {:compiler
                                 {:optimizations :none
                                  :source-map true}}}}}
             ;; Use: "lein with-profile +prod cljsbuild once"
             :prod {:cljsbuild {:builds
                                {:main
                                 {:compiler
                                  {:optimizations :advanced
                                   :source-map "public/comportex.js.map"}}}}}
             :repl {:source-paths ["dev" "src"]}})
