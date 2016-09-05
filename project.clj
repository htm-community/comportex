(defproject org.nfrac/comportex "0.0.15-SNAPSHOT"
  :description "Hierarchical Temporal Memory in Clojure"
  :url "http://github.com/htm-community/comportex/"
  :license {:name "GNU Affero General Public Licence"
            :url "http://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha10"]
                 [org.clojure/core.async "0.2.385"]
                 [org.clojure/data.int-map "0.2.3"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/clojurescript "1.9.227"]
                 [clj-http "3.1.0"]
                 [cljs-http "0.1.41"]]

  :jvm-opts ^:replace ["-server" "-Xmx2500m"
                       "-Dclojure.spec.check-asserts=true"]

  :plugins [[lein-cljsbuild "1.1.3"]]

  :codox {:namespaces [#"comportex\.(?!demos)"]
          :ns-aliases {org.nfrac.comportex.protocols p
                       org.nfrac.comportex.encoders enc
                       org.nfrac.comportex.cells cells}
          :source-uri "https://github.com/htm-community/comportex/blob/{version}/{filepath}#L{line}"}

  :clean-targets ["public/comportex.js" "public/out"]

  :cljsbuild {:builds
              {:main
               {:source-paths ["src"]
                :jar true
                :compiler {:output-dir "public/out"
                           :output-to "public/comportex.js"}}}}

  :profiles {:dev {:dependencies [[com.gfredericks/test.chuck "0.2.7"]
                                  [criterium "0.4.4"]]
                   :plugins [[lein-codox "0.9.6"]]
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
             :repl {:source-paths ["dev" "src"]}}

 :monkeypatch-clojure-test false)
