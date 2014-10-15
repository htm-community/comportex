(defproject org.nfrac/comportex "0.0.4"
  :description "Functionally composable cortex, an implementation of Hierarchical Temporal Memory"
  :url "http://github.com/floybix/comportex/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj" "src/cljx" "target/classes"]
  :test-paths ["target/test-classes"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.cemerick/pprng "0.0.2"]
                 [cljs-uuid "0.0.4"]]

  :jar-exclusions [#"\.cljx"]
  :jvm-opts ["-server" "-Xmx2500m"]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-2234"]
                                  [criterium "0.4.3"]]
                   :plugins [[com.keminglabs/cljx "0.4.0"]
                             [lein-cljsbuild "1.0.3"]
                             [com.cemerick/clojurescript.test "0.3.1"]
                             [com.cemerick/austin "0.1.4"]
                             [lein-marginalia "0.8.0"]]
                   :cljx {:builds [{:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :clj}

                                   {:source-paths ["src/cljx"]
                                    :output-path "target/classes"
                                    :rules :cljs}

                                   {:source-paths ["test/cljx"]
                                    :output-path "target/test-classes"
                                    :rules :clj}

                                   {:source-paths ["test/cljx"]
                                    :output-path "target/test-classes"
                                    :rules :cljs}]}

                   :hooks [cljx.hooks]

                   :cljsbuild {:builds [{:source-paths ["target/classes" "target/test-classes"]
                                         :jar true
                                         :compiler {:output-to "target/testable.js"
                                                    :libs [""]
                                                    :optimizations :advanced}}]}}}

  :autodoc {:copyright "Copyright (C) 2014 Felix Andrews."
            :web-src-dir "http://github.com/floybix/comportex/blob/"
            :web-home "http://floybix.github.com/comportex/"})
