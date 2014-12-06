;; Clojure loads user.clj long before cljx is loaded.
;; This hack is only necessary within the comportex project --
;; other comportex consumers never see the cljx files.
(require '[cljx.repl-middleware :as cljx])
(reset! @#'cljx/cljx-load-rules {:clj cljx.rules/clj-rules})
@@#'cljx/install-cljx-load

(ns user
  (:require org.nfrac.comportex.repl))

(org.nfrac.comportex.repl/truncate-large-data-structures)
