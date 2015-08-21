(ns org.nfrac.comportex.repl
  "Optional REPL tweaks"
  #?(:clj (:require [clojure.pprint :as pprint]
                    [org.nfrac.comportex.cells]
                    [org.nfrac.comportex.synapses])
     :cljs (:require [org.nfrac.comportex.cells
                      :refer [LayerOfCells LayerActiveState LayerDistalState]]
                     [org.nfrac.comportex.synapses :refer [SynapseGraph]]))
  #?(:clj (:import [org.nfrac.comportex.cells
                    LayerOfCells LayerActiveState LayerDistalState]
                   [org.nfrac.comportex.synapses SynapseGraph])))

;;; ## Truncate large data structures

(defn patchmethod1
  "Transform the first argument of calls to `multifn` before calling the method
  currently associated with dispatch-value. This transform happens after
  dispatch."
  [multifn dispatch-val f]
  (let [dispatch-fn (get-method multifn dispatch-val)]
    (defmethod multifn dispatch-val
      [arg1 & more]
      (apply dispatch-fn (f arg1) more))))

(def ^:dynamic *truncated-print-length* 3)

(def print-methods #?(:clj [print-method
                            pprint/simple-dispatch]
                      :cljs []))

(def should-truncate {LayerOfCells
                      [:boosts :active-duty-cycles]
                      SynapseGraph
                      [:syns-by-target :targets-by-source]
                      LayerActiveState
                      [:col-overlaps :matching-ff-seg-paths
                       :in-ff-bits :in-stable-ff-bits
                       :out-ff-bits :out-stable-ff-bits
                       :active-cells]
                      LayerDistalState
                      [:distal-bits :distal-lc-bits :distal-exc :pred-cells
                       :matching-seg-paths]})

(defrecord TruncateOnPrint [v])

(defn truncate-large-data-structures []
  ;; The TruncateOnPrint is invisible. Its contents are visible, but truncated.
  (doseq [m print-methods]
    (defmethod m TruncateOnPrint
      [this & args]
      (binding [*print-length* (if (and *print-length*
                                        *truncated-print-length*)
                                 (min *print-length*
                                      *truncated-print-length*)
                                 (or *print-length*
                                     *truncated-print-length*))]
        (apply m (:v this) args))))

  ;; Before printing records, wrap the specified fields in a TruncateOnPrint.
  (doseq [m print-methods
          [recordclass noisykeys] should-truncate]
    (patchmethod1 m recordclass
                  (fn [this]
                    (reduce #(update-in % [%2] ->TruncateOnPrint)
                            this noisykeys)))))
