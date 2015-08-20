(ns org.nfrac.comportex.synapses
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util :refer [getx]]))

(defrecord SegUpdate [target-id operation grow-sources die-sources])

(defn seg-update
  "Creates a record defining changes to make to a synaptic
  segment. Operation can be one of `:learn` (increment active sources,
  decrement inactive), `:punish` (decrement active sources) or
  `:reinforce` (increment active sources). Without the operation
  argument, only the growth and death of synapses will be applied."
  ([target-id grow-sources die-sources]
   (SegUpdate. target-id nil grow-sources die-sources))
  ([target-id operation grow-sources die-sources]
   {:pre [(contains? #{:learn :punish :reinforce} operation)]}
   (SegUpdate. target-id operation grow-sources die-sources)))

(defn segment-alterations
  "Returns lists of synapse source ids as `[up promote down demote
   cull]` according to whether they should be increased or decreased
   and whether they are crossing the connected permanence threshold."
  [syns adjustable? reinforce? pcon pinc pdec cull-zeros?]
  (let [pcon (double pcon)
        pcon+pdec (double (+ pcon pdec))
        pcon-pinc (double (- pcon pinc))]
    (loop [syns (seq syns)
           up ()
           promote ()
           down ()
           demote ()
           cull ()]
      (if syns
        ;; process one synapse
        (let [[id p] (first syns)
              p (double p)]
          (if-not (adjustable? id)
            (recur (next syns) up promote down demote cull)
            (if (reinforce? id)
              ;; positive reinforce
              (recur (next syns)
                     (if (< p 1.0) (conj up id) up)
                     (if (and (< p pcon)
                              (>= p pcon-pinc))
                       (conj promote id) promote)
                     down
                     demote
                     cull)
              ;; negative reinforce
              (recur (next syns)
                     up
                     promote
                     (if (> p 0.0) (conj down id) down)
                     (if (and (>= p pcon)
                              (< p pcon+pdec))
                       (conj demote id) demote)
                     (if (and (<= p 0.0) cull-zeros?)
                       (conj cull id) cull)))))
        ;; finished loop
        [up promote down demote cull]
        ))))

(defn- never [_] false)
(defn- always [_] true)

(defrecord SynapseGraph
    [syns-by-target targets-by-source pcon cull-zeros?]
  p/PSynapseGraph
  (in-synapses
    [this target-id]
    (get syns-by-target target-id))
  (sources-connected-to
    [this target-id]
    (->> (get syns-by-target target-id)
         (keep (fn [[k p]] (when (>= p pcon) k)))))
  (targets-connected-from
    [this source-id]
    (get targets-by-source source-id))
  (excitations
    [this active-sources stimulus-threshold]
    (->> active-sources
         (reduce (fn [m source-i]
                   (reduce (fn [m id]
                             (assoc! m id (inc (get m id 0))))
                           m
                           (p/targets-connected-from this source-i)))
                 (transient {}))
         (persistent!)
         (reduce-kv (fn [m id exc]
                   (if (>= exc stimulus-threshold)
                     (assoc! m id exc)
                     m))
                 (transient {}))
         (persistent!)))
  (bulk-learn
    [this seg-updates active-sources pinc pdec pinit]
    (loop [seg-updates (seq seg-updates)
           syns-by-target (transient syns-by-target)
           targets-by-source (transient targets-by-source)]
      (if-let [seg-up (first seg-updates)]
        (let [{:keys [target-id operation grow-sources die-sources]} seg-up
              syns* (getx syns-by-target target-id)
              syns (if (seq die-sources)
                     (apply dissoc syns* die-sources)
                     syns*)
              [up promote down demote cull]
              (case operation
                :learn (segment-alterations syns always active-sources
                                            pcon pinc pdec cull-zeros?)
                :punish (segment-alterations syns active-sources never
                                             pcon pinc pdec cull-zeros?)
                :reinforce (segment-alterations syns active-sources always
                                                pcon pinc pdec cull-zeros?)
                ;; otherwise - just grow and die
                [() () () () ()])
              new-syns (-> (if (seq cull)
                             (apply dissoc! (transient syns) cull)
                             (transient syns))
                           (util/update-each! up #(min (+ % pinc) 1.0))
                           (util/update-each! down #(max (- % pdec) 0.0))
                           (conj! (zipmap grow-sources (repeat pinit)))
                           (persistent!))
              connect-ids (if (>= pinit pcon) (concat promote grow-sources) promote)
              disconnect-ids (concat demote die-sources)]
          (recur (next seg-updates)
                 (assoc! syns-by-target target-id new-syns)
                 (-> targets-by-source
                     (util/update-each! connect-ids #(conj % target-id))
                     (util/update-each! disconnect-ids #(disj % target-id)))))
        ;; finished loop
        (assoc this
               :syns-by-target (persistent! syns-by-target)
               :targets-by-source (persistent! targets-by-source))))))

(defn empty-synapse-graph
  [n-targets n-sources pcon cull-zeros?]
  (map->SynapseGraph
   {:syns-by-target (vec (repeat n-targets {}))
    :targets-by-source (vec (repeat n-sources #{}))
    :pcon pcon
    :cull-zeros? cull-zeros?}))

(defn synapse-graph
  [syns-by-target n-sources pcon cull-zeros?]
  (let [targets-by-source
        (persistent!
         (reduce-kv (fn [v target-id syns]
                      (let [connect-ids (keep (fn [[i p]]
                                                (when (>= p pcon) i)) syns)]
                        (util/update-each! v connect-ids #(conj % target-id))))
                    (transient (vec (repeat n-sources #{})))
                    syns-by-target))]
    (map->SynapseGraph
     {:syns-by-target syns-by-target
      :targets-by-source targets-by-source
      :pcon pcon
      :cull-zeros? cull-zeros?})))

;;; ## Dendrite segments

(defn seg-uidx
  [depth max-segs [col ci si]]
  (+ (* col depth max-segs)
     (* ci max-segs)
     si))

(defn seg-path
  [depth max-segs uidx]
  (let [col (quot uidx (* depth max-segs))
        col-rem (rem uidx (* depth max-segs))]
    [col
     (quot col-rem max-segs)
     (rem col-rem max-segs)]))

(defrecord CellSegmentsSynapseGraph
    [int-sg n-cols depth max-segs]
  p/PSynapseGraph
  (in-synapses
    [_ target-id]
    (p/in-synapses int-sg (seg-uidx depth max-segs target-id)))
  (sources-connected-to
    [_ target-id]
    (assert target-id)
    (p/sources-connected-to int-sg (seg-uidx depth max-segs target-id)))
  (targets-connected-from
    [_ source-id]
    (->> (p/targets-connected-from int-sg source-id)
         (map (partial seg-path depth max-segs))))
  (excitations
    [_ active-sources stimulus-threshold]
    (let [exc-m (p/excitations int-sg active-sources stimulus-threshold)]
      (zipmap (map (fn [i] (seg-path depth max-segs i))
                   (keys exc-m))
              (vals exc-m))))
  (bulk-learn
    [this seg-updates active-sources pinc pdec pinit]
    (update-in this [:int-sg] p/bulk-learn
               (map (fn [seg-up]
                      (assoc seg-up :target-id
                             (seg-uidx depth max-segs (:target-id seg-up))))
                    seg-updates)
               active-sources pinc pdec pinit))
  p/PSegments
  (cell-segments
    [this cell-id]
    (let [cell-id (vec cell-id)]
      (mapv #(p/in-synapses this (conj cell-id %))
            (range max-segs)))))

(defn cell-segs-synapse-graph
  "A synapse graph where the targets refer to distal dendrite
  segments on cells, which themselves are arranged in columns.
  Accordingly `target-id` is passed and returned not as an integer
  but as a 3-tuple `[col ci si]`, column id, cell id, segment id.
  Sources often refer to cells but are passed and returned as
  **integers**, so any conversion to/from cell ids should happen
  externally."
  [n-cols depth max-segs n-sources pcon cull-zeros?]
  (let [n-targets (* n-cols depth max-segs)
        int-sg (empty-synapse-graph n-targets n-sources pcon cull-zeros?)]
    (map->CellSegmentsSynapseGraph
     {:int-sg int-sg
      :depth depth
      :max-segs max-segs})))

(defn col-segs-synapse-graph
  "A synapse graph where the targets refer to proximal dendrite
  segments on columns.  Accordingly `target-id` is passed and returned
  not as an integer but as a 3-tuple `[col 0 si]`, column id, (cell = 0),
  segment id.  Sources often refer to cells but are passed and
  returned as **integers**, so any conversion to/from cell ids should
  happen externally.  Initial synapses are given for each segment."
  [syns-by-col n-cols max-segs n-sources pcon cull-zeros?]
  (let [n-targets (* n-cols max-segs)
        int-syns-by-target (persistent!
                            (reduce (fn [v [col syns]]
                                      (let [path [col 0 0]
                                            i (seg-uidx 1 max-segs path)]
                                        (assoc! v i syns)))
                                    (transient (vec (repeat n-targets {})))
                                    (map-indexed vector syns-by-col)))
        int-sg (synapse-graph int-syns-by-target n-sources pcon cull-zeros?)]
    (map->CellSegmentsSynapseGraph
     {:int-sg int-sg
      :depth 1
      :max-segs max-segs})))
