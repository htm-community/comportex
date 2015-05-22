(ns org.nfrac.comportex.synapses
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util :refer [getx]]))

(defn segment-alterations
  "Returns lists of synapse source ids as `[up promote down demote
   cull]` according to whether they should be increased or decreased
   and whether they are crossing the connected permanence threshold."
  [syns skip? reinforce? pcon pinc pdec cull-zeros?]
  (loop [syns (seq syns)
         up ()
         promote ()
         down ()
         demote ()
         cull ()]
    (if syns
      ;; process one synapse
      (let [[id p] (first syns)]
        (if (skip? id)
          (recur (next syns) up promote down demote cull)
          (if (reinforce? id)
            ;; positive reinforce
            (recur (next syns)
                   (if (< p 1.0) (conj up id) up)
                   (if (and (< p pcon)
                            (>= p (- pcon pinc)))
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
                            (< p (+ pcon pdec)))
                     (conj demote id) demote)
                   (if (and (<= p 0.0) cull-zeros?)
                     (conj cull id) cull)))))
      ;; finished loop
      [up promote down demote cull]
      )))

(defrecord SynapseGraph
    [syns-by-target targets-by-source pcon cull-zeros?]
  p/PSynapseGraph
  (in-synapses
    [this target-id]
    (syns-by-target target-id))
  (sources-connected-to
    [this target-id]
    (->> (p/in-synapses this target-id)
         (keep (fn [[k p]] (when (>= p pcon) k)))))
  (targets-connected-from
    [this source-id]
    (targets-by-source source-id))
  (reinforce-in-synapses
    [this target-id skip? reinforce? pinc pdec]
    (let [syns (syns-by-target target-id)
          [up promote down demote cull]
          (segment-alterations syns skip? reinforce? pcon pinc pdec cull-zeros?)]
      (-> this
          (update-in [:syns-by-target target-id]
                     (fn [syns]
                       (-> (if (seq cull)
                             (apply dissoc syns cull)
                             syns)
                           (util/update-each up #(min (+ % pinc) 1.0))
                           (util/update-each down #(max (- % pdec) 0.0)))))
          (update-in [:targets-by-source]
                     util/update-each promote #(conj % target-id))
          (update-in [:targets-by-source]
                     util/update-each demote #(disj % target-id)))
      ))
  (conj-synapses
    [this target-id syn-source-ids p]
    (cond->
        (update-in this [:syns-by-target target-id]
                   merge (zipmap syn-source-ids (repeat p)))
      ;; record connection if initially connected
      (>= p pcon)
      (update-in [:targets-by-source]
                 util/update-each syn-source-ids #(conj % target-id))))
  (disj-synapses
    [this target-id syn-source-ids]
    (-> this
        (update-in [:syns-by-target target-id]
                   (fn [syns] (apply dissoc syns syn-source-ids)))
        (update-in [:targets-by-source]
                   util/update-each syn-source-ids #(disj % target-id))))
  (bulk-learn
    [this learn-info active-sources pinc pdec pinit]
    (let [skip? (constantly false)]
      (loop [learn-info (seq learn-info)
             syns-by-target (transient syns-by-target)
             targets-by-source (transient targets-by-source)]
        (if-let [[target-id grow-sources die-sources] (first learn-info)]
          (let [syns* (getx syns-by-target target-id)
                syns (if (seq die-sources)
                       (apply dissoc syns* die-sources)
                       syns*)
                [up promote down demote cull] (segment-alterations syns skip?
                                                                   active-sources
                                                                   pcon pinc pdec
                                                                   cull-zeros?)
                new-syns (-> (if (seq cull)
                               (apply dissoc! (transient syns) cull)
                               (transient syns))
                             (util/update-each! up #(min (+ % pinc) 1.0))
                             (util/update-each! down #(max (- % pdec) 0.0))
                             (conj! (zipmap grow-sources (repeat pinit)))
                             (persistent!))
                connect-ids (if (>= pinit pcon) (concat promote grow-sources) promote)
                disconnect-ids (concat demote die-sources)]
            (recur (next learn-info)
                   (assoc! syns-by-target target-id new-syns)
                   (-> targets-by-source
                       (util/update-each! connect-ids #(conj % target-id))
                       (util/update-each! disconnect-ids #(disj % target-id)))))
          ;; finished loop
          (assoc this
                 :syns-by-target (persistent! syns-by-target)
                 :targets-by-source (persistent! targets-by-source)))))))

(defn empty-synapse-graph
  [n-targets n-sources pcon cull-zeros?]
  (map->SynapseGraph
   {:syns-by-target (vec (repeat n-targets {}))
    :targets-by-source (vec (repeat n-sources #{}))
    :pcon pcon
    :cull-zeros? cull-zeros?}))

(defn synapse-graph
  [syns-by-target n-sources pcon cull-zeros?]
  (let [target-sets
        (reduce-kv (fn [v tid syns]
                     (let [sids (keep (fn [[k p]]
                                        (when (>= p pcon) k)) syns)]
                       (util/update-each v sids #(conj % tid))))
                   (vec (repeat n-sources #{}))
                   syns-by-target)]
    (map->SynapseGraph
     {:syns-by-target syns-by-target
      :targets-by-source target-sets
      :pcon pcon
      :cull-zeros? cull-zeros?})))

(defn excitations
  "Computes a map of target ids to their degree of excitation: the
   number of sources in `active-sources` they are connected to."
  [syns active-sources]
  (->> active-sources
       (reduce (fn [exc ai]
                 (reduce (fn [exc id]
                           (assoc! exc id (inc (get exc id 0))))
                         exc
                         (p/targets-connected-from syns ai)))
               (transient {}))
       (persistent!)))

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
    [raw-sg max-segs tgt->i i->tgt]
  p/PSynapseGraph
  (in-synapses
    [_ target-id]
    (p/in-synapses raw-sg (tgt->i target-id)))
  (sources-connected-to
    [_ target-id]
    (p/sources-connected-to raw-sg (tgt->i target-id)))
  (targets-connected-from
    [_ source-id]
    (->> (p/targets-connected-from raw-sg source-id)
         (map i->tgt)))
  (reinforce-in-synapses
    [this target-id skip? reinforce? pinc pdec]
    (-> this
        (update-in [:raw-sg] p/reinforce-in-synapses (tgt->i target-id)
                   skip? reinforce?
                   pinc pdec)))
  (conj-synapses
    [this target-id syn-source-ids p]
    (-> this
        (update-in [:raw-sg] p/conj-synapses (tgt->i target-id)
                   syn-source-ids p)))
  (disj-synapses
    [this target-id syn-source-ids]
    (-> this
        (update-in [:raw-sg] p/disj-synapses (tgt->i target-id)
                   syn-source-ids)))
  (bulk-learn
    [this learn-info active-sources pinc pdec pinit]
    (-> this
        (update-in [:raw-sg] p/bulk-learn
                   (map (fn [[t x y]] [(tgt->i t) x y]) learn-info)
                   active-sources pinc pdec pinit)))
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
        raw-sg (empty-synapse-graph n-targets n-sources pcon cull-zeros?)
        tgt->i (partial seg-uidx depth max-segs)
        i->tgt (partial seg-path depth max-segs)]
    (map->CellSegmentsSynapseGraph
     {:raw-sg raw-sg
      :max-segs max-segs
      :tgt->i tgt->i
      :i->tgt i->tgt
      })))

(defn col-segs-synapse-graph
  "A synapse graph where the targets refer to proximal dendrite
  segments on columns.  Accordingly `target-id` is passed and returned
  not as an integer but as a 2-tuple `[col si]`, column id, segment
  id.  Sources often refer to cells but are passed and returned as
  **integers**, so any conversion to/from cell ids should happen
  externally.  Initial synapses are given for each segment."
  [syns-by-target n-cols max-segs n-sources pcon cull-zeros?]
  (let [raw-sg (synapse-graph syns-by-target n-sources pcon cull-zeros?)
        tgt->i (fn [[col si]] (+ (* col max-segs) si))
        i->tgt (fn [uidx] [(quot uidx max-segs)
                           (rem uidx max-segs)])]
    (map->CellSegmentsSynapseGraph
     {:raw-sg raw-sg
      :max-segs max-segs
      :tgt->i tgt->i
      :i->tgt i->tgt
      })))
