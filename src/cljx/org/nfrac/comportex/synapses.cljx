(ns org.nfrac.comportex.synapses
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]))

(defrecord SynapseGraph
    [syns-by-target targets-by-source pcon max-syns cull-zeros?]
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
    (let [syns (p/in-synapses this target-id)]
      (loop [syns (seq syns)
             up ()
             promote ()
             down ()
             demote ()
             cull ()]
        (if (seq syns)
          ;; process one synapse
          (let [[id p] (first syns)]
            (if (skip? id)
              (recur (next syns) up down promote demote cull)
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
                         util/update-each demote #(disj % target-id)))))))
  (conj-synapses
    [this target-id syn-source-ids p]
    (let [osyns (p/in-synapses this target-id)
          syns (merge osyns (zipmap syn-source-ids (repeat p)))]
      (cond->
       (assoc-in this [:syns-by-target target-id] syns)
       ;; record connection if initially connected
       (>= p pcon)
       (update-in [:targets-by-source]
                  util/update-each syn-source-ids #(conj % target-id))
       ;; if too many synapses, remove those with lowest permanence
       (> (count syns) max-syns)
       (p/disj-synapses target-id
                        (->> (sort-by val syns)
                             (keys)
                             (take (- (count syns) max-syns)))))))
  (disj-synapses
    [this target-id syn-source-ids]
    (-> this
        (update-in [:syns-by-target target-id]
                   (fn [syns] (apply dissoc syns syn-source-ids)))
        (update-in [:targets-by-source]
                   util/update-each syn-source-ids #(disj % target-id)))))

(defn empty-synapse-graph
  [n-targets n-sources pcon max-syns cull-zeros?]
  (map->SynapseGraph
   {:syns-by-target (vec (repeat n-targets {}))
    :targets-by-source (vec (repeat n-sources #{}))
    :pcon pcon
    :max-syns max-syns
    :cull-zeros? cull-zeros?}))

(defn synapse-graph
  [syns-by-target n-sources pcon max-syns cull-zeros?]
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
      :max-syns max-syns
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

(defrecord SynapseGraphBySegments
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
  p/PSegments
  (cell-segments
    [this cell-id]
    (let [[col ci] cell-id]
      (mapv #(p/in-synapses this [col ci %])
            (range max-segs)))))

(defn synapse-graph-by-segments
  "A synapse graph where the targets refer to individual dendrite
   segments on cells, which themselves are arranged in columns.
   Accordingly `target-id` is passed and returned not as an integer
   but as a 3-tuple `[col ci si]`, column id, cell id, segment id.
   Sources often refer to cells but are passed and returned as
   **integers**, so any conversion to/from cell ids should happen
   externally."
  [n-cols depth max-segs n-sources pcon max-syns cull-zeros?]
  (let [n-targets (* n-cols depth max-segs)
        raw-sg (empty-synapse-graph n-targets n-sources pcon max-syns cull-zeros?)
        tgt->i (partial seg-uidx depth max-segs)
        i->tgt (partial seg-path depth max-segs)]
    (map->SynapseGraphBySegments
     {:raw-sg raw-sg
      :max-segs max-segs
      :tgt->i tgt->i
      :i->tgt i->tgt
      })))

