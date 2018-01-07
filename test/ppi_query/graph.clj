(ns ppi-query.graph
  (:require [ppi-query.network :as network]))

(defn to-graph-data [ret-proteins ret-interactions]
  (let [nodes-uniprots
          (into #{}
            (map (fn [{:keys [organism uniprotid]}]
                     uniprotid)
                 ret-proteins))
        edges-verify
          (remove nil?
            (map (fn [{:keys [protein-a ortholog-protein-a
                              protein-b ortholog-protein-b
                              origin-interaction]}]
                   (if (not (and (contains? nodes-uniprots (:uniprotid protein-a))
                                 (contains? nodes-uniprots (:uniprotid protein-b))))
                     [protein-a ortholog-protein-a
                      protein-b ortholog-protein-b
                      origin-interaction]))
                 ret-interactions))
        nodes
          (map #(hash-map :id % :label %)
               nodes-uniprots)
        edges
          (map (fn [{:keys [protein-a ortholog-protein-a
                            protein-b ortholog-protein-b
                            origin-interaction]}]
                (let [org-a (:organism ortholog-protein-a)
                      org-b (:organism ortholog-protein-b)
                      label
                        (if ortholog-protein-a
                          (apply str
                            (:uniprotid ortholog-protein-a)
                            " (" (:ortholog-score ortholog-protein-a) ")"
                            "<->"
                            (:uniprotid ortholog-protein-b)
                            " (" (:ortholog-score ortholog-protein-b) ")")
                          "")
                      desc
                        (if ortholog-protein-a
                          (:scientific-name org-a)
                          (:scientific-name (:organism protein-a)))]
                     (if (not (= org-a org-b)) (throw (Exception. "Interaction between two different organisms !")))
                     {:from (:uniprotid protein-a)
                      :to   (:uniprotid protein-b)
                      :label label
                      :desc desc}))
               ret-interactions)]
    (if (empty? edges-verify)
        {:nodes nodes :edges edges}
        (do (println edges-verify)
            (throw (Exception. "An interaction exist between proteins not in ret-proteins !"))))))
