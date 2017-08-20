(ns ppi-query.network-graph-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.fetch :as fetch]
            [ppi-query.network :as network]
            [proto-repl-charts.graph :as g]))
(stest/instrument)

(def dbs ["IntAct"])
(def clients (fetch/get-clients dbs))
(def ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans"))
(def protein-1 (prot/->Protein ref-organism "Q18688"))
(def protein-2 (prot/->Protein ref-organism "Q20646"))
(def proteins [protein-1
               protein-2])
(def other-organism-1 (orgn/inparanoid-organism-by-shortname "M.musculus"))
(def other-organism-2 (orgn/inparanoid-organism-by-shortname "S.pombe"))
(def other-organisms [other-organism-1
                      other-organism-2])

(comment
 (binding [*print-level* 4]
  (let [[ret-proteins ret-interactions]
        (network/fetch-protein-network
              dbs ; PSICQUIC databases to query
              ref-organism  ; Organism of Interest
              proteins ; Proteins of Interest
              other-organisms)] ; Other Organisms to check
    (println "#########################################")
    (println "###     END FETCH PROTEIN NETWORK     ###")
    (println "#########################################")
    ;(println "ret-proteins" ret-proteins)
    (println "")
    ;(println "ret-interactions" ret-interactions)
    (println "")

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
                        color
                          (cond
                            (not (= org-a org-b)) "orange"
                            (not org-a) "red"
                            (= org-a other-organism-1) "blue"
                            (= org-a other-organism-2) "green")
                        label
                          (if ortholog-protein-a
                            (apply str
                              (:uniprotid ortholog-protein-a)
                              "<->"
                              (:uniprotid ortholog-protein-b))
                            "")]

                       {:from (:uniprotid protein-a)
                        :to   (:uniprotid protein-b)
                        :color color
                        :label label}))
                 ret-interactions)]
      (println "nodes-uniprots")
      (println nodes-uniprots)
      (println "edges-verify")
      (println edges-verify)
      (println "nodes")
      (println nodes)
      (println "edges")
      (doall (map println edges))

      (g/graph
        "Psicquic interactions"
        {:nodes nodes :edges edges})))))
