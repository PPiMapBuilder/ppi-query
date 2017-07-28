(ns ppi-query.network-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.interaction :as intr]
            [ppi-query.network :as network]
            [proto-repl-charts.graph :as g]))


(defn get-edges [interactions]
  (->> interactions
    (map intr/get-interactors-uniprotids)
    (remove #(contains? % nil?))))

(defn get-nodes-simple [edges]
  (remove nil? (into #{} (flatten edges))))

(defn get-nodes-from-edges [edges group]
  (map #(hash-map :id % :label % :group group)
       (get-nodes-simple [edges])))

(comment

  (binding [*print-level* 4]
    (let [databases ["IntAct"]
          ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans")
          proteins [(prot/->Protein ref-organism "Q18688")
                    (prot/->Protein ref-organism "Q20646")]
          other-organisms [(orgn/inparanoid-organism-by-shortname "M.musculus")
                           (orgn/inparanoid-organism-by-shortname "S.pombe")]]

      (let [[direct-interactions secondary-interactions return-proteins
             orthologs-secondary-interactions]
            (network/fetch-protein-network
                  databases ; PSICQUIC databases to query
                  ref-organism  ; Organism of Interest
                  proteins ; Proteins of Interest
                  other-organisms)] ; Other Organisms to check
        (println "#########################################")
        (println "###     END FETCH PROTEIN NETWORK     ###")
        (println "#########################################")
        (println "direct-interactions" direct-interactions)
        (println "secondary-interactions" secondary-interactions)
        (println "return-proteins" return-proteins)
        (println "orthologs-secondary-interactions" orthologs-secondary-interactions)

        (let [direct-edges (get-edges direct-interactions)
              sec-edges    (get-edges secondary-interactions)
              orth-edges   (get-edges orthologs-secondary-interactions)
              direct-nodes (get-nodes-from-edges direct-edges 1)
              sec-nodes    (get-nodes-from-edges sec-edges 2)
              orth-nodes   (get-nodes-from-edges orth-edges 3)]

         (g/graph
           "Psicquic interactions"
           {:nodes direct-nodes :edges direct-edges}))))))
