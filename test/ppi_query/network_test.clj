(ns ppi-query.network-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.interaction :as intr]
            [ppi-query.fetch :as fetch]
            [ppi-query.network :as network]
            [proto-repl-charts.graph :as g]))

(defn get-edges [interactions]
  (->> interactions
    (map intr/get-interactors-uniprotids)
    (remove (partial some nil?))))

(defn get-nodes-list [edges]
  (remove nil? (into #{} (flatten edges))))

(defn nodes-map-from-edges [edges group]
  (apply assoc {}
         (interleave
           (get-nodes-list edges)
           (repeat group))))

(defn merge-nodes-map [nodes-map-list]
  (apply merge-with min nodes-map-list))

(defn graph-nodes-from-nodes-map [nodes-map]
  (map (fn [[k v]] (hash-map :id k :label k :group v))
       nodes-map))

(defn graph-nodes-from-nodes-map-list [nodes-map-list]
  (graph-nodes-from-nodes-map
     (merge-nodes-map nodes-map-list)))

(defn get-nodes-from-edges [edges group]
  (map #(hash-map :id % :label % :group group)
       (get-nodes-list [edges])))


(comment
  (binding [*print-level* 3]
    (let [databases ["IntAct"]
          ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans")
          proteins [;(prot/->Protein ref-organism "Q18688")
                    (prot/->Protein ref-organism "Q20646")]
          other-organisms [(orgn/inparanoid-organism-by-shortname "M.musculus")
                           (orgn/inparanoid-organism-by-shortname "S.pombe")]]

      (let [clients (fetch/get-clients databases)
            direct-interactions
              (fetch/get-direct-interactions
                clients ref-organism proteins)
        ;(comment
            [organism orthologs interactions]
            (network/get-orthologs-direct-interactions
              clients (first other-organisms) proteins)
        ;(comment
            orthologs-direct-interactions
              (map (fn [organism]
                      (network/get-orthologs-direct-interactions
                        clients organism proteins))
                   other-organisms)

        ;(comment
            [return-proteins secondary-interactions]
            (network/merge-proteins-and-get-secondary-interactions
              clients ref-organism proteins
              direct-interactions orthologs-direct-interactions)
        ;(comment
            orthologs-secondary-interactions-first-org
              (network/get-orthologs-secondary-interactions
                clients organism orthologs interactions)
        ;(comment
            orthologs-secondary-interactions
              (mapcat (fn [[organism orthologs interactions]]
                          (network/get-orthologs-secondary-interactions
                            clients organism orthologs interactions))
                      orthologs-direct-interactions)]
        (println
          direct-interactions return-proteins
          "\n"
          orthologs-secondary-interactions
          "\n"
          (intr/get-proteins direct-interactions)
          (intr/interactions->proteins-set direct-interactions)
          (map intr/get-interactors-uniprotids direct-interactions)
          (get-edges direct-interactions)

          "\n  direct-interactions"
          (s/valid? ::intr/interactions
            direct-interactions)
        ;(comment
          "\n  orthologs-direct-interactions-first-org prots"
          (s/valid? (s/coll-of ::prot/protein)
            orthologs)
        ;(comment
          "\n  orthologs-direct-interactions-first-org intr"
          (s/valid? ::intr/interactions
            interactions)
        ;(comment
          "\n  orthologs-direct-interactions prots"
          (s/valid? (s/coll-of ::prot/protein)
            (mapcat second orthologs-direct-interactions))
        ;(comment
          "\n  orthologs-direct-interactions intrs"
          (s/valid? ::intr/interactions
            (mapcat #(nth % 2) orthologs-direct-interactions))
        ;(comment
          "\n  return-proteins"
          (s/valid? (s/coll-of ::prot/protein)
            return-proteins)
        ;(comment
          "\n  secondary-interactions"
          (s/valid? ::intr/interactions
            secondary-interactions)
        ;(comment
          "\n  orthologs-secondary-interactions-first-org"
          (s/valid? ::intr/interactions
            orthologs-secondary-interactions-first-org)
        ;(comment
          "\n  orthologs-secondary-interactions"
          (s/valid? ::intr/interactions
            orthologs-secondary-interactions))))))


(comment

  (binding [*print-level* 4]
    (let [databases ["IntAct"]
          ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans")
          proteins [;(prot/->Protein ref-organism "Q18688")
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
        ;(println "direct-interactions" direct-interactions)
        (println "")
        ;(println "secondary-interactions" secondary-interactions)
        (println "")
        ;(println "return-proteins" return-proteins)
        (println "")
        ;(println "orthologs-secondary-interactions" orthologs-secondary-interactions)
        (println "")
        (println (s/valid? ::intr/interactions direct-interactions))
        (println (s/valid? ::intr/interactions secondary-interactions))
        (println (s/valid? (s/coll-of ::prot/protein) return-proteins))
        (println (s/valid? ::intr/interactions orthologs-secondary-interactions))

        (let [direct-edges (get-edges direct-interactions)
              sec-edges    (get-edges secondary-interactions)
              orth-edges   (get-edges orthologs-secondary-interactions)
              direct-nodes (nodes-map-from-edges direct-edges 1)
              sec-nodes    (nodes-map-from-edges sec-edges 2)
              orth-nodes   (nodes-map-from-edges orth-edges 3)
              all-edges    (concat direct-edges sec-edges orth-edges)
              all-nodes    (graph-nodes-from-nodes-map-list
                             [direct-nodes sec-nodes orth-nodes])]

         (println "direct-edges" direct-edges)
         (println "sec-edges" sec-edges)
         (println "orth-edges" orth-edges)
         (println "direct-edges" direct-edges)
         (println "sec-edges" sec-edges)
         (println "orth-edges" orth-edges)

         (g/graph
           "Psicquic interactions"
           {:nodes all-nodes :edges all-edges}))))))
