(ns ppi-query.network-graph-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.fetch :as fetch]
            [ppi-query.network :as network]
            [proto-repl-charts.graph :as g]))
(stest/instrument)

(do
  (def dbs ["IntAct"])
  (def clients (fetch/get-clients dbs))
  ;Human / Homo sapiens
  ;Taxonomy ID: 9606
  (def org-human (orgn/inparanoid-organism-by-id 9606))
  ;Caenorhabditis elegans
  ;Taxonomy ID: 6239
  (def org-celegans (orgn/inparanoid-organism-by-shortname "C.elegans"))
  ;Mus musculus
  ;Taxonomy ID: 10090
  (def org-musculus (orgn/inparanoid-organism-by-shortname "M.musculus"))
  ;Schizosaccharomyces pombe
  ;Taxonomy ID: 4896
  (def org-spombe (orgn/inparanoid-organism-by-shortname "S.pombe"))
  ;Arabidopsis thaliana
  ;Taxonomy ID: 3702
  (def org-thaliana (orgn/inparanoid-organism-by-id 3702))
  ;Bos taurus
  ;Taxonomy ID: 9913
  (def org-taurus (orgn/inparanoid-organism-by-id 9913))
  ;Drosophila melanogaster
  ;Taxonomy ID: 7227
  (def org-melanogaster (orgn/inparanoid-organism-by-id 7227))
  ;Saccharomyces cerevisiae
  ;Taxonomy ID: 4932
  (def org-cerevisiae (orgn/inparanoid-organism-by-shortname "S.cerevisiae"))
  ;Rattus norvegicus
  ;Taxonomy ID: 10116
  (def org-norvegicus (orgn/inparanoid-organism-by-id 10116))

  (def all-orgs
    [org-human org-celegans org-musculus org-spombe org-thaliana
     org-taurus org-melanogaster org-cerevisiae org-norvegicus])
  (def all-orgs-expt-human
    [org-celegans org-musculus org-spombe org-thaliana
     org-taurus org-melanogaster org-cerevisiae org-norvegicus])
  (def all-orgs-expt-celegans
    [org-human org-musculus org-spombe org-thaliana
     org-taurus org-melanogaster org-cerevisiae org-norvegicus])

  (def prot-celegans-1 (prot/->Protein org-celegans "Q18688"))
  (def prot-celegans-2 (prot/->Protein org-celegans "Q20646"))
  (def prot-human-1 (prot/->Protein org-human "Q08752"))
  (def prot-human-2 (prot/->Protein org-human "P08238"))

  ; Around 15 seconds
  (def ref-org-1 org-celegans)
  (def proteins-set-1 [prot-celegans-1
                       prot-celegans-2])
  (def other-orgs-set-1 [org-musculus
                         org-spombe])

  ; Around 20 seconds
  (def ref-org-2 org-celegans)
  (def proteins-set-2 [prot-celegans-1
                       prot-celegans-2])
  (def other-orgs-set-2 [org-musculus org-spombe org-thaliana
                         org-taurus org-norvegicus])

  ; Around 20 seconds ; FAIL
  (def other-orgs-set-2-fail [org-melanogaster])


  ; Around 8 minutes ; FAIL @ Secondary interactions
  (def ref-org-3 org-celegans)
  (def proteins-set-3 [prot-celegans-1 prot-celegans-2])
  (def other-orgs-set-3
        [org-human org-musculus org-spombe org-thaliana
         org-taurus org-cerevisiae org-norvegicus])

  (def ref-org-4 org-human)
  (def proteins-set-4 [prot-human-1])
  (def other-orgs-set-4 all-orgs-expt-human)

  (def ref-org-5 org-human)
  (def proteins-set-5 [prot-human-2])
  (def other-orgs-set-5 all-orgs-expt-human)

  (def ref-org-6 org-human)
  (def proteins-set-6 [prot-human-1 prot-human-2])
  (def other-orgs-set-6 all-orgs-expt-human))

  ;(dorun (map println all-orgs)))
(comment
 (binding [*print-level* 3]
  (let [[ret-proteins ret-interactions]
        (network/fetch-protein-network
              dbs ; PSICQUIC databases to query
                     ref-org-2   ; Organism of Interest
                proteins-set-2   ; Proteins of Interest
              other-orgs-set-2-fail)] ; Other Organisms to check
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
                            (not (= org-a org-b)) (throw (Exception. "Interaction between two different organisms !"))
                            (not org-a) "red"
                            (= org-a (nth all-orgs 0)) "blue" ;Human / Homo sapiens
                            (= org-a (nth all-orgs 1)) "green" ;Caenorhabditis elegans
                            (= org-a (nth all-orgs 2)) "orange" ;Mus musculus
                            (= org-a (nth all-orgs 3)) "yellow" ;Schizosaccharomyces pombe
                            (= org-a (nth all-orgs 4)) "purple" ;Arabidopsis thaliana
                            (= org-a (nth all-orgs 5)) "violet" ;Bos taurus
                            (= org-a (nth all-orgs 6)) "salmon" ;Drosophila melanogaster
                            (= org-a (nth all-orgs 7)) "teal" ;Saccharomyces cerevisiae
                            (= org-a (nth all-orgs 8)) "grey") ;Rattus norvegicus
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
      (if (not (empty? edges-verify))
          (do (println edges-verify)
              (throw (Exception. "An interaction exist between proteins not in ret-proteins !"))))
      ;(println "nodes-uniprots")
      ;(println nodes-uniprots)
      ;(println "edges-verify")
      ;(println edges-verify)
      ;(println "nodes")
      ;(println nodes)
      ;(println "edges")
      ;(doall (map println edges))

      (g/graph
        "Psicquic interactions"
        {:nodes nodes :edges edges})))))
