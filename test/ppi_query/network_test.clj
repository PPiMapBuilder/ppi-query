(ns ppi-query.network-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology.data :as orthd]
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

(def count-is (fn [cnt seq] (is (= cnt (count seq)))))

(def dbs ["IntAct"])
(def clients (fetch/get-clients dbs))
(def ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans"))
(def proteins-1 [(prot/->Protein ref-organism "Q18688")])
(def proteins-2 [(prot/->Protein ref-organism "Q20646")])
(def proteins [(first proteins-1)
               (first proteins-2)])
(def other-organisms-1 [(orgn/inparanoid-organism-by-shortname "M.musculus")])
(def other-organisms-2 [(orgn/inparanoid-organism-by-shortname "S.pombe")])
(def other-organisms [(first other-organisms-1)
                      (first other-organisms-2)])


(deftest test-get-ortholog-direct-interactions
 (binding [*print-level* 2]
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (first other-organisms) proteins-1)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-sourced-proteins ortholog-prots
     ::intr/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog (first other-organisms)))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P11499" (:uniprotid (first ortholog-prots))))
   (is (= (first proteins-1) (:origin-protein (first ortholog-prots))))
   (count-is 18 orth-dir-interactions))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (second other-organisms) proteins-1)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-sourced-proteins ortholog-prots
     ::intr/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog (second other-organisms)))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P41887" (:uniprotid (first ortholog-prots))))
   (is (= (first proteins-1) (:origin-protein (first ortholog-prots))))
   (count-is 0 orth-dir-interactions))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (first other-organisms) proteins-2)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-sourced-proteins ortholog-prots
     ::intr/interactions orth-dir-interactions)
   (count-is 0 ortholog-prots))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (second other-organisms) proteins-2)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-sourced-proteins ortholog-prots
     ::intr/interactions orth-dir-interactions)
   (count-is 0 ortholog-prots))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (first other-organisms) proteins)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-sourced-proteins ortholog-prots
     ::intr/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog (first other-organisms)))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P11499" (:uniprotid (first ortholog-prots))))
   (is (= (first proteins-1) (:origin-protein (first ortholog-prots))))
   (count-is 18 orth-dir-interactions))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (second other-organisms) proteins)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-sourced-proteins ortholog-prots
     ::intr/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog (second other-organisms)))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P41887" (:uniprotid (first ortholog-prots))))
   (is (= (first proteins-1) (:origin-protein (first ortholog-prots))))
   (count-is 0 orth-dir-interactions))))

(deftest test-get-orthologs-direct-interactions
 (binding [*print-level* 3]
  (let [orthologs-direct-interactions
          (network/get-orthologs-direct-interactions
            clients other-organisms-1 proteins)]
    ;(println orthologs-direct-interactions)
    (is (s/valid?
          (s/coll-of
            (s/cat :ortholog       ::orgn/organism
                   :ortholog-prots ::orthd/ortholog-sourced-proteins
                   :interactions   ::intr/interactions))
          orthologs-direct-interactions))
    (count-is 1 orthologs-direct-interactions))
  (let [orthologs-direct-interactions
          (network/get-orthologs-direct-interactions
            clients other-organisms-2 proteins)]
    ;(println orthologs-direct-interactions)
    (is (s/valid?
          (s/coll-of
            (s/cat :ortholog       ::orgn/organism
                   :ortholog-prots ::orthd/ortholog-sourced-proteins
                   :interactions   ::intr/interactions))
          orthologs-direct-interactions))
    (count-is 1 orthologs-direct-interactions))
  (let [orthologs-direct-interactions
          (network/get-orthologs-direct-interactions
            clients other-organisms proteins)]
    ;(println orthologs-direct-interactions)
    (is (s/valid?
          (s/coll-of
            (s/cat :ortholog       ::orgn/organism
                   :ortholog-prots ::orthd/ortholog-sourced-proteins
                   :interactions   ::intr/interactions))
          orthologs-direct-interactions))
    (count-is 2 orthologs-direct-interactions))))

(deftest test-get-ortholog-secondary-interactions
 (binding [*print-level* 3]
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (first other-organisms) proteins)]
    (let [ortholog-secondary-interactions
            (network/get-ortholog-secondary-interactions
              clients ortholog ortholog-prots orth-dir-interactions)]
      ;(println ortholog-secondary-interactions)
      (is (s/valid? ::intr/interactions
            ortholog-secondary-interactions))
      (count-is 38 ortholog-secondary-interactions)))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients (second other-organisms) proteins)]
    (let [ortholog-secondary-interactions
            (network/get-ortholog-secondary-interactions
              clients ortholog ortholog-prots orth-dir-interactions)]
      ;(println ortholog-secondary-interactions)
      (is (s/valid? ::intr/interactions
            ortholog-secondary-interactions))
      (count-is 0 ortholog-secondary-interactions)))))

(deftest test-get-orthologs-secondary-interactions
 (binding [*print-level* 3]
  (let [orthologs-direct-interactions
        (network/get-orthologs-direct-interactions
          clients other-organisms proteins)]
    (let [orthologs-secondary-interactions
            (network/get-orthologs-secondary-interactions
              clients orthologs-direct-interactions)]
      ;(println orthologs-secondary-interactions)
      (is (s/valid? ::intr/interactions
            orthologs-secondary-interactions))
      (count-is 38 orthologs-secondary-interactions)))))

(deftest test-merge-proteins-and-get-secondary-interactions
 (binding [*print-level* 2]
  (let [direct-interactions
          (fetch/get-direct-interactions
            clients ref-organism proteins)
        orthologs-direct-interactions
          (network/get-orthologs-direct-interactions
            clients other-organisms proteins)]
    (let [[return-proteins secondary-interactions]
          (network/merge-proteins-and-get-secondary-interactions
            clients ref-organism proteins
            direct-interactions orthologs-direct-interactions)]
      ;(println return-proteins)
      ;(println secondary-interactions)
      (is (s/valid? ::prot/proteins
            return-proteins))
      (is (s/valid? ::intr/interactions
            secondary-interactions))
      (count-is 49 secondary-interactions)))))

(comment

  (binding [*print-level* 4]
    (let [dbs ["IntAct"]
          ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans")
          proteins [;(prot/->Protein ref-organism "Q18688")
                    (prot/->Protein ref-organism "Q20646")]
          other-organisms [(orgn/inparanoid-organism-by-shortname "M.musculus")
                           (orgn/inparanoid-organism-by-shortname "S.pombe")]]

      (let [[direct-interactions secondary-interactions return-proteins
             orthologs-secondary-interactions]
            (network/fetch-protein-network
                  dbs ; PSICQUIC databases to query
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
