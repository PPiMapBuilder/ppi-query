(ns ppi-query.network-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.transform :as intrt]
            [ppi-query.fetch :as fetch]
            [ppi-query.network :as network]))
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


(deftest test-get-ortholog-direct-interactions
 (binding [*print-level* 2]
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-1 [protein-1])]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-scored-proteins ortholog-prots
     ::intrd/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog other-organism-1))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P11499" (:uniprotid (first ortholog-prots))))
   (count-is 18 orth-dir-interactions))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-2 [protein-1])]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-scored-proteins ortholog-prots
     ::intrd/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog other-organism-2))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P41887" (:uniprotid (first ortholog-prots))))
   (count-is 0 orth-dir-interactions))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-1 [protein-2])]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-scored-proteins ortholog-prots
     ::intrd/interactions orth-dir-interactions)
   (count-is 0 ortholog-prots))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-2 [protein-2])]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-scored-proteins ortholog-prots
     ::intrd/interactions orth-dir-interactions)
   (count-is 0 ortholog-prots))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-1 proteins)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-scored-proteins ortholog-prots
     ::intrd/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog other-organism-1))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P11499" (:uniprotid (first ortholog-prots))))
   (count-is 18 orth-dir-interactions))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-2 proteins)]
   ;(println ortholog ortholog-prots orth-dir-interactions)
   (are [fmt res] (s/valid? fmt res)
     ::orgn/organism ortholog
     ::orthd/ortholog-scored-proteins ortholog-prots
     ::intrd/interactions orth-dir-interactions)
   (count-is 1 ortholog-prots)
   (is (= ortholog other-organism-2))
   (is (= ortholog (:organism (first ortholog-prots))))
   (is (= "P41887" (:uniprotid (first ortholog-prots))))
   (count-is 0 orth-dir-interactions))))

(deftest test-get-orthologs-direct-interactions
 (binding [*print-level* 3]
  (let [orthologs-direct-interactions
          (network/get-orthologs-direct-interactions
            clients [other-organism-1] proteins)]
    ;(println orthologs-direct-interactions)
    (is (s/valid?
          (s/coll-of
            (s/cat :ortholog       ::orgn/organism
                   :ortholog-prots ::orthd/ortholog-scored-proteins
                   :interactions   ::intrd/interactions))
          orthologs-direct-interactions))
    (count-is 1 orthologs-direct-interactions))
  (let [orthologs-direct-interactions
          (network/get-orthologs-direct-interactions
            clients [other-organism-2] proteins)]
    ;(println orthologs-direct-interactions)
    (is (s/valid?
          (s/coll-of
            (s/cat :ortholog       ::orgn/organism
                   :ortholog-prots ::orthd/ortholog-scored-proteins
                   :interactions   ::intrd/interactions))
          orthologs-direct-interactions))
    (count-is 1 orthologs-direct-interactions))
  (let [orthologs-direct-interactions
          (network/get-orthologs-direct-interactions
            clients other-organisms proteins)]
    ;(println orthologs-direct-interactions)
    (is (s/valid?
          (s/coll-of
            (s/cat :ortholog       ::orgn/organism
                   :ortholog-prots ::orthd/ortholog-scored-proteins
                   :interactions   ::intrd/interactions))
          orthologs-direct-interactions))
    (count-is 2 orthologs-direct-interactions))))

(deftest test-get-ortholog-secondary-interactions
 (binding [*print-level* 3]
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-1 proteins)]
    (let [ortholog-secondary-interactions
            (network/get-ortholog-secondary-interactions
              clients ortholog ortholog-prots orth-dir-interactions)]
      ;(println ortholog-secondary-interactions)
      (is (s/valid? ::intrd/interactions
            ortholog-secondary-interactions))
      (count-is 38 ortholog-secondary-interactions)))
  (let [[ortholog ortholog-prots orth-dir-interactions]
        (network/get-ortholog-direct-interactions
          clients other-organism-2 proteins)]
    (let [ortholog-secondary-interactions
            (network/get-ortholog-secondary-interactions
              clients ortholog ortholog-prots orth-dir-interactions)]
      ;(println ortholog-secondary-interactions)
      (is (s/valid? ::intrd/interactions
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
      (is (s/valid? ::intrd/interactions
            orthologs-secondary-interactions))
      (count-is 38 orthologs-secondary-interactions)
      (let [all-interactions
              (network/merge-orthologs-direct-secondary-interactions
                orthologs-direct-interactions
                orthologs-secondary-interactions)
            proteins-interactions
              (intrt/interactions->proteins-interactions
                all-interactions)
            first-interaction
              (first proteins-interactions)]
        (is (s/valid? ::intrd/interactions
              all-interactions))
        (is (s/valid? ::prot/protein (:protein-a first-interaction)))
        (is (s/valid? ::prot/protein (:protein-b first-interaction)))
        (is (s/valid? ::intrd/interaction (:original-interaction first-interaction)))
        (is (s/valid? ::intrd/proteins-interaction first-interaction))
        (is (s/valid? ::intrd/proteins-interactions proteins-interactions))
        (count-is 56 all-interactions)
        (count-is 56 proteins-interactions)
        ;(println proteins-interactions))))))
        (let [orthologs-interactions-ref-organism
              (network/orthologs-interactions->ref-organism
                ref-organism
                orthologs-direct-interactions
                orthologs-secondary-interactions)]
          ;(println orthologs-interactions-ref-organism)
          (is (s/valid? ::intrd/prot-orths-interactions orthologs-interactions-ref-organism))
          (count-is 43 orthologs-interactions-ref-organism)))))))

(deftest test-merge-proteins-and-get-secondary-interactions
 (binding [*print-level* 3]
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
      (is (s/valid? ::intrd/interactions
            secondary-interactions))
      (count-is 49 secondary-interactions)))))

(deftest test-fetch-protein-network
 (binding [*print-level* 3]
  (let [[ret-proteins ret-interactions]
        (network/fetch-protein-network
           dbs ; PSICQUIC databases to query
           ref-organism  ; Organism of Interest
           proteins ; Proteins of Interest
           other-organisms)] ; Other Organisms to check
   (is (s/valid? ::prot/proteins ret-proteins))
   (is (s/valid? ::intrd/prot-orths-multi-interactions ret-interactions)))))
