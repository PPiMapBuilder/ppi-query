(ns ppi-query.orthology-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd]))

(def dbs ["IntAct"])
(def elegans (orgn/inparanoid-organism-by-shortname "C.elegans"))
(def human (orgn/inparanoid-organism-by-id 9606))
(def melanogaster (orgn/inparanoid-organism-by-id 7227))
(def eleg1 (prot/->Protein elegans "Q18688"))
(def eleg2 (prot/->Protein elegans "Q20646"))
(def melag1 (prot/->Protein melanogaster "Q9W1K2"))
(def catalase (prot/->Protein human "P04040"))

(deftest test-get-best-orthologs
  (let [orths (orth/get-best-orthologs elegans catalase)]
    (count-is 1 orths)
    (is (s/valid? ::orthd/ortholog-scored-protein (first orths))))
  (let [orths (orth/get-best-orthologs human eleg1)]
    (count-is 1 orths)
    (is (s/valid? ::orthd/ortholog-scored-protein (first orths))))
  (let [orths (orth/get-best-orthologs human eleg2)]
    ; This protein isn't in:
    ; http://inparanoid.sbc.su.se/download/8.0_current/Orthologs_OrthoXML/C.elegans/C.elegans-H.sapiens.orthoXML
    (count-is 0 orths))
  (let [orths (orth/get-best-orthologs melanogaster melag1)]
    (count-is 1 orths)
    (is (s/valid? ::orthd/ortholog-scored-protein (first orths)))))

(comment ;generate-cache-species
  (let [orgs orgn/inparanoid-organism-repository
        org-pairs
        (for [org1 orgs org2 orgs :when (not= org1 org2)]
            [org1 org2])]

    (map (fn [[org1 org2]] (orth/get-ortholog-species-pair org1 org2))
         (shuffle org-pairs))))
