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
    (s/valid? ::orthd/ortholog-scored-protein (first orths)))
  (let [orths (orth/get-best-orthologs human eleg1)]
    (count-is 1 orths)
    (s/valid? ::orthd/ortholog-scored-protein (first orths)))
  (let [orths (orth/get-best-orthologs human eleg2)]
    (count-is 1 orths)
    (s/valid? ::orthd/ortholog-scored-protein (first orths)))
  (let [orths (orth/get-best-orthologs melanogaster melag1)]
    (count-is 1 orths)
    (is (s/valid? ::orthd/ortholog-scored-protein (first orths)))))
