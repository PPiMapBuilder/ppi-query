(ns ppi-query.orthology-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology :as orth]))


(comment
  (binding [*print-level* 4]
    (let [databases ["IntAct"]
          elegans (orgn/inparanoid-organism-by-shortname "C.elegans")
          human (orgn/inparanoid-organism-by-id 9606)
          eleg1 (prot/->Protein elegans "Q18688")
          eleg2 (prot/->Protein elegans "Q20646")
          catalase (prot/->Protein human "P04040")]
      (println
        elegans human
        catalase)
      (println "#######")
      (println
        (orth/get-best-orthologs elegans catalase)
        (orth/get-best-orthologs human eleg1)
        (orth/get-best-orthologs human eleg2)))))
