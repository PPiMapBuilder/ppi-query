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
          ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans")
          proteins [(prot/->Protein ref-organism "Q18688")
                    (prot/->Protein ref-organism "Q20646")]]
      (println
        ref-organism
        (first proteins))
      (println
        (orth/get-best-orthologs ref-organism (first proteins))
        (orth/get-best-orthologs ref-organism (second proteins))))))
