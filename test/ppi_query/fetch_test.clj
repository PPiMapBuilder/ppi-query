(ns ppi-query.fetch-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.interaction :as intr]
            [ppi-query.fetch :as fetch]))

(stest/instrument)

(def databases ["IntAct"])
(def clients (fetch/get-clients databases))
(def ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans"))
(def proteins [(prot/->Protein ref-organism "Q18688")
               (prot/->Protein ref-organism "Q20646")])
(def other-organisms [(orgn/inparanoid-organism-by-shortname "M.musculus")
                      (orgn/inparanoid-organism-by-shortname "S.pombe")])

(comment
 (binding [*print-level* 3]
  (let [res (fetch/get-proteins-orthologs ref-organism proteins)]
    (println res
      "\n Valid ? \n"
      (s/valid? ::orthd/ortholog-scored-proteins
        res)))

  (let [res (fetch/get-proteins-orthologs (first other-organisms) proteins)]
    (println res
      "\n Valid ? \n"
      (s/valid? ::orthd/ortholog-scored-proteins
        res)))

  (let [res (fetch/get-proteins-orthologs (second other-organisms) proteins)]
    (println res
      "\n Valid ? \n"
      (s/valid? ::orthd/ortholog-scored-proteins
        res)))

  (let [res (fetch/get-secondary-interactions
                   clients ref-organism proteins)]
    (println res
      "\n Valid ? \n"
      (s/valid? ::intr/interactions
        res)))))

(deftest test-get-direct-interactions
  (let [direct-interactions
        (fetch/get-direct-interactions
           clients ref-organism proteins)]
    (is (s/valid? ::intr/interactions
          direct-interactions))
    (is (= 31 (count direct-interactions)))))

(deftest test-get-proteins-orthologs
  (is (empty? (fetch/get-proteins-orthologs ref-organism proteins)))
  (is (s/valid? ::orthd/ortholog-scored-proteins
                (fetch/get-proteins-orthologs (first other-organisms) proteins)))
  (is (s/valid? ::orthd/ortholog-scored-proteins
                (fetch/get-proteins-orthologs (second other-organisms) proteins))))

(deftest test-get-secondary-interactions
  (is (s/valid? ::intr/interactions
                (fetch/get-secondary-interactions
                   clients ref-organism proteins))))
