(ns ppi-query.fetch-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.interaction :as intr]
            [ppi-query.fetch :as fetch]))

(comment
  (binding [*print-level* 4]
    (let [databases ["IntAct"]
          ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans")
          proteins [(prot/->Protein ref-organism "Q18688")
                    (prot/->Protein ref-organism "Q20646")]
          other-organisms [(orgn/inparanoid-organism-by-shortname "M.musculus")
                           (orgn/inparanoid-organism-by-shortname "S.pombe")]]

      (let [clients (fetch/get-clients databases)]
        ;(s/valid? ::intr/interactions
        ;  (fetch/get-direct-interactions
        ;            clients ref-organism proteins]

        (let [res (fetch/get-secondary-interactions
                         clients ref-organism proteins)]
          (println res)
          (s/valid? ::intr/interactions
            res))))))
