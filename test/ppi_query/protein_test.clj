(ns ppi-query.protein-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein.uniprot :as uni]))


(deftest* test-uniprot
  (testing "Uniprot id spec"
    (are-spec ::uni/uniprotid
      :valid ["P04040-1"
              "P9WIE5-PRO_007"
              "P24270"]
      :invalid ["fooP04040"
                ""
                nil]))

  (testing "Uniprot id strict spec"
    (are-spec ::uni/uniprotid-strict
      :valid ["P04040"
              "P9WIE5"
              "P24270"]

      :invalid ["fooP04040"
                ""
                nil
                "P04040-1"
                "P9WIE5-PRO_007"]))

  (testing "Test check get strict uniprot id"
    (check' `uni/get-strict-uniprotid)))
