(ns ppi-query.protein-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein.uniprot :as uni]))

(deftest test-uniprotid-spec
  (are-spec ::uni/uniprotid
    :valid ["P04040-1"
            "P9WIE5-PRO_007"
            "P24270"]
    :invalid ["fooP04040"
              ""
              nil]))

(deftest test-strict-uniprotid-spec
  (are-spec ::uni/uniprotid-strict
    :valid ["P04040"
            "P9WIE5"
            "P24270"]

    :invalid ["fooP04040"
              ""
              nil
              "P04040-1"
              "P9WIE5-PRO_007"]))

(deftest test-get-strict-uniprotid
  (check' `uni/get-strict-uniprotid))
