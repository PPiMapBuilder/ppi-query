(ns ppi-query.protein-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein.uniprot :as uni]))

(deftest test-extract-strcti-uniprotid
  (is (= (uni/get-strict-uniprotid "P04040-1")
         "P04040")))

(deftest test-valid-uniprotid-specs
  (are-valid ::uni/uniprotid
    "P04040-1"
    "P04040"))

;; Generative testing on all functions of ppi-query.protein.uniprot
(defspec-test test-uniprot
  (stest/enumerate-namespace `ppi-query.protein.uniprot))
