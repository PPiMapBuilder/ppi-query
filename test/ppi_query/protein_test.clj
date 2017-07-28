(ns ppi-query.protein-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
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
;(defspec-test test-uniprot
;  (stest/enumerate-namespace `ppi-query.protein.uniprot))

;(gen/sample (s/gen ::uni/uniprotid))
;(s/exercise-fn `ppi-query.protein.uniprot/get-strict-uniprotid)
;(stest/instrument `ppi-query.protein.uniprot/get-strict-uniprotid)
(uni/get-strict-uniprotid "Q093M6.Zl")
;(defspec-test test-uniprot-1
;  [`ppi-query.protein.uniprot/get-strict-uniprotid])

(defspec-test test-uniprot-2
  [`ppi-query.protein.uniprot/uniprotid-regexp])

(defspec-test test-uniprot-3
  [`ppi-query.protein.uniprot/seq-to-str])

(defspec-test test-uniprot-4
  [`ppi-query.protein.uniprot/str-to-seq])

;(defspec-test test-uniprot-all
;  #{`ppi-query.protein.uniprot/get-strict-uniprotid
;    `ppi-query.protein.uniprot/uniprotid-regexp
;    `ppi-query.protein.uniprot/seq-to-str
;    `ppi-query.protein.uniprot/str-to-seq])
