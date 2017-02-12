(ns ppi-query.protein-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein.uniprot :as uni]))
(stest/instrument)

(s/fdef uni/get-strict-uniprotid
  :args (s/cat :id ::uni/uniprotid)
  :ret ::uni/uniprotid-strict)
(check `uni/get-strict-uniprotid)

(deftest test-extract-strcti-uniprotid
  (is (= (uni/get-strict-uniprotid "P04040-1")
         "P04040")))

(deftest test-valid-uniprotid-specs
  (are-valid ::uni/uniprotid
    "P04040-1"
    "P04040"))
