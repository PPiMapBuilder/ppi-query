(ns ppi-query.protein-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer [are-valid are-invalid]]
            [ppi-query.protein.uniprot :refer :all]))

(stest/instrument `get-strict-uniprotid)
(stest/check `get-strict-uniprotid)
(deftest test-extract-strcti-uniprotid
  (is (= (get-strict-uniprotid "P04040-1")
         "P04040")))

(deftest test-valid-uniprotid-specs
  (are-valid ::ppi-query.protein.uniprot/uniprotid
    "P04040-1"
    "P04040"))
