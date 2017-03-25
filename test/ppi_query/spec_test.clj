(ns ppi-query.spec-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.spec :as ps]))

; Generate lucene-like query syntax specs with two possible field search
(ps/def-lucene-syntax ::query #{:field1 :field2})

; Check that a list of examples does conforms to the generated
; lucene-like query syntax spec
(deftest test-valid-specs
  (are-valid ::query
    ; single term
    "A"
    ; term combination
    [:and "A"], [:or "A" "B"]
    ; single field
    [:field1 "A"]
    ; fields and terms combination
    [:and [:field2 "A"] "B"], [:field1 [:or "A" "B"]]
    ; nested queries of fields and terms
    [:and [:field1 [:or "A" "B"]] "C" [:and "D" [:field2 "E"]]]))

; Check that a list of examples does not conform to the generated
; lucene-like query syntax spec
(deftest test-invalid-miql-specs
  (are-invalid ::query
    nil, [], :A
    ; multiple field value without specifying an operand ("or" or "and")
    [:field1 "A" "B"]
    ; invalid field name
    [:foo "A"]))

(deftest test-valid-s-repeat-size
  (are-valid (ps/s-repeat int? 10)
    (range 10)
    [1 2 2 4 1 4 5 5 2 4]))

(deftest test-invalid-s-repeat-size
  (are-invalid (ps/s-repeat int? 4)
    [nil nil nil nil]
    (range 3)))

(deftest test-valid-s-repeat-range
  (are-valid (ps/s-repeat int? 5 10)
    (range 10)
    (range 7)
    (range 5)))

(deftest test-invalid-s-repeat-range
  (are-invalid (ps/s-repeat int? 4 7)
    (range 3)
    (range 8)))

#_
(defspec-test test-functions
  (stest/enumerate-namespace `ppi-query.spec))
