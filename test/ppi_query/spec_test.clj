(ns ppi-query.spec-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [ppi-query.test.utils :refer :all]
            [ppi-query.spec :refer [def-lucene-syntax]]))

; Generate lucene-like query syntax specs with two possible field search
(def-lucene-syntax ::query #{:field1 :field2})

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

(s/fdef char-range-set
  :args (s/cat :start char? :end char?)
  :ret (s/coll-of char? :distinct true)
  :fn #(some #{(-> % :args :start) (-> % :args :end)} (:ret %)))
