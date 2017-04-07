(ns ppi-query.spec-test
  (:require [clojure.test :refer :all]
            [ppi-query.test.utils :refer :all]
            [ppi-query.spec :as ps]))

; Generate lucene-like query syntax specs with two possible field search
(ps/def-lucene-syntax ::query #{:field1 :field2})

; Check query spec with a list of valid and invalid examples
(deftest test-query-specs
  (are-spec ::query
    :valid [; single term
            "A"
            ; term combination
            [:and "A"], [:or "A" "B"]
            ; single field
            [:field1 "A"]
            ; fields and terms combination
            [:and [:field2 "A"] "B"], [:field1 [:or "A" "B"]]
            ; nested queries of fields and terms
            [:and [:field1 [:or "A" "B"]] "C" [:and "D" [:field2 "E"]]]]

    :invalid [; nil, empty seq or keywords
              nil, [], :A
              ; multiple field value without specifying an
              ; operand ("or" or "and")
              [:field1 "A" "B"]
              ; invalid field name
              [:foo "A"]]))

(deftest test-fixed-size-s-repeat-spec
  (are-spec (ps/s-repeat int? 10)

    :valid [(range 10)
            [1 2 2 4 1 4 5 5 2 4]]

    :invalid [(repeat 10 nil)
              (range 3)]))

(deftest test-ranged-size-s-repeat-spec
  (are-spec (ps/s-repeat int? 5 10)

    :valid [(range 10)
            (range 5)
            (repeatedly 7 #(rand-int 99))]

    :invalid [(repeat 6 nil)
              (range 3)
              (range 11)]))
