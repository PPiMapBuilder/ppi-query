(ns ppi-query.interaction.miql-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [ppi-query.interaction.miql :as miql]))

(deftest test-valid-miql-specs
  (are [query] (is (s/valid? ::miql/query query) (s/explain-str ::miql/query query))
    ; single term
    "A"
    ; term combinaision
    [:and "A"], [:or "A" "B"]
    ; single field
    [:id "A"]
    ; fields and terms combinaision
    [:and [:id "A"] "B"], [:taxidA [:or "A" "B"]]
    ; nested queries of fields and terms
    [:and [:id [:or "A" "B"]] "C" [:and "D" [:species "E"]]]))

(deftest test-invalid-miql-specs
  (are [query] (is (not (s/valid? ::miql/query query)) (s/conform ::miql/query query))
    nil, [], :A
    ; multiple field value without specifying an operand ("or" or "and")
    [:id "A" "B"]))
