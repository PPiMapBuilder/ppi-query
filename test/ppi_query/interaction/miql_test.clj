(ns ppi-query.interaction-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.interaction.miql :refer :all]))
(stest/instrument)

; Interactome Tests
(s/exercise-fn `get-query-by-taxon)
(check `get-query-by-taxon)
(deftest test-get-query-by-taxon
   (is (=
          (get-query-by-taxon 9606)
          [:and [:taxidA 9606] [:taxidB 9606] [:species 9606]])))

(s/exercise-fn `get-query-by-taxon-and-prots)
(check `get-query-by-taxon-and-prots)
(deftest test-get-query-by-taxon-and-prots
   (is (=
          (get-query-by-taxon-and-prots 9606 "P04040")
          [:and [:taxidA 9606] [:taxidB 9606] [:species 9606]
                [:or [:id "P04040"]]]))
   (is (=
          (get-query-by-taxon-and-prots 9606 "P04040" "O64HD2" "J1D0B7FO54")
          [:and [:taxidA 9606] [:taxidB 9606] [:species 9606]
                [:or [:id "P04040"] [:id "O64HD2"] [:id "J1D0B7FO54"]]])))

(s/exercise-fn `get-queries-by-taxon-and-prot-couples)
(check 'get-queries-by-taxon-and-prot-couples)
(deftest test-get-queries-by-taxon-and-prot-couples
  (is (= (get-queries-by-taxon-and-prot-couples 9606 [["P04040" "Q9D2V5"] ["Q9D2V5" "P04040"]])
         [:and
              [:taxidA 9606]
              [:taxidB 9606]
              [:species 9606]
              [:or
                  [:and [:idA "P04040"] [:idB "Q9D2V5"]]
                  [:and [:idA "Q9D2V5"] [:idB "P04040"]]]])))

(s/exercise-fn `get-queries-by-taxon-and-prot-pool)
(check 'get-queries-by-taxon-and-prot-pool)
(deftest test-get-queries-by-taxon-and-prot-pool
  (is
   (=
    (get-queries-by-taxon-and-prot-pool 9606 ["P04040" "Q9D2V5"] 2)
    (list [:and
            [:taxidA 9606]
            [:taxidB 9606]
            [:species 9606]
            [:or
                [:and [:idA "P04040"] [:idB "P04040"]]
                [:and [:idA "P04040"] [:idB "Q9D2V5"]]]]
          [:and
            [:taxidA 9606]
            [:taxidB 9606]
            [:species 9606]
            [:or
                [:and [:idA "Q9D2V5"] [:idB "P04040"]]
                [:and [:idA "Q9D2V5"] [:idB "Q9D2V5"]]]]))))

;(s/exercise ::ppi-query.interaction.miql/query)]]))))
;(s/exercise-fn `to-miql)
;(check `to-miql)
(deftest test-to-miql
  (is (= (to-miql [:taxidA "value"])
         "taxidA:\"value\""))
  (is (= (to-miql [:taxidA "value with spaces"])
         "taxidA:\"value with spaces\""))
  (is (= (to-miql [:and [:taxidA "value"] [:or [:taxidB "value2"] [:species 10000]]])
         " ( taxidA:\"value\" AND  ( taxidB:\"value2\" OR species:10000 )  ) "))
  (is (not (= (to-miql [:and [:taxidA "value"] [:or [:taxidB "value2"] [:species 10000]]])
              "taxidA:\"value\" AND  ( taxidB:\"value2\" OR species:10000 ) ")))
  (is (= (to-miql [:and [:taxidA 9606] [:taxidB 9606] [:species 9606] [:or [:idA "P04040"] [:idB "P04040"]]])
         " ( taxidA:9606 AND taxidB:9606 AND species:9606 AND  ( idA:\"P04040\" OR idB:\"P04040\" )  ) "))
  (is (= (to-miql [:and [:taxidA 9606] [:taxidB 9606] [:species 9606] [:or [:id "P04040"]]])
         " ( taxidA:9606 AND taxidB:9606 AND species:9606 AND  ( id:\"P04040\" )  ) ")))
