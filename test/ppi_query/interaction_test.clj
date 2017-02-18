(ns ppi-query.interaction-test
  (:require [clojure.test :refer :all]
            [ppi-query.interaction :refer :all]
            [ppi-query.interaction.miql :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(defn trace [x]
  (println "trace" x)
  x)

(defn get-edges [interactions]
  (->> interactions
    (map get-interactors-uniprotids)
    (remove #(contains? % nil?))))

(require '[proto-repl-charts.graph :as g])

(comment

  (let [client (first registry-clients)
        ;query "P04040 or Q14145"
        query  (to-miql (get-query-by-taxon 6239))
        interactions (fetch-by-query client query)
        edges (get-edges interactions)
        nodes-simple (remove nil? (into #{} (flatten edges)))
        nodes (map #(hash-map :id % :label % :group (- (int (first %)) (int \A)))
               nodes-simple)]
    ;(take 10 nodes)

    (g/graph
      "Psicquic interactions"
      {:nodes nodes :edges edges})))

(comment
  (deftest test-fetch-by-query
    (let [client (first registry-clients)
          query  "P04040 or Q14145"
          getby  (get-by-query client query)]
      (is (= getby
             (fetch-by-query client query)))
      (is (= getby
             (fetch-by-query client query 50))))))

; Interactome Tests
(deftest test-get-query-by-taxon
   (is (=
          (get-query-by-taxon 9606)
          [:and [:taxidA 9606] [:taxidB 9606] [:species 9606]])))

(deftest test-get-query-by-taxon-and-prots
   (is (=
          (get-query-by-taxon-and-prots 9606 "P04040")
          [:and [:taxidA 9606] [:taxidB 9606] [:species 9606] [:or [:id "P04040"]]]))
   (is (=
          (get-query-by-taxon-and-prots 9606 "P04040" "QTOTO")
          [:and [:taxidA 9606] [:taxidB 9606] [:species 9606] [:or [:id "P04040"] [:id "QTOTO"]]])))

(s/exercise-fn `get-queries-by-taxon-and-prot-couples)
(stest/check 'get-queries-by-taxon-and-prot-couples)
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
(stest/check `get-queries-by-taxon-and-prot-pool)
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

(stest/instrument `to-miql)
"taxidA:value"
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
         "...")))
