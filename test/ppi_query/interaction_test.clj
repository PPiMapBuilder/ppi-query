(ns ppi-query.interaction-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.interaction :as intr]
            [ppi-query.interaction.miql :as miql]))
(stest/instrument)

(defn get-edges [interactions]
  (->> interactions
    (map intr/get-interactors-uniprotids)
    (remove #(contains? % nil?))))

(require '[proto-repl-charts.graph :as g])

(comment

  (let [client (first intr/registry-clients)
        ;query "P04040 or Q14145"
        query  (miql/to-miql (miql/get-query-by-taxon 6239))
        interactions (intr/fetch-by-query client query)
        edges (get-edges interactions)
        nodes-simple (remove nil? (into #{} (flatten edges)))
        nodes (map #(hash-map :id % :label % :group (- (int (first %)) (int \A)))
               nodes-simple)]
    ;(take 10 nodes)

    (g/graph
      "Psicquic interactions"
      {:nodes nodes :edges edges})))

(comment)
(deftest test-fetch-by-query
  (let [client (first intr/registry-clients)
        query  "P04040 or Q14145"
        getby  (intr/get-by-query client query)]
    (is (= getby
           (intr/fetch-by-query client query)))
    (is (= getby
           (intr/fetch-by-query client query 50)))))
