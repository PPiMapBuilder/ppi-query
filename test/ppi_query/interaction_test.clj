(ns ppi-query.interaction-test
  (:require [clojure.test :refer :all]
            [ppi-query.interaction :refer :all]))

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
        query  "P04040 or Q14145"
        interactions (get-by-query client query)
        edges (get-edges interactions)
        nodes (into #{} (flatten edges))]

    (g/graph
      "Psicquic interactions"
      {:nodes nodes :edges edges})))

; Interactome Tests
(deftest test-get-query-by-taxon
   (is (=
          (get-query-by-taxon 9606)
          [:and [:taxidA 9606] [:taxidB 9606] [:species 9606]])))

(deftest test-to-miql
  (is (= (to-miql [:key1 "value"])
         "key1:value"))
  (is (= (to-miql [:and [:key1 "value"] [:or [:key2 "value2"] [:key3 "value3"]]])
         "(key1:value and (key2:value2 or key3:value3))"))
  (is (not (= (to-miql [:and [:key1 "value"] [:or [:key2 "value2"] [:key3 "value3"]]])
              "key1:value and (key2:value2 or key3:value3)"))))
