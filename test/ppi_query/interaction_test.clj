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
