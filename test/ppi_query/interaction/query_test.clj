(ns ppi-query.interaction.query-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.query :as intrq]
            [ppi-query.interaction.transform :as intrt]
            [ppi-query.interaction.miql :as miql]
            [ppi-query.interaction.psicquic.registry :as reg]
            [proto-repl-charts.graph :as g]))
(stest/instrument)

(defn get-edges [interactions]
  (->> interactions
    (map intrt/get-interactors-uniprotids)
    (remove #(some nil? %))))

(comment
  ; Create a very simple graph directly from the query
  (let [client (reg/get-client "IntAct")
        query "P04040 or Q14145"
        ;query  (miql/to-miql (miql/get-query-by-taxon 6239))
        interactions (intrq/fetch-by-query client query)
        edges (get-edges interactions)
        nodes-simple (remove nil? (into #{} (flatten edges)))
        nodes (map #(hash-map :id % :label % :group (- (int (first %)) (int \A)))
               nodes-simple)]
    ;(take 10 nodes)

    (g/graph
      "Psicquic interactions"
      {:nodes nodes :edges edges})))

(def test-query-1 "P04040 or Q14145")
(def test-query-2 "taxidA:6239 AND taxidB:6239 AND species:6239")
(def test-query-3 "&!]=")
(def test-query-4 " ( taxidA:9606 AND taxidB:9606 AND species:9606 AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" ) AND  ( id:\"P04040\" )  ) ")
; See /interaction_example.txt for a full interaction
(def test-client-1 (reg/get-client "IntAct"))
(def test-all-clients (reg/get-clients ["IntAct"]))
(def test-query-1-res (intrq/get-by-query test-client-1 test-query-1))
(def do-test-query-2 false) ; Warning: Long !
(def test-query-2-res
   (if do-test-query-2
       (intrq/get-by-query test-client-1 test-query-2)))
(def test-query-3-res '())
(def test-query-4-res (intrq/get-by-query test-client-1 test-query-4))

(deftest test-get-by-query
  ;(println test-query-1-res)
  (is (s/valid? ::intrd/interactions test-query-1-res))
  (count-is 284 test-query-1-res)

  (when do-test-query-2
    (is (s/valid? ::intrd/interactions test-query-2-res))
    (count-is 284 test-query-1-res))

  (is (s/valid? ::intrd/interactions test-query-3-res))
  (is (= (intrq/get-by-query test-client-1 test-query-3)
         test-query-3-res))
  (count-is 0 test-query-3-res)

  (is (s/valid? ::intrd/interactions test-query-4-res))
  (is (= (intrq/get-by-query test-client-1 test-query-4)
         test-query-4-res))
  (count-is 13 test-query-4-res))

(deftest test-fetch-by-query
  (is (= test-query-1-res
         (intrq/fetch-by-query
            test-client-1 test-query-1)))
  (is (= test-query-1-res
         (intrq/fetch-by-query
            test-client-1 test-query-1 10)))

  (when do-test-query-2
    (is (= test-query-2-res
           (intrq/fetch-by-query
              test-client-1 test-query-2)))
    (is (= test-query-2-res
           (intrq/fetch-by-query
              test-client-1 test-query-2 10))))

  (is (= test-query-3-res
         (intrq/fetch-by-query
            test-client-1 test-query-3)))
  (is (= test-query-3-res
         (intrq/fetch-by-query
            test-client-1 test-query-3 10)))

  (is (= test-query-4-res
         (intrq/fetch-by-query
            test-client-1 test-query-4)))
  (is (= test-query-4-res
         (intrq/fetch-by-query
            test-client-1 test-query-4 10))))

(deftest test-fetch-by-query-all-clients
  (is (= test-query-1-res
         (intrq/fetch-by-query-all-clients
            test-all-clients test-query-1)))

  (when do-test-query-2
    (is (= test-query-2-res
           (intrq/fetch-by-query-all-clients
              test-all-clients test-query-2))))

  (is (= test-query-3-res
         (intrq/fetch-by-query-all-clients
            test-all-clients test-query-3))))
