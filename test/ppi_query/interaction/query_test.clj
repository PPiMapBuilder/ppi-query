(ns ppi-query.interaction.query-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.query :as intrq]
            [ppi-query.interaction.transform :as intrt]
            [ppi-query.interaction.miql :as miql]
            [proto-repl-charts.graph :as g]))
(stest/instrument)

(defn get-edges [interactions]
  (->> interactions
    (map intrt/get-interactors-uniprotids)
    (remove #(some nil? %))))

(comment
  ; Create a very simple graph directly from the query
  (let [client (first intrq/registry-clients)
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
; See /interaction_example.txt for a full interaction
(def test-client-1 (first intrq/registry-clients))
(def test-query-1-res (intrq/get-by-query test-client-1 test-query-1))
(def do-test-query-2 false) ; Warning: Long !
(def test-query-2-res
   (if do-test-query-2
       (intrq/get-by-query test-client-1 test-query-2)))
(def test-query-3-res '())

(deftest test-get-by-query
  ;(println test-query-1-res)
  (is (s/valid? ::intrd/interactions test-query-1-res))
  (count-is 282 test-query-1-res)

  (when do-test-query-2
    (is (s/valid? ::intrd/interactions test-query-2-res))
    (count-is 282 test-query-1-res))

  (is (s/valid? ::intrd/interactions test-query-3-res))
  (is (= (intrq/get-by-query test-client-1 test-query-3)
         test-query-3-res))
  (count-is 0 test-query-3-res))

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
            test-client-1 test-query-3 10))))

(deftest test-fetch-by-query-all-clients
  (is (= test-query-1-res
         (intrq/fetch-by-query-all-clients
            intrq/registry-clients test-query-1)))

  (when do-test-query-2
    (is (= test-query-2-res
           (intrq/fetch-by-query-all-clients
              intrq/registry-clients test-query-2))))

  (is (= test-query-3-res
         (intrq/fetch-by-query-all-clients
            intrq/registry-clients test-query-3))))