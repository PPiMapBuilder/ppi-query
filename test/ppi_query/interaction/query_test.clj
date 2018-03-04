(ns ppi-query.interaction.query-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.query :as intrq]
            [ppi-query.interaction.transform :as intrt]
            [ppi-query.interaction.psicquic.registry :as reg]
            [proto-repl-charts.graph :as g])
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader)))

(stest/instrument)


(def interactions
  '({:interactorA {:organism {:identifiers (), :taxid "9606"}
                   :identifiers ({:database "uniprotkb", :identifier "A"})}
     :interactorB {:organism {:identifiers (), :taxid "9606"},
                   :identifiers ({:database "uniprotkb", :identifier "B"})}}
    {:interactorA {:organism {:identifiers (), :taxid "9606"}
                   :identifiers ({:database "uniprotkb", :identifier "A"})}
     :interactorB {:organism {:identifiers (), :taxid "9606"},
                   :identifiers ({:database "uniprotkb", :identifier "C"})}}))


(defn create-fn-throw-until-nth-call [nth-call]
  "Create a function throwing exception until the nth-call"
  (let [call-nb (atom 0)]
    (fn [& _]
      (when (< @call-nb nth-call)
        (swap! call-nb inc)
        (throw (RuntimeException.))))))


(defn create-stub-client
  "Stub PsicquicSimpleClient that throws exception until called enough"
  [required-retry count]
  (let [stub (create-fn-throw-until-nth-call required-retry)]
    (proxy [PsicquicSimpleClient] [""]
      (countByQuery [& _] (stub) count)
      (getByQuery [& _] (stub) nil))))


(defn create-stub-reader [ret]
  (proxy [PsimiTabReader] []
    (iterate [_] (.iterator ret))))


(deftest stub-test-count-by-query-retry
  (testing "Should succeed with no retry"
    (let [count 10
          client (create-stub-client 0 count)]
      (is (= count
             (intrq/count-by-query client "")))))

  (testing "Should succeed with 2 retries"
    (let [count 10
          client (create-stub-client 2 count)]
      (is (= count
             (intrq/count-by-query client "")))))

  (testing "Should fail with 4 or more retries"
    (let [count 10
          client (create-stub-client 4 count)]
      (is (= '()
             (intrq/count-by-query client ""))))))


(deftest stub-test-fetch-by-query
  (testing "Should call count by query and get by query to paginate"
    (let [client        (reg/get-client "IntAct")
          page-size     100
          total-count   (inc page-size)
          nb-call-count (atom 0)
          nb-call-get   (atom 0)
          stub-count    (fn [& _] (swap! nb-call-count inc) total-count)
          stub-get      (fn [& _] (swap! nb-call-get inc) interactions)]
      (with-redefs [intrq/count-by-query stub-count
                    intrq/get-by-query   stub-get]
        (is (= (concat interactions interactions)
               (intrq/fetch-by-query client "" page-size)))
        (is (= 1 @nb-call-count))
        (is (= 2 @nb-call-get))))))


(deftest test-stub-lazy-get-by-query
  (testing "Should succeed if no retry needed"
    (let [; stub Psicquic simple client
          client (create-stub-client 0 10)
          ; stub Psi mitab reader
          reader (create-stub-reader interactions)]
      (with-redefs [intrq/reader reader]
        (is (= interactions
               (intrq/lazy-get-by-query client ""))))))

  (testing "Should succeed with retries"
    (let [; stub Psicquic simple client
          client (create-stub-client 3 10)
          ; stub Psi mitab reader
          reader (create-stub-reader interactions)]
      (with-redefs [intrq/reader reader]
        (is (= interactions
               (intrq/lazy-get-by-query client ""))))))

  (testing "Should fail with 4 or more retries needed"
    (let [; stub Psicquic simple client
          client (create-stub-client 4 10)
          ; stub Psi mitab reader
          reader (create-stub-reader interactions)]
      (with-redefs [intrq/reader reader]
        (is (= '()
               (intrq/lazy-get-by-query client "")))))))


; -----------------
; Integration tests
; -----------------


(deftest ^:integration integration-test-count-by-query
  (testing "Should count Intact interactions by query"
    (let [intact (reg/get-client "IntAct")
          count (intrq/count-by-query intact "P04040")]
      (is (s/valid? int? count))
      (is (< 0 count))))

  (testing "Should count Intact search interactions by incorrect query"
    (let [intact (reg/get-client "IntAct")
          count (intrq/count-by-query intact "&!]=")]
      (is (= 0 count)))))


(deftest ^:integration integration-test-lazy-get-by-query
  (testing "Should succeed get lazy sequence of interaction by query
            with retry if needed."
    (let [page-size 10
          intact    (reg/get-client "IntAct")
          ints      (intrq/lazy-get-by-query intact "P04040 or Q14145"
                       :page-offset 0 :page-size page-size)]
      (is (not (realized? ints)))
      (is (= page-size (count ints)))
      (is (realized? ints)))))


(deftest ^:integration integration-test-get-by-query
  (testing "Should search IntAct interactions by query"
    (let [intact (reg/get-client "IntAct")
          result (intrq/get-by-query intact "P04040" 10 0)]

      (is (s/valid? ::intrd/interactions result))
      (is (< 0 (count result)))))

  (testing "Should ignore search IntAct interactions by incorrect query"
    (let [intact (reg/get-client "IntAct")
                 result (intrq/get-by-query intact "&!]=" 10 0)]

      (is (s/valid? ::intrd/interactions result))
      (is (= '() result))
      (count-is 0 result))))


(comment
  (defn get-edges [interactions]
    (->> interactions
         (map intrt/get-interactors-uniprotids)
         (remove #(some nil? %))))

  ; Create a very simple graph directly from the query
  (let [client (reg/get-client "IntAct")
        query "P04040 or Q14145"
        ;query  (miql/to-miql (miql/get-query-by-taxon 6239))
        interactions (intrq/fetch-by-query client query)
        edges (get-edges interactions)
        nodes-simple (remove nil? (into #{} (flatten edges)))
        nodes (map #(hash-map :id % :label % :group (- (int (first %)) (int \A)))
                   nodes-simple)]
    (g/graph
      "Psicquic interactions"
      {:nodes nodes :edges edges})))

