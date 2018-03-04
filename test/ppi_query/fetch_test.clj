(ns ppi-query.fetch-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.core.async :as async]
            [ppi-query.test.utils :refer :all]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.query :as intrq]
            [ppi-query.fetch :as fetch]
            [ppi-query.interaction.psicquic.registry :as reg]
            [clojure.test.check.generators :as gen]))

(stest/instrument)

(defn consume [channel]
  (async/<!! (async/into [] channel)))

(deftest test-fetch-dbs-by-query
  (dotimes [nb-thread 3]
    (let [count-cbq (atom 0)
          count-lgbq (atom 0)
          count-gc (atom 0)
          gen-int (s/gen ::intrd/interaction)]
      (with-redefs [intrq/count-by-query (fn [& _] (swap! count-cbq inc) 103)
                    intrq/lazy-get-by-query (fn [& _]
                                              (swap! count-lgbq inc)
                                              (gen/sample gen-int 10))
                    reg/get-client (fn [& _] (swap! count-gc inc) nil)]
        (consume
          (fetch/fetch-dbs-by-query>
            ["DB1" "DB2"] "" :nb-thread nb-thread :page-size 100))
        (is (= 2 @count-gc))
        (is (= 2 @count-cbq))
        (is (= 4 @count-lgbq))))))

; -----------------
; Integration tests
; -----------------

(def databases ["IntAct"])
(def clients (reg/get-clients databases))
(def ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans"))
(def proteins [(prot/->Protein ref-organism "Q18688")
               (prot/->Protein ref-organism "Q20646")])
(def other-organisms [(orgn/inparanoid-organism-by-shortname "M.musculus")
                      (orgn/inparanoid-organism-by-shortname "S.pombe")])

(deftest ^:integration integration-test-fetch-dbs-by-query
  (testing "Number of threads or page size should not influence result"
    (let [res-thread2-size10
          (consume
            (fetch/fetch-dbs-by-query>
              ["IntAct" "InnateDB"] "P04040"
              :page-size 10 :nb-thread 3))
          res-thread1-size100
          (consume
            (fetch/fetch-dbs-by-query>
              ["IntAct" "InnateDB"] "P04040"
              :page-size 100 :nb-thread 1))]
      (is (= (count res-thread1-size100)
             (count res-thread2-size10))))))


(deftest ^:integration test-get-direct-interactions
  (let [direct-interactions
        (fetch/get-direct-interactions
          clients ref-organism proteins)]
    (is (s/valid? ::intrd/interactions direct-interactions))
    (is (= 31 (count direct-interactions)))))


(deftest ^:integration test-get-proteins-orthologs
  (is (not (empty? (fetch/get-proteins-orthologs ref-organism proteins))))
  (is (s/valid? ::orthd/ortholog-scored-proteins
                (fetch/get-proteins-orthologs (first other-organisms) proteins)))
  (is (s/valid? ::orthd/ortholog-scored-proteins
                (fetch/get-proteins-orthologs (second other-organisms) proteins))))


(deftest ^:integration test-get-secondary-interactions
  (is (s/valid? ::intrd/interactions
                (fetch/get-secondary-interactions
                  clients ref-organism proteins))))


(comment
  (binding [*print-level* 3]
    (let [res (fetch/get-proteins-orthologs ref-organism proteins)]
      (println res
               "\n Valid ? \n"
               (s/valid? ::orthd/ortholog-scored-proteins
                         res)))

    (let [res (fetch/get-proteins-orthologs (first other-organisms) proteins)]
      (println res
               "\n Valid ? \n"
               (s/valid? ::orthd/ortholog-scored-proteins
                         res)))

    (let [res (fetch/get-proteins-orthologs (second other-organisms) proteins)]
      (println res
               "\n Valid ? \n"
               (s/valid? ::orthd/ortholog-scored-proteins
                         res)))

    (let [res (fetch/get-secondary-interactions
                clients ref-organism proteins)]
      (println res
               "\n Valid ? \n"
               (s/valid? ::intrd/interactions
                         res))))

  (dotimes [_ 1]
    (time
      (->>
        (fetch/fetch-dbs-by-query> ["IntAct", "InnateDB"] "P04040")
        (consume)
        (map :source)
        (frequencies)
        (println))))

  (let [ints (fetch/fetch-dbs-by-query> ["IntAct", "InnateDB"] "P04040")
        ints' (async/chan)]
    (async/pipeline 4 ints' (map #(assoc % :foo :bar)) ints)
    (async/go-loop [int (async/<! ints'), current 0]
      (println current)
      (when int (recur (async/<! ints') (inc current)))))


  nil)
