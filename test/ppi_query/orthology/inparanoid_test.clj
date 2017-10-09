(ns ppi-query.orthology.inparanoid-test
  (:require [ppi-query.orthology.inparanoid :as inp]
            [ppi-query.orthology.cache :as cache]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.organism :as org]
            [ppi-query.orthology.data :as orth]
            [clojure.xml :as xml]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test.alpha :as stest]
            [ppi-query.protein :as prot])
  (:import (ppi_query.orthology.data OrthologScoredProtein)))

(comment
  (inp/fetch-xml-by-uniprotid "P04040"))

(defn create-protein-xml [{:keys [uniprotid organism ortholog-score]}]
  {:tag :protein
   :content nil
   :attrs {:speclink (str "url/" (:taxon-id organism))
           :prot_id uniprotid
           :score (str ortholog-score)}})

(defn create-cluster-xml [& proteins]
  {:tag :speciespair
   :attrs nil
   :content [{:tag :species :attrs nil :content nil}
             {:tag :species :attrs nil :content nil}
             {:tag :clusters
              :attrs nil
              :content [{:tag :cluster
                         :attrs nil
                         :content (into [] (map create-protein-xml proteins))}]}]})

(defn create-inparanoid-xml [& protein-clusters]
  {:tag :cluster_list
   :attrs nil
   :content (into [] (map create-cluster-xml protein-clusters))})

(def human (org/inparanoid-organism-by-id 9606))
(def mouse (org/inparanoid-organism-by-id 10090))

(def catalase-human
  (orth/->OrthologScoredProtein human "P04040" 1.0))

(def catalase-mouse
  (orth/->OrthologScoredProtein mouse "P05050" 0.99))

(def catalase2-mouse
  (orth/->OrthologScoredProtein mouse "P06060" 0.9))

(def catalase-protein-xml
  (create-protein-xml catalase-human))

(def catalase-inparanoid-xml
  (create-inparanoid-xml catalase-human catalase-mouse catalase2-mouse))

(def catalase-ortholog-group
  {mouse (list catalase-mouse catalase2-mouse)})

(deftest* test-inparanoid

  (testing "Parse protein xml into OrthologScoredProtein record"
    (is (= (inp/parse-ortholog-scored-protein catalase-protein-xml)
           catalase-human)))

  (testing "Test check get ortholog group"

    ; Stub inparanoid I/O using a custom xml generator
    (instrument-stub-return
      `inp/fetch-xml-by-uniprotid
      ::inp/inparanoid-xml
      #(gen/return catalase-inparanoid-xml))

    (is (=
         (inp/get-ortholog-group catalase-human)
         catalase-ortholog-group))))

; Generative testing below (can be long to run):
(comment
  ;
  ; Orthology score generator
  (defn score-gen []
    (gen/fmap str (gen/double* {:min 0 :max 1})))

  ; Uniprot taxonomy url generator
  (defn uniprot-taxonomy-url-gen []
    (->> org/inparanoid-organism-repository
         (map :taxon-id)
         (gen/elements)
         (gen/fmap (partial str "url/"))))

  (testing "Test check parse taxon id"
    (check' `inp/parse-taxon-id
            {:gen {::inp/uniprot-taxonomy-url uniprot-taxonomy-url-gen}}))

  (testing "Test check parse ortholog scored protein"
    (check' `inp/parse-ortholog-scored-protein
            {:gen {::inp/uniprot-taxonomy-url uniprot-taxonomy-url-gen
                   ::inp/score-xml score-gen}}))

  (testing "Test check parse ortholog group"
    (check' `inp/parse-ortholog-group
            {:gen {::inp/uniprot-taxonomy-url uniprot-taxonomy-url-gen
                   ::inp/score-xml score-gen}})))

