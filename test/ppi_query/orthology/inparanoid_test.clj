(ns ppi-query.orthology.inparanoid-test
  (:require [ppi-query.orthology.inparanoid :as inp]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.organism :as org]
            [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test :as stest]))

; Orthology score generator
(def score-gen (gen/fmap str (gen/double* {:min 0 :max 1})))

; Uniprot taxonomy url generator
(def uniprot-taxonomy-url-gen
     (->> org/inparanoid-organism-repository
          (map :taxon-id)
          (gen/elements)
          (gen/fmap (partial str "url/"))))

(deftest test-parse-taxon-id
  (check' `inp/parse-taxon-id
          {:gen {::inp/uniprot-taxonomy-url (constantly uniprot-taxonomy-url-gen)}}))

;; Protein xml generator
(def protein-xml-gen
     (xml-gen :tag :protein
              :attrs (gen/hash-map :speclink uniprot-taxonomy-url-gen
                                   :prot_id (s/gen ::uni/uniprotid)
                                   :score score-gen)))

(deftest test-parse-protein-xml
  (check' `inp/parse-ortholog-scored-protein
          {:gen {::inp/protein-xml (constantly protein-xml-gen)}}))

; Generator for inparanoid gene search xml response (simplified)
; example: http://inparanoid.sbc.su.se/cgi-bin/gene_search.cgi?id=P04040&idtype=proteinid&all_or_selection=all&rettype=xml
(def inparanoid-xml-gen
  (let [clusters-gen (gen/tuple protein-xml-gen)

        speciespair-gen (gen/tuple
                          (xml-gen :tag :species)
                          (xml-gen :tag :species)
                          (xml-gen :tag :clusters
                                   :content clusters-gen))

        clusterlist-gen (gen/vector
                          (xml-gen :tag :speciespair
                                   :content speciespair-gen))]
    (xml-gen :tag :cluster_list
             :content clusterlist-gen)))

(deftest test-parse-inparanoid-xml
  (check' `inp/parse-ortholog-group
          {:gen {::inp/inparanoid-xml (constantly inparanoid-xml-gen)}}))

(deftest test-get-ortholog-group
  ; Stub inparanoid I/O using a custom xml generator
  (stest/instrument
    `inp/fetch-xml-by-uniprotid
    {:stub #{`inp/fetch-xml-by-uniprotid}
     :gen {::inp/inparanoid-xml (constantly inparanoid-xml-gen)}})

  (check' `inp/get-ortholog-group))
