(ns ppi-query.orthology.inparanoid-test
  (:require [ppi-query.orthology.inparanoid :as inp]
            [ppi-query.orthology.cache :as cache]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.organism :as org]
            [ppi-query.orthology.data :as orth]
            [clojure.xml :as xml]
            [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test :as stest]
            [ppi-query.protein :as prot]))

(stest/instrument)

(deftest test-parse-protein
  (let [xml {:tag :protein
             :content nil
             :attrs {:speclink "url/9606"
                     :score "0.42"
                     :prot_id "P04040"}}]
    (is (= (inp/parse-ortholog-scored-protein xml)
           (orth/->OrthologScoredProtein
             (org/inparanoid-organism-by-id 9606)
             "P04040"
             0.42)))))

; Orthology score generator
(defn score-gen []
  (gen/fmap str (gen/double* {:min 0 :max 1})))

; Uniprot taxonomy url generator
(defn uniprot-taxonomy-url-gen []
  (->> org/inparanoid-organism-repository
       (map :taxon-id)
       (gen/elements)
       (gen/fmap (partial str "url/"))))

(deftest check-parse-taxon-id
  (check' `inp/parse-taxon-id
          {:gen {::inp/uniprot-taxonomy-url uniprot-taxonomy-url-gen}}))

(deftest check-parse-protein-xml
  (check' `inp/parse-ortholog-scored-protein
          {:gen {::inp/uniprot-taxonomy-url uniprot-taxonomy-url-gen
                 ::inp/score-xml score-gen}}))

(deftest check-parse-inparanoid-xml
  (check' `inp/parse-ortholog-group
          {:gen {::inp/uniprot-taxonomy-url uniprot-taxonomy-url-gen
                 ::inp/score-xml score-gen}}))

(deftest check-get-ortholog-group
  ; Stub inparanoid I/O using a custom xml generator
  (stest/instrument
    `inp/fetch-xml-by-uniprotid
    {:stub #{`inp/fetch-xml-by-uniprotid}
     :gen {::inp/uniprot-taxonomy-url uniprot-taxonomy-url-gen
           ::inp/score-xml score-gen}})

  (check' `inp/get-ortholog-group))

