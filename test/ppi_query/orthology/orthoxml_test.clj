(ns ppi-query.orthology.orthoxml-test
  (:require [clojure.test :refer :all]
            [ppi-query.test.utils :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [ppi-query.orthology.orthoxml :as orthoxml]
            [ppi-query.orthology.cache :as ortholog.cache]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology.data :as orthd]
            [clojure.data.zip.xml :as zx]
            [clojure.zip :as zip]))
(stest/instrument)
(def orgs orgn/inparanoid-organism-repository)

(->>
  (for [org1 orgs org2 orgs :when (not= org1 org2)]
    #{(orgn/get-shortname org1) (orgn/get-shortname org2)})
  (distinct)
  (map sort)
  (def org-pairs))

(def o1 (orgn/inparanoid-organism-by-id 272561))
(def o2 (orgn/inparanoid-organism-by-id 7159))

(def orthoxml
  (orthoxml/fetch-orthoxml! o1 o2))


(deftest* test-orthoxml
  (binding [*print-level* 3]
    (testing "Loading one xml file, test navigation, test specs"
      ;(println "orthoxml:" orthoxml)
      (is (s/valid? ::orthoxml/orthoxml orthoxml))

      (def rootzip (zip/xml-zip orthoxml))
      ;(println "rootzip:" rootzip)
      (is (s/valid? ::orthoxml/orthoxml (zip/node rootzip)))
      (is (s/valid? (s/cat :orthoxml ::orthoxml/orthoxml :zip any?) rootzip))

      (def orthozip (zx/xml1-> rootzip :orthoXML))
      ;(println "orthozip:" orthozip)
      (is (s/valid? ::orthoxml/orthoxml (zip/node orthozip)))
      (is (s/valid? (s/cat :orthoxml ::orthoxml/orthoxml :zip any?) orthozip))

      (def specieszip1 (first (zx/xml-> orthozip :species)))
      ;(println "specieszip1:" specieszip1)
      (is (s/valid? ::orthoxml/species-xml (zip/node specieszip1)))
      (is (s/valid? (s/cat :speciesxml1 ::orthoxml/species-xml :zip any?) specieszip1))

      (def geneszip (zx/xml1-> specieszip1 :database :genes :gene))
      ;(println "geneszip:" geneszip)
      (is (s/valid? ::orthoxml/gene-xml (zip/node geneszip)))
      (is (s/valid? (s/cat :genesxml ::orthoxml/gene-xml :zip any?) geneszip))

      (def proteins-map
        (apply merge
          (map orthoxml/parse-species-xml
               (zx/xml-> orthozip :species))))
      ;(println "proteins-map:" proteins-map
      (is (s/valid? (s/map-of int? ::prot/protein) proteins-map))

      (def orthogroup (zx/xml1-> orthozip :groups :orthologGroup))
      ;(println "orthogroup:" orthogroup)
      (is (s/valid? ::orthoxml/ortholog-group-xml (zip/node orthogroup)))
      (is (s/valid? (s/cat :orthogroup ::orthoxml/ortholog-group-xml :zip any?) orthogroup))

      (def orthogroups (zx/xml-> orthozip :groups :orthologGroup))
      ;(println "orthogroups:" orthogroups)
      (is (s/valid? ::orthoxml/ortholog-group-xml (zip/node (first orthogroups))))
      ;(is (s/valid? (s/cat :orthogroup ::orthoxml/ortholog-group-xml :zip any?) orthogroup))

      (def parsed-orth-group
         (orthoxml/parse-ortholog-group-xml
           proteins-map
           orthogroup))
      ;(println "parsed-orth-group:" parsed-orth-group)
      (is (s/valid? (s/coll-of ::orthd/ortholog-scored-protein)
                    parsed-orth-group))

      (def genescorezip
        (zx/xml1->
          (zx/xml1-> orthozip :groups :orthologGroup :geneRef)
          :score (zx/attr= :id "inparalog")))
      ;(println "genescorezip:" genescorezip)
      (is (s/valid? ::orthoxml/score-xml (zip/node genescorezip)))
      (is (s/valid? (s/cat :genescore ::orthoxml/score-xml :zip any?) genescorezip)))

    (testing "Full code"
      (def ortholog-group (orthoxml/parse-ortholog-xml orthoxml))
      ;(println ortholog-group)
      (is (s/valid? ::ortholog.cache/ortholog-cache
                    ortholog-group)))))
