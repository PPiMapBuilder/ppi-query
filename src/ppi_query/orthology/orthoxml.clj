(ns ppi-query.orthology.orthoxml
  (:require [clojure.spec.alpha :as s]
            [ppi-query.orthology.cache :as ortholog.cache]
            [ppi-query.protein :as protein]
            [ppi-query.protein.uniprot :as uniprot]
            [clj-http.client :as http]
            [clojure.xml :as xml]
            [ppi-query.xml :as pxml]
            [ppi-query.organism :as orgn]
            [clojure.data.zip.xml :refer [text xml-> xml1->]]
            [clojure.zip :as zip]
            [ppi-query.spec :as ps]))


(def registry "http://inparanoid.sbc.su.se/download/8.0_current/Orthologs_OrthoXML/")


(s/fdef create-request
  :args (s/cat :orgs (s/spec (s/cat :short-org1 string? :short-org2 string?)))
  :ret map?)

(defn create-request [[short-org1 short-org2]]
  {:method :get
   :url (str registry short-org1 "/" short-org1 "-" short-org2 ".orthoXML")
   :as :stream})


(s/def ::species-xml
  (pxml/node
    :tag #{:species}
    :attrs (ps/map-spec :name string? :NCBITaxId string?)
    :content (pxml/node
               :tag #{:database}
               :content (pxml/node
                          :tag #{:genes}
                          :content (s/coll-of
                                     (pxml/node
                                       :tag #{:gene}
                                       :content (ps/map-spec :protId ::uniprot/uniprotid)))))))
(s/def ::score-xml
  (pxml/node
    :tag #{:score}
    :attrs (ps/map-spec :id string? :value string?)))

(s/def ::gene-ref-xml
  (pxml/node
    :tag #{:geneRef}
    :content
      (s/coll-of ::score-xml)))

(s/def ::ortholog-group-xml
  (pxml/node
    :tag #{:orthologGroup}
    :content
      (s/coll-of (s/or :score ::score-xml :geneRef ::gene-ref-xml))))

(s/def ::ortholog-groups-xml
  (pxml/node
    :tag #{:groups}
    :content
      (s/coll-of ::ortholog-group-xml)))

(s/def ::orthoxml
  (pxml/node
    :tag #{:orthoXML}
    :content
      (s/cat :notes any?
             :species1 ::species-xml
             :species2 ::species-xml
             :scores any?
             :groups ::ortholog-groups-xml)))

(s/fdef fetch-orthoxml
  :args (s/cat :organism1 ::orgn/organism :organism2 ::orgn/organism)
  :ret ::orthoxml)

(defn fetch-ortholoxml [organism1 organism2]
  (-> (sort [(orgn/get-shortname organism1)
             (orgn/get-shortname organism2)])
      (create-request)
      (http/request)
      (:body)
      (xml/parse)))

(s/fdef parse-species-xml
  :args (s/cat :species-xml ::species-xml)
  :ret (s/map-of int? ::protein/protein))

(defn logt [t]
  (println t)
  t)

(defn parse-species-xml [species-xml]
  (let [organism
          (-> species-xml
              zip/node
              :attrs
              :NCBITaxId
              Integer/parseInt
              orgn/inparanoid-organism-by-id)
        genes
          (map #(:attrs (zip/node %))
               (xml-> species-xml :database :genes :gene))]

    (println (first genes))
    (apply merge
      (map #(hash-map
               (Integer/parseInt (:id %))
               (protein/->Protein organism (:protId  %)))
           genes))))

(s/fdef parse-orthoxml
  :args (s/cat :orthoxml ::orthoxml)
  :ret ::ortholog.cache/ortholog-cache)

(defn parse-orthologx-xml [orthoxml]
  (let [rootzip (zip/xml-zip orthoxml)
        orthozip (xml1-> rootzip :orthoXML)]
     (map parse-species-xml (xml-> orthozip :species))))

(comment
  (def orgs orgn/inparanoid-organism-repository)

  (->>
    (for [org1 orgs org2 orgs :when (not= org1 org2)]
      #{(orgn/get-shortname org1) (orgn/get-shortname org2)})
    (distinct)
    (map sort)
    (def org-pairs))

  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)
  (def o1 (orgn/inparanoid-organism-by-id 272561))
  (def o2 (orgn/inparanoid-organism-by-id 7159))
  (def orthoxml
    (fetch-ortholoxml o1 o2))
  (def rootzip (zip/xml-zip orthoxml))
  (def orthozip (xml1-> rootzip :orthoXML))
  (def specieszip1 (first (xml-> orthozip :species)))
  (def geneszip (xml1-> specieszip1 :database :genes :gene))
  (parse-species-xml specieszip1)
  :nil)
