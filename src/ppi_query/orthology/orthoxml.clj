(ns ppi-query.orthology.orthoxml
  (:require [clojure.spec.alpha :as s]
            [ppi-query.organism :as organism]
            [ppi-query.orthology.cache :as ortholog.cache]
            [ppi-query.protein :as protein]
            [ppi-query.protein.uniprot :as uniprot]
            [clj-http.client :as http]
            [clojure.xml :as xml]
            [ppi-query.xml :as pxml]
            [ppi-query.organism :as org]
            [clojure.data.zip.xml :refer [text xml-> xml1->]]
            [clojure.zip :as zip]
            [ppi-query.spec :as ps]))


(def registry "http://inparanoid.sbc.su.se/download/8.0_current/Orthologs_OrthoXML/")


(s/fdef create-request
  :args (s/spec (s/cat :short-org1 string? :short-org2 string?))
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

(s/def ::ortholog-group-xml
  (pxml/node
    :tag #{:groups}))


(s/def ::orthoxml
  (pxml/node
    :tag #{:orthoXML}
    :content
      (s/cat :notes any?
             :species1 ::species-xml
             :species2 ::species-xml
             :scores any?
             :groups ::ortholog-group-xml)))

(s/fdef fetch-orthoxml
  :args (s/cat :organism1 ::organism/organism :organism2 ::organism/organism)
  :ret ::orthoxml)

(defn fetch-ortholoxml [organism1 organism2]
  (-> (sort [(organism/get-shortname organism1)
             (organism/get-shortname organism2)])
      (create-request)
      (http/request)
      (:body)
      (xml/parse)))

(s/fdef parse-species-xml
  :args (s/cat :species-xml ::species-xml)
  :ret (s/coll-of ::protein/protein))

(defn parse-species-xml [species-xml]
  nil)

(s/fdef parse-orthoxml
  :args (s/cat :orthoxml ::orthoxml)
  :ret ::ortholog.cache/ortholog-cache)

(defn parse-orthologx-xml [orthoxml]
  (let [rootzip (zip/xml-zip orthoxml)
        orthozip (xml-> rootzip :orthoXML)]
     (map parse-species-xml (xml-> orthozip :species))))

(comment
  (def orgs organism/inparanoid-organism-repository)

  (->>
    (for [org1 orgs org2 orgs :when (not= org1 org2)]
      #{(organism/get-shortname org1) (organism/get-shortname org2)})
    (distinct)
    (map sort)
    (def org-pairs))

  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)
  (def o1 (org/inparanoid-organism-by-id 272561))
  (def o2 (org/inparanoid-organism-by-id 7159))
  (def x
    (fetch-ortholoxml o1 o2))
  :nil)
