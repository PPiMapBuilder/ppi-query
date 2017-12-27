(ns ppi-query.orthology.orthoxml
  (:require [clojure.spec.alpha :as s]
            [ppi-query.orthology.cache :as ortholog.cache]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.protein :as protein]
            [ppi-query.protein.uniprot :as uniprot]
            [clj-http.client :as http]
            [clojure.xml :as xml]
            [ppi-query.xml :as pxml]
            [ppi-query.organism :as orgn]
            [clojure.data.zip.xml :as zx]
            [clojure.zip :as zip]
            [ppi-query.spec :as ps]))


(def registry "http://inparanoid.sbc.su.se/download/8.0_current/Orthologs_OrthoXML/")


(defn create-request
  "Prepare HTTP GET request to fetch an orthoXML species pair"
  [[short-org1 short-org2]]
  (println "Request to : " (str registry short-org1 "/" short-org1 "-" short-org2 ".orthoXML"))
  {:method :get
   :url (str registry short-org1 "/" short-org1 "-" short-org2 ".orthoXML")
   :as :stream})

(s/fdef create-request
  :args (s/cat :orgs (s/spec (s/cat :short-org1 string? :short-org2 string?)))
  :ret map?)


(s/def ::gene-xml
  (pxml/node
    :tag #{:gene}
    :attrs (ps/map-spec :protId ::uniprot/uniprotid)))

(s/def ::species-xml
  (pxml/node
    :tag #{:species}
    :attrs (ps/map-spec :name string? :NCBITaxId string?)
    :content (s/cat
               :db (pxml/node
                     :tag #{:database}
                     :content (s/coll-of
                                (pxml/node
                                  :tag #{:genes}
                                  :content (s/coll-of ::gene-xml))))

               :. any?)))

(s/def ::score-xml
  (pxml/node
    :tag #{:score}
    :attrs (ps/map-spec :id string? :value string?)))

(s/def ::gene-ref-xml
  (pxml/node
    :tag #{:geneRef}
    :content (s/coll-of ::score-xml)))

(s/def ::ortholog-group-xml
  (pxml/node
    :tag #{:orthologGroup}
    :content (s/coll-of (s/or :score ::score-xml
                              :geneRef ::gene-ref-xml))))

(s/def ::ortholog-groups-xml
  (pxml/node
    :tag #{:groups}
    :content (s/coll-of ::ortholog-group-xml)))

(s/def ::orthoxml
  (pxml/node
    :tag #{:orthoXML}
    :content (s/cat :notes any?
                    :species1 ::species-xml
                    :species2 ::species-xml
                    :scores any?
                    :groups ::ortholog-groups-xml)))


(defn fetch-orthoxml!
  "Fetch orthoXML species pair from inparanoid"
  [organism1 organism2]
  (-> (sort [(orgn/get-shortname organism1)
             (orgn/get-shortname organism2)])
      (create-request)
      (http/request)
      (:body)
      (xml/parse)))

(s/fdef fetch-orthoxml!
  :args (s/cat :organism1 ::orgn/organism :organism2 ::orgn/organism)
  :ret ::orthoxml)


(defn parse-species-xml
  "Parse species section of an orthoXML"
  [species-xml]
  (let [organism
          (-> species-xml
              zip/node
              :attrs
              :NCBITaxId
              Integer/parseInt
              orgn/inparanoid-organism-by-id)
        genes
          (map #(:attrs (zip/node %))
               (zx/xml-> species-xml :database :genes :gene))]
    (apply merge
      (map #(hash-map
               (Integer/parseInt (:id %))
               (protein/->Protein organism (:protId  %)))
           genes))))

(s/fdef parse-species-xml
  :args (s/cat :species-zip
           (s/spec (s/cat :species-xml ::species-xml :zip any?)))
  :ret (s/map-of int? ::protein/protein))


(defn parse-ortholog-group-xml
  "Parse ortholog group section of an orthoXML"
  [proteins-map orth-group]
  (map (fn [gene-ref]
         (let [prot (-> gene-ref
                        zip/node
                        :attrs
                        :id
                        Integer/parseInt
                        proteins-map)
               score (-> gene-ref
                         (zx/xml1-> :score (zx/attr= :id "inparalog"))
                         zip/node
                         :attrs
                         :value
                         Double/parseDouble)]
            (orthd/->OrthologScoredProtein
               (:organism prot)
               (:uniprotid prot)
               score)))
       (zx/xml-> orth-group :geneRef)))

(s/fdef parse-ortholog-group-xml
  :args (s/cat
          :proteins-map
            (s/spec (s/map-of int? ::protein/protein))
          :ortholog-groups-zip
            (s/spec  (s/cat :ortholog-group ::ortholog-group-xml :zip any?)))
  :ret (s/coll-of ::orthd/ortholog-scored-protein))


(defn as-protein [ortholog-prot]
  (protein/->Protein
    (:organism ortholog-prot)
    (:uniprotid ortholog-prot)))


(defn parse-ortholog-xml
  "Parse an orthoXML"
  [orthoxml]
  (let [rootzip
          (zip/xml-zip orthoxml)
        orthozip
          (zx/xml1-> rootzip :orthoXML)
        proteins-map
          (apply merge
            (map parse-species-xml
                 (zx/xml-> orthozip :species)))
        ortholog-groups
          (map (partial parse-ortholog-group-xml proteins-map)
               (zx/xml-> orthozip :groups :orthologGroup))]
    (reduce
      (fn [cache-map ortholog-group]
         (reduce
            (fn [sub-cache-map [prot1 prot2]]
                (-> sub-cache-map
                  (update-in
                     [(:organism prot1) (as-protein prot1) (:organism prot2)]
                     (fnil conj #{})
                     prot2)
                  (update-in
                     [(:organism prot2) (as-protein prot2) (:organism prot1)]
                     (fnil conj #{})
                     prot1)))
            cache-map
            (for [prot1 ortholog-group
                  prot2 ortholog-group
                  :when (not= (:organism prot1) (:organism prot2))]
               [prot1 prot2])))
      {}
      ortholog-groups)))

(s/fdef parse-ortholog-xml
  :args (s/cat :orthoxml ::orthoxml)
  :ret ::ortholog.cache/ortholog-cache)


(defn fetch-ortholog-species-pair! [organism1 organism2]
  "Fetch orthoXML from inparanoid and parse it"
  (-> (fetch-orthoxml! organism1 organism2)
      (parse-ortholog-xml)))

(s/fdef fetch-ortholog-species-pair!
  :args (s/cat :organism1 ::orgn/organism :organism2 ::orgn/organism)
  :ret ::ortholog.cache/ortholog-cache)
