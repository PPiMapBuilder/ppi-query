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
            [ppi-query.spec :as ps]
            [ppi-query.utils :refer :all]))


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

(defn ortholog-groups->couples [ortholog-groups]
  (mapcat
    (fn [ortholog-group]
        (for [prot1 ortholog-group
              prot2 ortholog-group
              :when (not= (:organism prot1) (:organism prot2))]
           [prot1 prot2]))
    ortholog-groups))

(s/fdef ortholog-groups->couples
  :args (s/cat
          :ortholog-groups
            (s/spec (s/coll-of (s/coll-of ::orthd/ortholog-scored-protein :distinct true))))
  :ret (s/coll-of (s/coll-of ::orthd/ortholog-scored-protein :count 2 :distinct true)))

(defn first-arg [arg1 arg2] arg1)
; This function in used when computing the score of an ortholog-protein score in the cache.
; Should it be the min score of the two proteins in the ortholog group ?
;   The multiplication of the two scores ?
;   Just the initial score of this ortholog scored protein in the ortholog group ?
;   TODO Verify with @gcornut
(def default-score-func min)

(s/fdef default-score-func
  :args (s/cat :score-1 number? :score-2 number?)
  :ret number?)

(defn ortholgs-couples->ortholog-cache
  [ortholog-couples]
  (reduce
    (fn [cache-map
         [{org1 :organism protid1 :uniprotid score1 :ortholog-score :as prot1}
          {org2 :organism protid2 :uniprotid score2 :ortholog-score :as prot2}]]
        (-> cache-map
          (update-in
             [org1 (protein/->Protein org1 protid1) org2]
             (fnil conj #{})
             (orthd/->OrthologScoredProtein
                org2
                protid2
                (default-score-func score2 score1)))
          (update-in
             [org2 (protein/->Protein org2 protid2) org1]
             (fnil conj #{})
             (orthd/->OrthologScoredProtein
                org1
                protid1
                (default-score-func score1 score2)))))
    {}
    ortholog-couples))

(s/fdef ortholgs-couples->ortholog-cache
  :args (s/cat
          :ortholog-groups
            (s/spec (s/coll-of (s/coll-of ::orthd/ortholog-scored-protein :count 2 :distinct true))))
  :ret ::ortholog.cache/ortholog-cache)

(defn ortholgs-couples->score-min->ortholog-cache
  [ortholog-couples]
  (ortholgs-couples->ortholog-cache ortholog-couples min))

(defn ortholgs-couples->standard->ortholog-cache
  [ortholog-couples]
  (ortholgs-couples->ortholog-cache ortholog-couples (fn [sc1 sc2] sc1)))

(defn parse-ortholog-xml
  "Parse an orthoXML"
  [orthoxml]
  (let [rootzip
          (do (println "## Orthoxml downloaded, starting parsing.")
              (time (zip/xml-zip orthoxml)))
        orthozip
          (zx/xml1-> rootzip :orthoXML)
        proteins-map
          ; (Could be parallellized, but quite short)
          (do (println "## Calculating protein-map...")
              (time
               (doall
                (apply merge
                  (map parse-species-xml
                       (zx/xml-> orthozip :species))))))
        ortholog-groups-xml
          (do (println "## Getting all ortholog groups...")
              (time
               (doall
                 (zx/xml-> orthozip :groups :orthologGroup))))
        ortholog-groups
          (do (println "## Parsing ortholog groups (very long, to optimize)...")
              (println (count ortholog-groups-xml) "ortholog groups to parse.")
              (time
               (doall
                (ppmap2 (partial parse-ortholog-group-xml proteins-map)
                        ortholog-groups-xml))))
        ortholog-couples
          (do (println "## Generating orthologs proteins couples..")
              (time
               (doall
                (ortholog-groups->couples ortholog-groups))))]
    ;(dorun (map println (take 10 ortholog-groups-proteins)))
    (println "## Generating cache-style data from ortholog couples..")
    (time
     (doall
      (ortholgs-couples->ortholog-cache ortholog-couples)))))

(s/fdef parse-ortholog-xml
  :args (s/cat :orthoxml ::orthoxml)
  :ret ::ortholog.cache/ortholog-cache)


(defn fetch-ortholog-species-pair! [organism1 organism2]
  "Fetch orthoXML from inparanoid and parse it"
  (try
    (-> (fetch-orthoxml! organism1 organism2)
        (parse-ortholog-xml))
    (catch Exception  e
      (println "Fail in fetch-ortholog-species-pair! " organism1 organism2)
      (println "Caught Exception:" (.getMessage e)))))

(s/fdef fetch-ortholog-species-pair!
  :args (s/cat :organism1 ::orgn/organism :organism2 ::orgn/organism)
  :ret ::ortholog.cache/ortholog-cache)
