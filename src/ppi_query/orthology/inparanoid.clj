(ns ppi-query.orthology.inparanoid
  (:require [clojure.spec :as s]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :refer [xml->]]
            [clj-http.client :as http]
            [ppi-query.spec :as ps]
            [ppi-query.xml :as pxml]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.orthology.data :as orth]))

; Inparanoid gene search xml response spec (only we is parsed)
(s/def ::inparanoid-xml
  (pxml/node
    :tag #{:cluster_list}
    :content (s/coll-of
               (pxml/node
                 :tag #{:speciespair}
                 :content (s/tuple
                            (pxml/node :tag #{:species})
                            (pxml/node :tag #{:species})
                            (pxml/node
                              :tag #{:clusters}
                              :content (s/tuple
                                         (pxml/node
                                           :tag #{:cluster}
                                           :content (s/coll-of ::protein-xml)))))))))

(s/def ::uniprot-taxonomy-url string?)
(s/def ::score-xml string?)
(s/def ::protein-xml
  (pxml/node
    :tag #{:protein}
    :attrs (ps/map-spec
             :speclink ::uniprot-taxonomy-url
             :prot_id ::uni/uniprotid
             :score ::score-xml)))

(defn create-request [uniprotid]
  "Create HTTP request for inparanoid protein search."
  {:method       :get
   :url          "http://inparanoid.sbc.su.se/cgi-bin/gene_search.cgi"
   :query-params {:id               uniprotid
                  :idtype           "proteinid"
                  :all_or_selection "all"
                  :rettype          "xml"}
   :as           :stream})

(s/fdef create-request
        :args (s/cat :id ::uni/uniprotid)
        :ret map?)

(defn fetch-xml-by-uniprotid [uniprotid]
  "Fetch inparanoid protein search (& parse xml body)."
  (-> (create-request uniprotid)
      (http/request)
      (:body)
      (xml/parse)))

(s/fdef fetch-xml-by-uniprotid
        :args (s/cat :id ::uni/uniprotid)
        :ret ::inparanoid-xml)

(defn parse-taxon-id [uniprot-taxonomy-url]
  "Get taxonomy id from uniprot taxonomy url."
  (-> (re-matches #"\D*(\d+)$" uniprot-taxonomy-url)
      (second)
      (Long/parseLong)))

(s/fdef parse-taxon-id
        :args (s/cat :url ::uniprot-taxonomy-url)
        :ret int?)

(defn parse-ortholog-scored-protein [protein-xml]
  "Parse a protein xml node into an ortholog scored protein."
  (-> (:attrs protein-xml)
      (#(orth/->OrthologScoredProtein
         ; organism
         (org/inparanoid-organism-by-id (parse-taxon-id (:speclink %)))
         ; uniprotid
         (:prot_id %)
         ; ortholog-score
         (Double/parseDouble (:score %))))))

(s/fdef parse-ortholog-scored-protein
        :args (s/cat :protein-xml ::protein-xml)
        :ret ::orth/ortholog-scored-protein)

(defn parse-ortholog-group [inparanoid-xml ref-organism]
  "Parse inparanoid gene search response xml document into an ortholog group."
  (->> (zip/xml-zip inparanoid-xml)

       ; List protein xml nodes
       (#(xml-> % :cluster_list :speciespair :clusters :cluster :protein))

       ; convert protein to OrthologScoredProtein record
       (map (comp parse-ortholog-scored-protein
                  first))

       ; remove protein without organism or in reference organism
       (remove (some-fn (comp nil? :organism)
                        (comp #{ref-organism} :organism)))

       ; regroup OrthologScoredProtein by organism
       (partition-by :organism)

       ; for tuple of organism and list of OrthologScoredProtein
       (map (juxt (comp :organism first) identity))

       ; into a map
       (into {})))

(s/fdef parse-ortholog-group
        :args (s/cat :inparanoid-xml ::inparanoid-xml
                     :ref-organism ::org/organism)
        :ret ::orth/ortholog-group)

(defn get-ortholog-group [{:keys [uniprotid organism] :as protein}]
  "Fetch ortholog group for a protein"
  (-> (fetch-xml-by-uniprotid uniprotid)
      (parse-ortholog-group organism)))

(s/fdef get-ortholog-group
        :args (s/cat :prot ::prot/protein)
        :ret ::orth/ortholog-group)

(comment
  (let [catalase "P04040"
        human (org/inparanoid-organism-by-id 9606)]
    (def a (get-ortholog-group {:uniprotid catalase :organism human}))))
