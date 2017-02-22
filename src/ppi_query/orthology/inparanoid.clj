(ns ppi-query.orthology.inparanoid
  (:require [clojure.spec :as s]
            [clojure.xml :as xml]
            [clojure.zip :as z]
            [clj-http.client :as client]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.orthology.data :as orth]))

(def inparanoid-url "http://inparanoid.sbc.su.se/cgi-bin/gene_search.cgi")

(defn- fetch-xml-by-prot [uniprotid]
  "Fetch inparanoid search by protein id response (& parse body xml)"
  (-> (client/get inparanoid-url
        {:query-params {"id" uniprotid
                        "idtype" "proteinid"
                        "all_or_selection" "all"
                        "rettype" "xml"}
         :as :stream})
    (update :body xml/parse)))

(s/fdef fetch-xml-by-prot
  :args (s/cat :id ::uni/uniprotid))

(defn- get-taxon-id [uniprot-taxonomy-url]
  "Get taxonomy id from uniprot taxonomy url."
  (-> (re-matches #"\D*(\d+)$" uniprot-taxonomy-url)
    second
    Long/parseLong))

(s/fdef get-taxon-id
  :args (s/cat :url string?)
  :ret ::org/taxon-id)

(defn- get-ortholog-scored-protein [prot-nodes]
  "Convert xml protein nodes into ortholog-scored-protein."
  (->> prot-nodes
    ; get prot attributes
    (map :attrs)
    ; convert to ortholog-scored-protein
    (map #(orth/->OrthologScoredProtein
           ; organism
           (org/inparanoid-organism-by-id (get-taxon-id (:speclink %)))
           ; uniprotid
           (:prot_id %)
           ; ortholog-score
           (Double/parseDouble (:score %))))))

(s/fdef get-ortholog-scored-protein
  :ret ::orth/ortholog-scored-protein)

(defn get-ortholog-group [protein]
  "Fetch ortholog group for a protein"
  (let [inparanoid-xml (fetch-xml-by-prot (:uniprotid protein))
        ztree (z/xml-zip (:body inparanoid-xml))
        ref-organism (:organism protein)]
    (loop [speciespair-node (z/down ztree)
           ortholog-group {}]
      (if (nil? (z/right speciespair-node))
        ortholog-group
        (let [prot-node (-> speciespair-node
                            ;species->species->clusters->cluster->protein
                            z/down z/right z/right z/down z/down)

              prot-nodes (concat [(first prot-node)] (z/rights prot-node))

              proteins (->> (get-ortholog-scored-protein prot-nodes)
                            ; remove proteins from ref-organism or nil
                            (remove (comp
                                      #(or (nil? %) (= ref-organism %))
                                      :organism)))]
          (recur
            ; next speciespair xml node
            (z/right speciespair-node)
            (if-let [target-organism (:organism (first proteins))]
              (assoc ortholog-group target-organism proteins)
              ortholog-group)))))))

(s/fdef get-ortholog-group
  :args (s/cat :prot ::prot/protein)
  :ret ::orth/ortholog-group)
