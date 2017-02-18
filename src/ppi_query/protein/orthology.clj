(ns ppi-query.protein.orthology
  (:require [clojure.spec :as s]
            [clojure.xml :as xml]
            [clojure.zip :as z]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.protein.uniprot :as uni]
            [clj-http.client :as client]))

(def inparanoid-url "http://inparanoid.sbc.su.se/cgi-bin/gene_search.cgi")
(def ^:dynamic cache-location (str "/tmp/" *ns*))
(def ortholog-cache (atom {}))

(s/def ::ortholog-score double?)

(s/def ::ortholog-scored-protein
  (s/keys :req-un [::org/organism ::uni/uniprotid ::ortholog-score]))

(s/def ::ortholog-group
  (s/map-of ::org/organism (s/coll-of ::ortholog-scored-protein)))

(s/def ::ortholog-cache
  (s/map-of ::org/organism (s/map-of ::prot/protein ::ortholog-group)))

(defprotocol OrthologClient
  (get-ortholog-group [this protein]))

(defprotocol OrthologCache
  (add-ortholog-group [this protein ortholog-group]))

(defn- fetch-xml-by-prot [uniprotid]
  "Fetch inparanoid search by protein id response (& parse body xml)"
  (-> (client/get inparanoid-url
        {:query-params {"id" uniprotid
                        "idtype" "proteinid"
                        "all_or_selection" "all"
                        "rettype" "xml"}
         :as :stream})
    (update :body xml/parse)))

(defn trace [x]
  (binding [*print-level* 2]
    (println "trace" x))
  x)

(defn- get-taxon-id [uniprot-taxonomy-url]
  "Get taxonomy id from uniprot taxonomy url."
  (-> (re-matches #"\D*(\d+)$" uniprot-taxonomy-url)
    second
    Long/parseLong))

(defn- get-ortholog-scored-protein [prot-nodes]
  "Convert xml protein nodes into ortholog-scored-protein."
  (->> prot-nodes
    ; get prot attributes
    (map :attrs)
    ; convert to ortholog-scored-protein
    (map #(hash-map
           :organism (org/inparanoid-organism-by-id
                       (get-taxon-id (:speclink %)))
           :uniprotid (:prot_id %)
           :ortholog-score (Double/parseDouble (:score %))))))

(defn fetch-ortholog-group [protein]
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
                            ; remove proteins from ref-organism of nil organisms
                            (remove (comp
                                      #(or (nil? %) (= ref-organism %))
                                      :organism)))]
          (recur
            ; next speciespair xml node
            (z/right speciespair-node)
            (if-let [target-organism (:organism (first proteins))]
              (assoc ortholog-group target-organism proteins)
              ortholog-group)))))))

(s/fdef fetch-ortholog-group
  :args (s/cat :protein ::prot/protein)
  :ret ::ortholog-group)

(comment
  (let [human (org/inparanoid-organism-by-id 9606)
        catalase  (prot/->Protein human "P04040")]
    (fetch-ortholog-group catalase)
    #_
    (->
      (fetch-xml-by-prot (:uniprotid catalase))
      :body
      z/xml-zip
      first :tag)
    #_
    (fetch-ortholog-group catalase human))

  (require '[clojure.spec.gen :as gen])
  (gen/sample (s/gen ::ortholog-group)))
