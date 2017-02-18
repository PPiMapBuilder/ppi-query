(ns ppi-query.protein.orthology
  (:require [clojure.spec :as s]
            [clojure.xml :as xml]
            [clojure.zip :as z]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.protein.uniprot :as uni]
            [clj-http.client :as client]))

(s/def ::ortholog-score double?)

(s/def ::ortholog-scored-protein
  (s/keys :req [::org/organism ::uni/uniprotid ::ortholog-score]))

(s/def ::ortholog-group
  (s/map-of ::org/organism (s/coll-of ::ortholog-scored-protein)))

(def inparanoid-url "http://inparanoid.sbc.su.se/cgi-bin/gene_search.cgi")

(defn- fetch-xml-by-prot [uniprotid]
  (->
    (client/get inparanoid-url
                {:query-params {"id" uniprotid
                                "idtype" "proteinid"
                                "all_or_selection" "all"
                                "rettype" "xml"}
                 :as :stream})
    (update :body xml/parse)))

(def fetch-xml-by-prot (memoize fetch-xml-by-prot))

(defn valid-node [node tag]
  (= (-> node first :tag) tag))

(defn trace [x] (println "trace" x) x)
(defn- select [zxml tag]
  "List nodes from an xml zipper for the specified tag"
  (let [valid-node #(= (-> % first :tag) tag)]
    (loop [node zxml
           selected []]
      (let [r (z/right node)
            d (z/down node)
            valid (valid-node node)]
        (cond
          (z/end? node) selected
          (not valid) (recur (if d d r) selected)
          :else (->>  (z/rights node)
                      (filter valid-node)
                      (concat selected)))))))

(defn fetch-inparanoid-ortholog-group [protein target-organism]
  (let [inparanoid-xml (fetch-xml-by-prot (:uniprotid protein))
        ztree (z/xml-zip (:body inparanoid-xml))]
    (loop [speciespair-node (z/down ztree)
           ortholog-group {}]
      (if (nil? (z/right speciespair-node))
        ortholog-group
        (let [species1-node (z/down speciespair-node)
              org1 (org/inparanoid-organism-by-species
                     (-> species1-node first :attrs :speclong))

              species2-node (z/right species1-node)
              org2 (org/inparanoid-organism-by-species
                     (-> species2-node first :attrs :speclong))

              prot-nodes (-> species2-node z/right z/down z/down)])))))

(s/fdef fetch-inparanoid-ortholog-group
  :args (s/cat :protein ::prot/protein :target-organism ::org/organism)
  :ret ::ortholog-group)

(comment
  (def b
    (let [human (org/inparanoid-organism-by-id 9606)
          mouse (org/inparanoid-organism-by-id 10090)
          catalase  (prot/->Protein human "P04040")]
      (->
        (fetch-xml-by-prot (:uniprotid catalase))
        :body
        z/xml-zip
        first :tag)
      #_
      (fetch-inparanoid-ortholog-group catalase human)))

  (require '[clojure.spec.gen :as gen])
  (gen/sample (s/gen ::ortholog-group)))
