(ns ppi-query.orthology
  (:require [clojure.spec.alpha :as s]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.orthology.data :as orth]
            [ppi-query.orthology.cache :as cache]
            [ppi-query.orthology.orthoxml :as orthoxml]
            [ppi-query.orthology.inparanoid :as inparanoid]))


(defn get-ortholog-species-pair
  "Get all ortholog group for a species pair"
  [org1 org2]
  (if-let [species-pair (cache/get-ortholog-species-pair org1 org2)]
    species-pair
    (when-let [species-pair (orthoxml/fetch-ortholog-species-pair! org1 org2)]
      (cache/add-ortholog-species-pair species-pair)
      species-pair)))

(s/fdef get-ortholog-species-pair
  :args (s/cat :organism1 ::org/organism :organism2 ::org/organism)
  :ret ::cache/ortholog-cache)


(defn ^:deprecated get-ortholog-group-inp [protein]
  "Get an ortholog group for a protein from cache or from inparanoid web service."
  (if-let [ortholog-group (cache/get-ortholog-group protein)]
    ortholog-group
    (when-let [ortholog-group (inparanoid/get-ortholog-group protein)]
      (cache/add-ortholog-group protein ortholog-group)
      ortholog-group)))


(defn get-ortholog-group
  "Get an ortholog group for a protein from cache or from inparanoid orthoXML."
  [target-organism protein]
  (if (= target-organism (:organism protein))
      #{(orth/protein->ortholog-scored protein 1.0)}
      (let [source-organism (:organism protein)
            species-pair (get-ortholog-species-pair source-organism target-organism)]
        (when species-pair
          (get-in species-pair [source-organism protein target-organism])))))

(s/fdef get-ortholog-group
  :args (s/cat :target-organism ::org/organism :protein ::prot/protein)
  :ret ::orth/ortholog-scored-proteins)


(def default-ortholog-score-threshold 0.85)

(defn get-best-orthologs
  "Get the best orthologs for a protein in a target organism."
  [target-organism protein]
  (try
    (let [prot (if (:ortholog-score protein) (orth/ortholog-scored->protein protein) protein)]
      (when-let [orthologs (get-ortholog-group target-organism prot)]
        (let [best-score (apply max (map :ortholog-score orthologs))]
          (filter #(and (= best-score (:ortholog-score %))
                        (>= (:ortholog-score %) default-ortholog-score-threshold))
            orthologs))))
    (catch Exception ex
      (println "Warning: A request to get-best-orthologs failed with an exception:")
      (println (.toString ex))
      (println "Organism sent:" target-organism)
      (println "Protein sent:" protein)
      '())))

(s/fdef get-best-orthologs
  :args (s/cat :target-organism ::org/organism :protein ::prot/protein)
  :ret (s/coll-of ::orth/ortholog-scored-protein))


(comment
  (def human (org/inparanoid-organism-by-id 9606))
  (def mouse (org/inparanoid-organism-by-id 10090))
  (def catalase  (prot/->Protein human "P04040"))
  (get-best-orthologs mouse catalase)
  nil)
