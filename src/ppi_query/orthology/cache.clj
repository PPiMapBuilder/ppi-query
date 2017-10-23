(ns ppi-query.orthology.cache
  (:require [clojure.spec.alpha :as s]
            [ppi-query.orthology.data :as orth]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.utils :as utils]))

; Ortholog group by protein by organism (protein's organism)
(s/def ::ortholog-cache
  (s/map-of ::org/organism
    (s/map-of ::prot/protein ::orth/ortholog-group)))

; Ortholog memory cache
(def ^:dynamic mem-cache (atom {}))


(defn get-ortholog-group
  "Get from memory cache an ortholog group for a protein."
  [protein]
  (let [organism (:organism protein)]
    (get-in @mem-cache [organism protein]) :ortholog/not-found))

(s/fdef get-ortholog-group
  :args (s/cat :protein ::prot/protein)
  :ret  (s/or :ortholog-group ::orth/ortholog-group
              :not-found #{:ortholog/not-found}))


(defn add-ortholog-group
  "Add to memory cache an ortholog group for a protein."
  [protein ortholog-group]
  (if-let [organism (:organism protein)]
    (swap!
      mem-cache
      assoc-in
      [organism protein]
      (utils/merge-distinct
        (get-ortholog-group protein)
        ortholog-group))))

(s/fdef add-ortholog-group
  :args (s/cat :protein ::prot/protein
               :ortholog-group ::orth/ortholog-group)
  :ret ::ortholog-cache)


(defn get-ortholog-species-pair
  "Get species pair ortholog groups from mem cache"
  [organism1 organism2]
  (let [orthologs-org1 (get @mem-cache organism1)
        orthologs-org2 (get @mem-cache organism2)]
    (if (and orthologs-org1 orthologs-org2)
      {organism1 orthologs-org1, organism2 orthologs-org2}
      nil)))

(s/fdef get-ortholog-species-pair
  :args (s/cat :organism1 ::org/organism :organism2 ::org/organism)
  :ret (s/nilable ::ortholog-cache))


(defn add-ortholog-species-pair
  "Add species pair ortholog groups to mem cache"
  [species-pair]
  (swap!
    mem-cache
    utils/merge-distinct
    species-pair))

(s/fdef add-ortholog-species-pair
  :args (s/cat :ortholog-species-pair ::ortholog-cache)
  :ret ::ortholog-cache)
