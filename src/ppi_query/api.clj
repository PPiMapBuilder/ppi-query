(ns ppi-query.api
  (:require [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.network :as network]
            [ppi-query.protein :as protein]
            [ppi-query.organism :as org]))

(declare to-java)

(defn nil-or-empty? [e]
  (or (nil? e) (and (seq? e) (empty? e))))

(defn map-to-java [object]
  (->> object
       #_(remove (comp (nil-or-empty? second)))
       (map (fn [[k v]] [(name k) (to-java v)]))
       (into {})))

(defn seq-to-java [object]
  (->> object
       (map (fn [v] (to-java v)))
       (remove nil-or-empty?)
       (into (list))))

(defn to-java [object]
  (cond
    (map? object) (map-to-java object)
    (string? object) object
    (seqable? object) (seq-to-java (seq object))
    :else object))

(defn to-string-map [kw-map]
  "Transform a keyword keyed map into a string keyed map."
  (->> kw-map
       (map
         (fn [[k v]]
           [(name k) v]))
       (into {})))

(definterface Api
  (^java.util.List getPsicquicServices [])

  (^java.util.List getOrganisms [])

  (^java.util.List getInteractome
    [^java.util.List database-names ^java.lang.Long organism-id])

  (^java.util.List getProteinNetwork
    [^java.util.List databases-names ^java.lang.Long ref-organism-id
     ^java.util.List protein-ids ^java.util.List other-organism-ids]))

(gen-class
  :name ppi_query.api.ApiImpl
  :implements [ppi_query.api.Api]
  :prefix "api-")

(defn api-getPsicquicServices [this]
  (->> (reg/get-registry!)
       (vals)
       (map to-string-map)))

(defn api-getOrganisms [this]
  (->> org/inparanoid-organism-repository
       (map to-string-map)))

(defn api-getInteractome [this database-names organism-id]
  (let [organism (org/inparanoid-organism-by-id organism-id)]
    (->>
      (network/fetch-interactome database-names organism)
      (to-java))))

(defn api-getProteinNetwork [this databases-names ref-organism-id protein-ids other-organism-ids]
  (let [ref-organism (org/inparanoid-organism-by-id ref-organism-id)
        other-organisms (map org/inparanoid-organism-by-id other-organism-ids)
        proteins (map (partial protein/->Protein ref-organism) protein-ids)]
    (->>
      (network/fetch-protein-network
        databases-names ref-organism proteins other-organisms)
      (second)
      (map #(dissoc % :original-interactions))
      (to-java))))
