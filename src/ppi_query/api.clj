(ns ppi-query.api
  (:require [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.network :as network]
            [ppi-query.organism :as org]))

(declare to-java)

(defn map-to-java [object]
  (->> object
       (map (fn [[k v]] [(name k) (to-java v)]))
       (into {})))

(defn seq-to-java [object]
  (->> object
       (map (fn [v] (to-java v)))
       (into (list))))

(defn to-java [object]
  (cond
    (map? object) (map-to-java object)
    (seq? object) (seq-to-java object)
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
  (^java.util.List getInteractome [^java.util.List database-names ^java.lang.Long organism-id]))

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
