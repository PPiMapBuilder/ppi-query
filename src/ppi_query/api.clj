(ns ppi-query.api
  (:require [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.organism :as org]))

(defn to-string-map [kw-map]
  "Transform a keyword keyed map into a string keyed map."
  (->> kw-map
       (map (fn [[k v]] [(name k) v]))
       (into {})))

(definterface Api
  (^java.util.List getPsicquicServices [])
  (^java.util.List getOrganisms []))

(gen-class
  :name ppi_query.api.ApiImpl
  :implements [ppi_query.api.Api]
  :prefix "api-")

(defn api-getPsicquicServices []
  (->> (reg/get-registry!)
       (vals)
       (map to-string-map)))

(defn api-getOrganisms []
  (->> org/inparanoid-organism-repository
       (map to-string-map)))

