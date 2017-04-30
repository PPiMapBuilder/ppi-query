(ns ppi-query.api
  (:require [ppi-query.interaction.psicquic.registry :as reg])
  (:import (java.util List)))

(defn to-string-map [kw-map]
  "Transform a keword keyed map into a string keyed map."
  (->> kw-map
       (map (fn [[k v]] [(name k) v]))
       (into {})))

(gen-class
  :name ppi_query.api.PPIQueryAPI
  :methods [^:static [getServices [] java.util.List]])

(defn -getServices []
  (->> (reg/update-registry)
       (vals)
       (map to-string-map)
       (doall)))

