(ns ppi-query.protein
  (:require [clojure.spec :as s]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.organism :as org]))

; Simple identification of a protein
(defrecord Protein [organism uniprotid])
(s/def ::protein
  (s/keys :req [::org/organism ::uni/uniprotid]))
