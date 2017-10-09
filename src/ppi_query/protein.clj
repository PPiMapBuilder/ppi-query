(ns ppi-query.protein
  (:require [clojure.spec.alpha :as s]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.organism :as org]))

; Simple identification of a protein
(defrecord Protein [organism uniprotid])
(s/def ::protein
  (s/keys :req-un [::org/organism ::uni/uniprotid]))
(s/def ::proteins (s/coll-of ::protein))
