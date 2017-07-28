(ns ppi-query.orthology.data
  (:require [clojure.spec :as s]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]))

; The following definitions are not in the orthology namespace to avoid
; circular dependencies between it and inparanoid or cache namespaces.

(s/def ::ortholog-score double?)

; Ortholog scored protein: protein with orthology score
(defrecord OrthologScoredProtein [organism uniprotid ortholog-score])
(s/def ::ortholog-scored-protein
  (s/merge ::prot/protein (s/keys :req-un [::ortholog-score])))

; Ortholog group: ortholog scored protein by target organism
(s/def ::ortholog-group
  (s/map-of ::org/organism
            (s/coll-of ::ortholog-scored-protein :min-count 1)))
