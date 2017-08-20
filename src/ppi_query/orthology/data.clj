(ns ppi-query.orthology.data
  (:require [clojure.spec :as s]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]))

; The following definitions are not in the orthology namespace to avoid
; circular dependencies between it and inparanoid or cache namespaces.

(s/def ::ortholog-score double?)
(s/def ::origin-protein ::prot/protein)

; Ortholog scored protein: protein with orthology score
(defrecord OrthologScoredProtein [organism uniprotid ortholog-score])
(s/def ::ortholog-scored-protein
  (s/merge ::prot/protein (s/keys :req-un [::ortholog-score])))
(s/def ::ortholog-scored-proteins (s/coll-of ::ortholog-scored-protein))

(defn ortholog-scored->protein [ortholog-scored-prot]
  (if ortholog-scored-prot
    (let [{:keys [organism uniprotid ortholog-score]} ortholog-scored-prot]
      (prot/->Protein
        organism uniprotid))))

(s/fdef ortholog-scored->protein
  :args (s/cat :ortholog-scored-protein (s/nilable ::ortholog-scored-protein))
  :ret  (s/nilable ::prot/protein))

; Ortholog group: ortholog scored protein by target organism
(s/def ::ortholog-group
  (s/map-of ::org/organism
            (s/coll-of ::ortholog-scored-protein :min-count 1)))
