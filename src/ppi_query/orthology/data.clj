(ns ppi-query.orthology.data
  (:require [clojure.spec.alpha :as s]
            [ppi-query.organism :as orgn]
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

(defn protein->ortholog-scored [prot score]
  (if prot
    (let [{:keys [organism uniprotid]} prot]
      (->OrthologScoredProtein
        organism uniprotid score))))

(s/fdef protein->ortholog-scored
  :args (s/cat :protein (s/nilable ::prot/protein)
               :ortholog-score ::ortholog-score)
  :ret  (s/nilable ::ortholog-scored-protein))


; Ortholog group: ortholog scored protein by target organism
(s/def ::ortholog-group
  (s/map-of ::orgn/organism
            (s/spec (s/coll-of ::ortholog-scored-protein :min-count 1))))
