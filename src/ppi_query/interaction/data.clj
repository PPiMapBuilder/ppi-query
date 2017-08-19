(ns ppi-query.interaction.data
  (:require [clojure.spec :as s]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.interaction :as int]))


(defrecord OrthologInteraction [protein1
                                protein2
                                original-interaction])
(s/def ::ortholog-interaction
  (s/keys :req-un [::orthd/ortholog-sourced-protein
                   ::orthd/ortholog-sourced-protein
                   ::int/interaction]))
(s/def ::otholog-interactions (s/coll-of ::ortholog-interaction))

(defrecord OriginalOrgInteraction [protein1
                                   ortholog-protein1
                                   protein2
                                   ortholog-protein2
                                   original-interaction])
(s/def ::original-org-interaction
  (s/keys :req-un [::prot/protein
                   (s/nilable ::orthd/ortholog-sourced-protein)
                   ::prot/protein
                   (s/nilable ::orthd/ortholog-sourced-protein)
                   ::int/interaction]))
(s/def ::original-org-interactions (s/coll-of ::original-org-interactions))
