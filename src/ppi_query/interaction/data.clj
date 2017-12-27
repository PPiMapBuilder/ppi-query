(ns ppi-query.interaction.data
  (:require [clojure.spec.alpha :as s]
            [ppi-query.protein :as prot]))

(s/def ::identifier string?)
(s/def ::query string?)
(s/def ::client some?)
(s/def ::clients (s/coll-of ::client))
(s/def ::database string?)
(s/def ::databases (s/coll-of ::database))
(s/def ::taxid string?)
(s/def ::identifiers (s/coll-of (s/keys :req-un [::identifier ::database])))
(s/def ::organism (s/keys :req-un [::identifiers ::taxid]))

(s/def ::interactor (s/keys :req-un [::identifiers ::organism]))
(s/def ::interactorA ::interactor)
(s/def ::interactorB ::interactor)

(s/def ::interaction (s/keys :req-un [::interactorA ::interactorB]))
(s/def ::interactions (s/coll-of ::interaction))
(s/def ::protein-couple (s/tuple ::prot/protein ::prot/protein))

(defrecord ProteinsInteraction [protein-a
                                protein-b
                                original-interaction])

(s/def ::protein-a ::prot/protein)
(s/def ::protein-b ::prot/protein)
(s/def ::original-interaction ::interaction)
(s/def ::proteins-interaction
  (s/keys :req-un [::protein-a
                   ::protein-b
                   ::original-interaction]))
(s/def ::proteins-interactions (s/coll-of ::proteins-interaction))

(s/def ::ortholog-protein-a (s/nilable ::prot/protein))
(s/def ::ortholog-protein-b (s/nilable ::prot/protein))
(defrecord ProtOrthsInteraction [protein-a
                                 ortholog-protein-a
                                 protein-b
                                 ortholog-protein-b
                                 original-interaction])
(s/def ::prot-orths-interaction
  (s/keys :req-un [::protein-a
                   ::ortholog-protein-a
                   ::protein-b
                   ::ortholog-protein-b
                   ::original-interaction]))
(s/def ::prot-orths-interactions (s/coll-of ::prot-orths-interaction))

(s/def ::original-interactions ::interactions)
(defrecord ProtOrthsMultiInteraction [protein-a
                                      ortholog-protein-a
                                      protein-b
                                      ortholog-protein-b
                                      original-interactions])
(s/def ::prot-orths-multi-interaction
  (s/keys :req-un [::protein-a
                   ::ortholog-protein-a
                   ::protein-b
                   ::ortholog-protein-b
                   ::original-interactions]))
(s/def ::prot-orths-multi-interactions (s/coll-of ::prot-orths-multi-interaction))

(defn add-interaction-to-prot-orths-multi-interactions
    [prot-orths-multi-interaction interaction]
  (let [{:keys [protein-a ortholog-protein-a
                protein-b ortholog-protein-b
                original-interactions]}
        prot-orths-multi-interaction]
    (->ProtOrthsMultiInteraction
        protein-a ortholog-protein-a
        protein-b ortholog-protein-b
        (conj original-interactions interaction))))

(s/fdef add-interaction-to-prot-orths-multi-interactions
  :args (s/cat :prot-orths-multi-interaction ::prot-orths-multi-interaction
               :interaction                  ::interaction)
  :ret  ::prot-orths-multi-interaction)
