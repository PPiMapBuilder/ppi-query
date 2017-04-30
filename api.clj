(ns ppi-query.api
  (:import java.util.Collection
           ppi_query.organism.Organism
           ppi_query.protein.Protein))

; Maybe a macro could do that sometime ^^

(defprotocol IOrganism
  (^int getTaxonId [this]))

(extend-type Organism
  IOrganism
  (getTaxonId [this] (:taxon-id this)))

(defprotocol IProtein
  (^String getUniprotId [this])
  (^IOrganism getOrganism [this]))

(extend-type Protein
  IProtein
  (getUniprotId [this] (:uniprotid this))
  (getOrganism [this] (:organism this)))

(defprotocol IInteraction
  (^IProtein getInteractorA [this])
  (^IProtein getInteractorB [this])
  (^IOrganism getSourceOrganism [this])
  (^Collection getSourceDatabases [this]))
