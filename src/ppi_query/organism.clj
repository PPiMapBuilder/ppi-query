(ns ppi-query.organism
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec :as s]))

; Simple identification of an organism
(defrecord Organism [common-name taxon-id scientific-name])

; Load Inparanoid organism repository into a set of Organism records
(def inparanoid-organism-repository
  (->> (-> "inparanoid-organism-repository.edn"
           io/resource
           io/reader
           java.io.PushbackReader.
           edn/read)
    (map map->Organism)
    (into #{})))

(def inparanoid-organism-by-id
  (->> inparanoid-organism-repository
       (map (juxt :taxon-id identity))
       (into {})))

(defn get-species [organism]
  (if-let [matches (re-matches #"(\w+ \w+).*"
                               (:scientific-name organism))]
    (str (second matches))))

(def inparanoid-organism-by-species
  (->> inparanoid-organism-repository
       (map (juxt get-species identity))
       (into {})))

(s/def ::organism
  (s/and
    ; restrict organisms to be in this set:
    (s/spec inparanoid-organism-repository)
    ; and having the following keys
    (s/keys :req-un [::common-name ::taxon-id ::scientific-name])))

(s/def ::common-name string?)
(s/def ::taxon-id pos-int?)
(s/def ::scientific-name string?)
