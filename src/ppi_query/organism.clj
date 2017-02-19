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

(defn get-shortname [organism]
  (if-let [[_ [S] genus] (re-matches #"(\w+) (\w+).*"
                             (:scientific-name organism))]
    (str S \. genus)))

(def inparanoid-organism-by-shortname
  (->> inparanoid-organism-repository
       (map (juxt get-shortname identity))
       (into {})))

(s/def ::organism
  (s/and
    ; restrict organisms to be in this set:
    (s/spec inparanoid-organism-repository)
    ; and having the following keys
    (s/keys :req-un [::taxon-id ::scientific-name] :opt-un [::common-name])))

(s/def ::common-name (s/or :nil nil? :str string?))
(s/def ::taxon-id pos-int?)
(s/def ::scientific-name string?)
