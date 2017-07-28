(ns ppi-query.organism
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.spec :as s])
  (:import (java.io PushbackReader)))

; Simple identification of an organism
(defrecord Organism [common-name taxon-id scientific-name])

; Load Inparanoid organism repository into a set of Organism records
(def inparanoid-organism-repository
  (->> (-> "inparanoid-organism-repository.edn"
           (io/resource)
           (io/reader)
           (PushbackReader.)
           (edn/read))
    (map map->Organism)
    (into #{})))

(def inparanoid-organism-by-id
  "Get an inparanoid organism by taxon-id."
  (->> inparanoid-organism-repository
       (map (juxt :taxon-id identity))
       (into {})))

(s/fdef inparanoid-organism-by-id
  :args (s/cat :taxon-id ::taxon-id)
  :ret (s/nilable ::organism))

(defn get-shortname [organism]
  "Get short name of an organism (ex: M.musculus, H.sapiens, etc.)."
  (if-let [[_ [S] genus] (re-matches #"(\w+) (\w+).*"
                             (:scientific-name organism))]
    (str S \. genus)))

(s/fdef get-shortname
  :args (s/cat :organism ::organism)
  :ret string?)

(def inparanoid-organism-by-shortname
  "Get an inparanoid organism by short name."
  (->> inparanoid-organism-repository
       (map (juxt get-shortname identity))
       (into {})))

(s/fdef inparanoid-organism-by-shortname
  :args (s/cat :short-name string?)
  :ret (s/nilable ::organism))

(s/def ::organism
  (s/and
    ; restrict organisms to be in this set:
    (s/spec inparanoid-organism-repository)
    ; and having the following keys
    (s/keys :req-un [::taxon-id ::scientific-name] :opt-un [::common-name])))

(s/def ::common-name (s/nilable string?))
(s/def ::taxon-id pos-int?)
(s/def ::scientific-name string?)
