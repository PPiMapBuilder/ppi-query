(ns ppi-query.interaction
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader))
  (:require [clojure.java.data :refer :all]
            [clojure.spec :as s]))


(s/def ::identifier string?)
(s/def ::database string?)
(s/def ::identifiers (s/coll-of (s/keys :req-un [::identifier ::database])))

(s/def ::interactor (s/keys :req-un [::identifiers]))
(s/def ::interactorA ::interactor)
(s/def ::interactorB ::interactor)

(s/def ::interaction (s/keys :req-un [::interactorA ::interactorB]))

(def registry
  [{:name "IntAct"
    :url  "http://www.ebi.ac.uk/Tools/webservices/psicquic/intact/webservices/current/search/"}])

(def registry-clients
  (map (fn [service]
         (new PsicquicSimpleClient (:url service)))
       registry))

(def reader (new PsimiTabReader))

(defn get-by-query [client query]
  "Get lazy sequence of interactions by query (with psicquic client)"
  (let [result-stream (.getByQuery client query)
        result-java   (.read reader result-stream)
        result-clj    (from-java result-java)]
    result-clj))

(s/fdef get-by-query
  :args (s/cat :client class? :query string?)
  :ret (s/coll-of ::interactions))


(defn get-interactor-database-ids [database interactor]
  "Get interactor identifiers for a specific database"
  (->> interactor
    :identifiers
    (filter #(= (:database %) database))
    (map :identifier)))

(s/fdef get-interactor-database-ids
  :args (s/cat :database string? :interactor ::interactor)
  :ret (s/coll-of string?))


(def get-interactor-uniprotid
  (comp first (partial get-interactor-database-ids "uniprotkb")))

(s/fdef get-interactor-uniprotid
  :args (s/cat :database string?)
  :ret (s/coll-of ::identifier))


(def get-interactors-uniprotids
  (juxt
    (comp get-interactor-uniprotid :interactorA)
    (comp get-interactor-uniprotid :interactorB)))

(s/fdef get-interactors-uniprotids
  :args (s/cat :interaction ::interaction)
  :ret (s/coll-of ::identifier))
