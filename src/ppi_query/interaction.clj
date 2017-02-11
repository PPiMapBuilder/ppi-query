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

; List of APIs
(def registry
  [{:name "IntAct"
    :url  "http://www.ebi.ac.uk/Tools/webservices/psicquic/intact/webservices/current/search/"}])

; Psicquic Clients
(def registry-clients
  (map (fn [service]
         (new PsicquicSimpleClient (:url service)))
       registry))

; Psimi Reader
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

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println (take 2 (get-by-query client query))))))

;({:detectionMethods (#), :updateDate (), :publications (# #),
;  :negativeInteraction false, :xrefs (), :checksums (),
;  :interactorA {
;    :features #, :organism #, :alternativeIdentifiers #,
;    :identifiers #, :xrefs #, :checksums #, :interactorTypes #,
;    :stoichiometry #, :empty false, :participantIdentificationMethods #,
;    :annotations #, :aliases #, :experimentalRoles #, :biologicalRoles #
;  },
;  :interactorB {
;    :features #, ###},
;  :complexExpansion (), :interactionAcs (# #),
;  :annotations (), :authors (#), :parameters (), :confidenceValues (#),
;  :creationDate (), :sourceDatabases (#), :interactionTypes (#), :hostOrganism nil
;}
;{:detectionMethods (#), ###})

(defn get-interactor-database-ids [database interactor]
  "Get interactor identifiers for a specific database"
  (->> interactor
    :identifiers
    (filter #(= (:database %) database))
    (map :identifier)))

(s/fdef get-interactor-database-ids
  :args (s/cat :database ::database :interactor ::interactor)
  :ret (s/coll-of string?))

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040"]
      (println
        (get-interactor-database-ids
          "uniprotkb"
          (:interactorA
            (first (get-by-query client query))))))))
; (P04040)

(def get-interactor-uniprotid
  (comp first (partial get-interactor-database-ids "uniprotkb")))

(s/fdef get-interactor-uniprotid
  :args (s/cat :database ::database)
  :ret (s/coll-of ::identifier))

; See above

(def get-interactors-uniprotids
  (juxt
    (comp get-interactor-uniprotid :interactorA)
    (comp get-interactor-uniprotid :interactorB)))

(s/fdef get-interactors-uniprotids
  :args (s/cat :interaction ::interaction)
  :ret (s/coll-of ::identifier))

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 6
          (map get-interactors-uniprotids
            (get-by-query client query)))))))

; ([P04040 Q14145] [P04040 P29991-PRO_0000037946] [P04040 P04040]
;  [B4DYC6 Q14145] [Q14145 Q8IVD9] [Q14145 Q96BE0] ###)
