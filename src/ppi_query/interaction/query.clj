(ns ppi-query.interaction.query
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader))
  (:require [clojure.java.data :refer :all]
            [clojure.spec :as s]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.miql :as miql]))

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

(defn get-by-query
  "Get lazy sequence of interactions by query (with psicquic client)"
  ([client query]
   (let [result-stream (.getByQuery client query)
         result-java   (.read reader result-stream)
         result-clj    (from-java result-java)]
     result-clj))
  ([client query max-results first-result]
   (let [result-stream (.getByQuery client query PsicquicSimpleClient/MITAB25
                                    first-result max-results)
         result-java   (.read reader result-stream)
         result-clj    (from-java result-java)]
     result-clj)))

(s/fdef get-by-query
  :args (s/cat :client ::intrd/client :query ::intrd/query
               :max-results (s/? pos-int?)
               :first-result (s/? int?))
  :ret ::intrd/interactions)

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  (miql/to-miql (miql/get-query-by-taxon 6239))]
      (println (take 2 (get-by-query client query)))))
  (get-by-query (first registry-clients) "taxidA:6239 AND taxidB:6239 AND species:6239"))
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

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          ;query  "P04040 or Q14145"
          query  (miql/to-miql (miql/get-query-by-taxon 6239))]
      (println (.countByQuery client query))
      (println (take 10 (get-by-query client query))))))

; https://github.com/PSICQUIC/psicquic-simple-client/blob/master/src/example/java/org/hupo/psi/mi/psicquic/wsclient/PsicquicSimpleExampleLimited.java
(defn fetch-by-query
  "Handle pagination doing get-by-query (with psicquic client)"
  ([client query] (fetch-by-query client query 100))
  ([client query pagesize]
   (mapcat (partial get-by-query client query pagesize)
      (range 0 (.countByQuery client query) pagesize))))

(s/fdef fetch-by-query
  :args (s/cat :client ::intrd/client :query ::intrd/query
               :optional-pagesize (s/? pos-int?))
  :ret ::intrd/interactions)

(defn merge-interactions [interactions]
  "Function to be implemented with mi-cluster or by hand"
  (apply concat interactions))

(s/fdef merge-interactions
  :args (s/cat :interactions (s/coll-of ::intrd/interactions))
  :ret  ::intrd/interactions)

(defn fetch-by-query-all-clients [clients query]
  "Apply the same query on all the clients and merge the result in
   one list of interactions"
  (merge-interactions
    (map #(fetch-by-query % query)
         clients)))

(s/fdef fetch-by-query-all-clients
  :args (s/cat :clients ::intrd/clients :query ::intrd/query)
  :ret  ::intrd/interactions)

(comment
  (binding [*print-level* 3]
    (let [query " ( taxidA:9606 AND taxidB:9606 AND species:9606 AND id:P04040 ) "]
      (println
        (take 2 (fetch-by-query-all-clients registry-clients query))))))
