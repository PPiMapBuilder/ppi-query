(ns ppi-query.interaction.query
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader))
  (:require [clojure.java.data :refer :all]
            [clojure.spec.alpha :as s]
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

(defn print-exception [exception client & query]
  (println "Warning: A request failed with an exception:")
  (println (.toString exception))
  (println "Client:" client)
  (println "The query was: ")
  (apply println query)
  (println "")
  '())

(defn get-by-query
  "Get lazy sequence of interactions by query (with psicquic client)"
  ([client query]
   (try
     (let [result-stream (.getByQuery client query)
           result-java   (.read reader result-stream)
           result-clj    (from-java result-java)]
       result-clj)
     (catch Exception ex
       (print-exception ex client "getByQuery" query))))
  ([client query max-results first-result]
   (try
     (let [result-stream (.getByQuery client query PsicquicSimpleClient/MITAB25
                                      first-result max-results)
           result-java   (.read reader result-stream)
           result-clj    (from-java result-java)]
       result-clj)
     (catch Exception ex
       (print-exception ex client "getByQuery" query
           "\nWith first & max results:" first-result max-results)))))

(s/fdef get-by-query
  :args (s/cat :client ::intrd/client :query ::intrd/query
               :max-results (s/? pos-int?)
               :first-result (s/? int?))
  :ret ::intrd/interactions)

; https://github.com/PSICQUIC/psicquic-simple-client/blob/master/src/example/java/org/hupo/psi/mi/psicquic/wsclient/PsicquicSimpleExampleLimited.java
(defn fetch-by-query
  "Handle pagination doing get-by-query (with psicquic client)"
  ([client query] (fetch-by-query client query 100))
  ([client query pagesize]
   (try
     (mapcat (partial get-by-query client query pagesize)
        (range 0 (.countByQuery client query) pagesize))
     (catch Exception ex
       (print-exception ex client "countByQuery" query)))))

(s/fdef fetch-by-query
  :args (s/cat :client ::intrd/client :query ::intrd/query
               :optional-pagesize (s/? pos-int?))
  :ret ::intrd/interactions)

(defn fetch-by-query-all-clients [clients query]
  "Apply the same query on all the clients and merge the result in
   one list of interactions"
  (apply concat
    (map #(fetch-by-query % query)
         clients)))

(s/fdef fetch-by-query-all-clients
  :args (s/cat :clients ::intrd/clients :query ::intrd/query)
  :ret  ::intrd/interactions)
