(ns ppi-query.interaction.query
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader))
  (:require [clojure.java.data :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [ppi-query.interaction.data :as intrd]))

(s/def ::page-size int?)
(s/def ::page-offset int?)
(s/def ::retry int?)

(def ^:const DEFAULT-PAGE-SIZE 100)
(def ^:const DEFAULT-RETRY-COUNT 4)

; Psimi Reader
(def reader (new PsimiTabReader))

(defn- print-exception [exception client & query]
  (log/error
    "Warning: A request failed with an exception:" (.toString exception)
    "Client:" client
    "The query was: " (apply str query))
  '())

(defn- retry-wait [retry]
  (Thread/sleep
    (* 300 (max 0 (- DEFAULT-RETRY-COUNT retry)))))


(defn get-by-query
  "Get lazy sequence of interactions by query (with psicquic client)"
  ([client query]
   (get-by-query client query DEFAULT-RETRY-COUNT))
  ([client query retry]
   (try
     (let [result-stream (.getByQuery client query)
           result-java   (.read reader result-stream)
           result-clj    (from-java result-java)]
       result-clj)
     (catch Exception ex
       (if (<= retry 1)
         (print-exception ex client "getByQuery" query)
         (do (log/debug "Retry getByQuery" retry)
             (retry-wait retry)
             (get-by-query client query (dec retry)))))))
  ([client query max-results first-result]
   (get-by-query client query max-results first-result DEFAULT-RETRY-COUNT))
  ([client query max-results first-result retry]
   (try
     (let [result-stream (.getByQuery client query PsicquicSimpleClient/MITAB25
                                      first-result max-results)
           result-java   (.read reader result-stream)
           result-clj    (from-java result-java)]
       result-clj)
     (catch Exception ex
       (if (<= retry 1)
         (print-exception ex client "getByQuery" query "\n"
             "With first & max results:" first-result max-results)
         (do (log/debug "Retry getByQuery" retry)
             (retry-wait retry)
             (get-by-query client query first-result max-results (dec retry))))))))

;(s/fdef get-by-query
;  :args (s/cat :client ::intrd/client :query ::intrd/query
;               :max-results (s/? pos-int?)
;               :first-result (s/? int?)))))))
;  :ret ::intrd/interactions)

(defn count-by-query
  ([client query]
   (count-by-query client query DEFAULT-RETRY-COUNT))
  ([client query retry]
   (try
     (.countByQuery client query)
     (catch Exception ex
       (if (<= retry 1)
         (print-exception ex client "countByQuery" query)
         (do (log/debug "Retry countByQuery" retry)
             (retry-wait retry)
             (count-by-query client query (dec retry))))))))


; https://github.com/PSICQUIC/psicquic-simple-client/blob/master/src/example/
; java/org/hupo/psi/mi/psicquic/wsclient/PsicquicSimpleExampleLimited.java
(defn fetch-by-query
  "Handle pagination doing get-by-query (with psicquic client)"
  ([client query] (fetch-by-query client query 100))
  ([client query page-size]
   (mapcat (partial get-by-query client query page-size)
      (range 0 (count-by-query client query) page-size))))

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


(defn lazy-get-by-query
  "Get interaction by query returning a lazy sequence of interactions.
   Retries on error"
  [client query & {:keys [page-offset, page-size, retry]
                   :or {page-offset 0,
                        page-size   DEFAULT-PAGE-SIZE,
                        retry       DEFAULT-RETRY-COUNT}}]
  (try
    (->> (.getByQuery client query PsicquicSimpleClient/MITAB25
                      page-offset page-size)
         (.iterate reader)
         (iterator-seq)
         (map from-java))
    (catch Exception ex
      (if (<= retry 1)
        (print-exception ex client "getByQuery" query "\n"
                         "With first & max results:" page-offset page-size)
        (do (log/debug "Retry getByQuery" retry)
            (retry-wait retry)
            (lazy-get-by-query client query
              :page-offset page-offset :page-size page-size
              :retry (dec retry)))))))

(s/fdef lazy-get-by-query
  :args (s/cat :client ::intrd/client :query ::intrd/query
               :options (s/keys* :opt-un [::page-offset, ::page-size, ::retry]))
  :ret (s/* ::intrd/interaction))
