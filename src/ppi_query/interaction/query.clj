(ns ppi-query.interaction.query
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader))
  (:require [clojure.java.data :refer :all]
            [clojure.spec.alpha :as s]
            [ppi-query.interaction.data :as intrd]
            [com.climate.claypoole :as cp]
            [clojure.core.async :as async]
            [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.utils :as utils]))

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

(def retry-count 4)
(defn retry-wait [retry]
  (Thread/sleep
    (* 300 (max 0
                 (- retry-count
                    retry)))))

(defn get-by-query
  "Get lazy sequence of interactions by query (with psicquic client)"
  ([client query]
   (get-by-query client query retry-count))
  ([client query retry]
   (try
     (let [result-stream (.getByQuery client query)
           result-java   (.read reader result-stream)
           result-clj    (from-java result-java)]
       result-clj)
     (catch Exception ex
       (if (<= retry 1)
         (print-exception ex client "getByQuery" query)
         (do (println "Retry getByQuery" retry)
             (retry-wait retry)
             (get-by-query client query (dec retry)))))))
  ([client query max-results first-result]
   (get-by-query client query max-results first-result retry-count))
  ([client query max-results first-result retry]
   (try
     (let [result-stream (.getByQuery client query PsicquicSimpleClient/MITAB25
                                      first-result max-results)
           result-java   (.read reader result-stream)
           result-clj    (from-java result-java)]
       result-clj)
     (catch Exception ex
       (if (<= retry 1)
         (print-exception ex client "getByQuery" query
             "\nWith first & max results:" first-result max-results)
         (do (println "Retry getByQuery" retry)
             (retry-wait retry)
             (get-by-query client query first-result max-results (dec retry))))))))

;(s/fdef get-by-query
;  :args (s/cat :client ::intrd/client :query ::intrd/query
;               :max-results (s/? pos-int?)
;               :first-result (s/? int?)))))))
;  :ret ::intrd/interactions)

(defn count-by-query
  ([client query]
   (count-by-query client query retry-count))
  ([client query retry]
   (try
     (.countByQuery client query)
     (catch Exception ex
       (if (<= retry 1)
         (print-exception ex client "countByQuery" query)
         (do (println "Retry countByQuery" retry)
             (retry-wait retry)
             (count-by-query client query (dec retry))))))))

; https://github.com/PSICQUIC/psicquic-simple-client/blob/master/src/example/
; java/org/hupo/psi/mi/psicquic/wsclient/PsicquicSimpleExampleLimited.java
(defn fetch-by-query
  "Handle pagination doing get-by-query (with psicquic client)"
  ([client query] (fetch-by-query client query 100))
  ([client query pagesize]
   (mapcat (partial get-by-query client query pagesize)
      (range 0 (count-by-query client query) pagesize))))

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

(def ^:const DEFAULT-RETRY-COUNT 4)
(def ^:const DEFAULT-PAGE-SIZE 100)
(def ^:const DEFAULT-THREAD-POOL-SIZE (+ 1 (cp/ncpus)))

(defn get-by-query2
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
        (print-exception ex client "getByQuery" query
                         "\nWith first & max results:" page-offset page-size)
        (do (println "Retry getByQuery" retry)
            (retry-wait retry)
            (get-by-query2 client query
                           :page-offset page-offset
                           :page-size page-size
                           :retry (dec retry)))))))

(defn prepare-requests
  "Prepares page requests for a database"
  [out query page-size db]
  (let [client (reg/get-client db)
        total-count (count-by-query client query)
        page-offsets (range 0 total-count page-size)

        ; Post-process interaction (assoc source db) & put on output channel
        output-interaction (comp #(async/>!! out %)
                                 #(assoc % :source db))]
    ; Prepare get-by-query requests
    (map
      (fn [page-offset]
        #(->> (get-by-query2 client query
                             :page-offset page-offset
                             :page-size page-size)
              (map output-interaction)
              (doall)))
      page-offsets)))

(defn dbs-get-by-query>
  "Asynchronously execute query on databases in a thread pool.
   Returns a channel of interactions "
  [dbs query & {:keys [threads, page-size]
                :or   {threads DEFAULT-THREAD-POOL-SIZE,
                       page-size DEFAULT-PAGE-SIZE}}]
  (let [out (async/chan (* threads page-size))]
    (async/thread
      (let [pool (cp/threadpool threads)
            prepare-requests2 (partial prepare-requests out query page-size)
            requests (cp/upmap pool prepare-requests2 dbs)]
        (->> (reduce concat requests)
             (cp/upmap pool #(apply % []))
             (doall))
        (async/close! out)
        (cp/shutdown! pool)))
    out))
