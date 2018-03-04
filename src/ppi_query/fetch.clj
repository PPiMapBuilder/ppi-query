(ns ppi-query.fetch
  (:require [clojure.spec.alpha :as s]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.query :as intrq]
            [ppi-query.interaction.miql :as miql]
            [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.interaction.query :as query]
            [clojure.core.async :as async]
            [com.climate.claypoole :as cp]))


(s/def ::nb-thread int?)
(def ^:const DEFAULT-THREAD-POOL-SIZE (+ 1 (cp/ncpus)))


(defn get-taxon-interactions [clients organism]
  "Fetch full interactome of organism"
  (->> (miql/get-query-by-taxon (:taxon-id organism))
       miql/to-miql
       (intrq/fetch-by-query-all-clients clients)))

(s/fdef get-taxon-interactions
  :args (s/cat :clients  ::intrd/clients
               :organism ::orgn/organism)
  :ret  ::intrd/interactions)


(defn get-direct-interactions [clients organism proteins]
  "Fetch direct interactions between proteins into organism"
  (if (empty? proteins)
    '()
    (->> (miql/get-query-by-taxon-and-prots
           (:taxon-id organism)
           (into #{} (map :uniprotid proteins)))
         miql/to-miql
         (intrq/fetch-by-query-all-clients clients))))

(s/fdef get-direct-interactions
  :args (s/cat :clients  ::intrd/clients
               :organism ::orgn/organism
               :proteins ::prot/proteins)
  :ret  ::intrd/interactions)


(defn get-secondary-interactions [clients organism proteins]
  "Fetch secondary interactions between proteins into organism"
  (mapcat #(intrq/fetch-by-query-all-clients clients (miql/to-miql %))
          (miql/get-queries-by-taxon-and-prot-pool
             (:taxon-id organism)
             (into #{} (map :uniprotid proteins)))))

(s/fdef get-secondary-interactions
  :args (s/cat :clients  ::intrd/clients
               :organism ::orgn/organism
               :proteins ::prot/proteins)
  :ret  ::intrd/interactions)


(defn get-proteins-orthologs [organism proteins]
  "Fetch all orthologs of proteins into organism"
  (mapcat (fn [protein]
              (orth/get-best-orthologs organism protein))
          proteins))

(s/fdef get-proteins-orthologs
  :args (s/cat :target-organism ::orgn/organism
               :protein         ::prot/proteins)
  :ret  ::orthd/ortholog-scored-proteins)


(defn prepare-requests
  "Prepares all page requests for a database, a query, a page size
   which will output to the given channel"
  [out query page-size db]
  (let [client (reg/get-client db)

        ; Prepare pagination: total count & page offset for each pages
        total-count (query/count-by-query client query)
        page-offsets (range 0 total-count page-size)

        ; Post-process interaction (assoc source db) & put on output channel
        output-interaction (comp #(async/>!! out %)
                                 #(assoc % :source db))]
    ; Prepare get-by-query requests for each page
    (map
      (fn [page-offset]
        #(->> (query/lazy-get-by-query client query
                                       :page-offset page-offset
                                       :page-size page-size)
              (map output-interaction)
              (doall)))
      page-offsets)))

(s/fdef prepare-requests
  :args (s/cat :output-channel any? :query ::intrd/query :page-size int? :db string?)
  :ret  (s/coll-of fn?))


(defn fetch-dbs-by-query>
  "Asynchronously execute query on databases in a thread pool.
   Immediately returns a channel of interactions"
  [dbs query & {:keys [nb-thread, page-size]
                :or   {nb-thread DEFAULT-THREAD-POOL-SIZE,
                       page-size query/DEFAULT-PAGE-SIZE}}]
  (let [out (async/chan (* nb-thread page-size))]
    (async/thread
      (let [pool (cp/threadpool nb-thread)
            prepare-requests' (partial prepare-requests out query page-size)
            ; Prepare all request functions (in thread pool)
            requests (cp/upmap pool prepare-requests' dbs)]
        ; Concat all requests and wait for all to execute (in thread pool)
        (->> (reduce concat requests)
             (cp/upmap pool #(apply % []))
             (doall))
        ; Close output channel & thread pool
        (async/close! out)
        (cp/shutdown! pool)))
    out))

(s/fdef fetch-dbs-by-query>
  :args (s/cat :databases ::intrd/databases :query ::intrd/query
               :options (s/keys* :opt-un [::nb-thread ::intrq/page-size])))
