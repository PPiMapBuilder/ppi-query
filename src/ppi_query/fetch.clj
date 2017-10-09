(ns ppi-query.fetch
  (:require [clojure.spec.alpha :as s]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.query :as intrq]
            [ppi-query.interaction.miql :as miql]
            [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd])
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)))

(s/fdef get-clients
  :args (s/cat :databases (s/coll-of ::intrd/database))
  :ret (partial instance? PsicquicSimpleClient))

(defn get-clients [names]
  "Get list of PSICQUIC clients by PSICQUIC service name"
  (map
    (fn [name]
      (when-let [service (reg/get-service name)]
        (new PsicquicSimpleClient (:url service))))
    names))

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
