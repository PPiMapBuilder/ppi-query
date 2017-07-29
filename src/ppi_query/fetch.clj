(ns ppi-query.fetch
  (:require [clojure.spec :as s]
            [ppi-query.interaction :as intr]
            [ppi-query.interaction.miql :as miql]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd]))

(defn get-clients
  [databases]
  intr/registry-clients) ;TODO Code

(s/fdef get-clients
  :args (s/cat :databases (s/coll-of ::intr/database))
  :ret  (s/coll-of any?))

(defn get-taxon-interactions [clients organism]
  "Fetch full interactome of organism"
  (->> (miql/get-query-by-taxon (:taxon-id organism))
       miql/to-miql
       (intr/fetch-by-query-all-clients clients)))

(s/fdef get-taxon-interactions
  :args (s/cat :clients (s/coll-of any?)
               :organism ::orgn/organism)
  :ret  ::intr/interactions)


(defn get-direct-interactions [clients organism proteins]
  "Fetch direct interactions between proteins into organism"
  (if (empty? proteins)
    '()
    (->> (miql/get-query-by-taxon-and-prots
           (:taxon-id organism)
           (into #{} (map :uniprotid proteins)))
         miql/to-miql
         (intr/fetch-by-query-all-clients clients))))

(s/fdef get-direct-interactions
  :args (s/cat :clients (s/coll-of any?)
               :organism ::orgn/organism
               :proteins (s/coll-of ::prot/protein))
  :ret  ::intr/interactions)

(defn get-secondary-interactions [clients organism proteins]
  "Fetch secondary interactions between proteins into organism"
  (mapcat #(intr/fetch-by-query-all-clients clients (miql/to-miql %))
          (miql/get-queries-by-taxon-and-prot-pool
             (:taxon-id organism)
             (into #{} (map :uniprotid proteins)))))

(s/fdef get-secondary-interactions
  :args (s/cat :clients (s/coll-of any?)
               :organism ::orgn/organism
               :proteins (s/coll-of ::prot/protein))
  :ret  ::intr/interactions)

(defn get-proteins-orthologs [organism proteins]
  "Fetch all orthologs of proteins into organism"
  (mapcat #(orth/get-best-orthologs organism %)
          proteins))

(s/fdef get-proteins-orthologs
  :args (s/cat :target-organism ::orgn/organism
               :protein (s/coll-of ::prot/protein))
  :ret  (s/coll-of ::orthd/ortholog-scored-protein))
