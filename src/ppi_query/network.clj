(ns ppi-query.network
  (:require [clojure.spec :as s]
            [ppi-query.interaction :as intr]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.protein.orthology :as orth]
            [ppi-query.fetch :as fetch]))

(defn get-orthologs-direct-interactions
  [clients organism proteins]
  (let [orthologs ; parallel to direct-interactions
          (into #{} (fetch/get-proteins-orthologs organism proteins))
          ; -> proteins
        interactions
          (fetch/get-direct-interactions clients organism orthologs)]
          ; -> interactions
    [organism orthologs interactions]))

(s/fdef get-orthologs-direct-interactions
  :args (s/cat :clients      (s/coll-of any?)
               :organism     ::orgn/organism
               :proteins     (s/coll-of ::prot/protein))
  :ret  (s/cat :organism     ::orgn/organism
               :orthologs    (s/coll-of ::orth/ortholog-scored-protein)
               :interactions (s/coll-of ::intr/interaction)))

(defn get-orthologs-secondary-interactions
  [clients organism orthologs direct-interactions]
  (->> direct-interactions
       intr/get-proteins ; Set of Proteins
       (#(disj % (into #{} orthologs)))
       (fetch/get-secondary-interactions clients organism)))

(s/fdef get-orthologs-secondary-interactions
  :args (s/cat :clients      (s/coll-of any?)
               :organism     ::orgn/organism
               :orthologs    (s/coll-of ::orth/ortholog-scored-protein)
               :direct-interactions (s/coll-of ::intr/interaction))
  :ret  (s/coll-of ::intr/interaction))

(defn merge-proteins-and-get-secondary-interactions
    [clients ref-organism proteins
     direct-interactions orthologs-direct-interactions]
  (let [orthologs
         (into #{}
           (concat
             (intr/get-proteins direct-interactions)
             (mapcat (fn [organism orthologs interactions]
                         (->> interactions
                              intr/get-proteins
                              (fetch/get-proteins-in-ref-organism organism))
                      orthologs-direct-interactions))))]))

(s/fdef merge-proteins-and-get-secondary-interactions
  :args (s/cat :clients      (s/coll-of any?)
               :ref-organism ::orgn/organism
               :proteins     (s/coll-of ::prot/proteins)
               :direct-interactions (s/coll-of ::intr/interaction)
               :orthologs-direct-interactions (s/coll-of ::intr/interaction))
  :ret  (s/coll-of ::intr/interaction))

(defn fetch-interactome [databases organism]
  (let [clients (fetch/get-clients databases)]
    (fetch/get-taxon-interactions clients organism)))

(s/fdef fetch-interactome
  :args (s/cat :databases (s/coll-of ::intr/database)
               :organism ::orgn/organism)
  :ret  (s/coll-of ::intr/interaction))

(defn fetch-protein-network
  [databases ; PSICQUIC databases to query
   ref-organism  ; Organism of Interest
   proteins ; Proteins of Interest
   other-organisms] ; Other Organisms to check

  (let [clients (fetch/get-clients databases)

        ; Get proteins in reference organism
        cleaned-proteins
          (into #{} (fetch/get-proteins-in-ref-organism
                      ref-organism proteins))

        ; Get Direct Interactions (left arrow)
        direct-interactions
          (future (fetch/get-direct-interactions
                    clients ref-organism cleaned-proteins))

        ; Get proteins orthologs + Get direct interactors
        orthologs-direct-interactions
          ; potentially parallellized
          (map (fn [organism]
                  (get-orthologs-direct-interactions
                    clients organism cleaned-proteins))
               other-organisms)


        ; Three blue arrows + left secondary interactions
        proteins-and-secondary-interactions
          (future (merge-proteins-and-get-secondary-interactions
                     clients cleaned-proteins
                     direct-interactions orthologs-direct-interactions))

        ; Two violet arrows + right secondary arrows
        orthologs-secondary-interactions
           ; Potentially parallellized
           (map (fn [[organism orthologs interactions]]
                    (get-orthologs-secondary-interactions
                      clients organism orthologs interactions))
                orthologs-direct-interactions)
        ; TODO orthologs-secondary-interactions->reference organism
        ; Deref future proteins-and-secondary-interactions
        [return-proteins secondary-interactions] @proteins-and-secondary-interactions]
        ; TODO Merge interactions for the return
    [direct-interactions secondary-interactions return-proteins orthologs-secondary-interactions]))


(s/fdef fetch-interactome
  :args (s/cat :databases       (s/coll-of ::intr/database)
               :ref-organism    ::orgn/organism
               :proteins        (s/coll-of ::prot/proteins)
               :other-organisms (s/coll-of ::orgn/organism))
  :ret  (s/coll-of any?))
        ;(s/coll-of ::intr/interaction))
