(ns ppi-query.network
  (:require [clojure.spec :as s]
            [ppi-query.interaction :as intr]
            [ppi-query.organism :as orgn]
            [ppi-query.protein :as prot]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.fetch :as fetch]))

(defn trace-f [f m]
  (println "########################################")
  (println f m)
  (println "########################################")
  m)

(defn get-orthologs-direct-interactions
  [clients organism proteins]
  (let [orthologs ; parallel to direct-interactions
         ;(trace-f "netwk/get-orthologs-direct-interactions orthologs"
          (into #{} (fetch/get-proteins-orthologs organism proteins))
          ; -> proteins
        interactions
         ;(trace-f "netwk/get-orthologs-direct-interactions interactions"
          (fetch/get-direct-interactions clients organism orthologs)]
          ; -> interactions
    [organism orthologs interactions]))

(s/fdef get-orthologs-direct-interactions
  :args (s/cat :clients      (s/coll-of any?)
               :organism     ::orgn/organism
               :proteins     (s/coll-of ::prot/protein))
  :ret  (s/cat :organism     ::orgn/organism
               :orthologs    (s/coll-of ::orthd/ortholog-scored-protein)
               :interactions ::intr/interactions))

(defn get-orthologs-secondary-interactions
  [clients organism orthologs direct-interactions]
  (->> direct-interactions
       intr/get-proteins ; Set of Proteins
       (#(disj % (into #{} orthologs)))
       (fetch/get-secondary-interactions clients organism)))

(s/fdef get-orthologs-secondary-interactions
  :args (s/cat :clients      (s/coll-of any?)
               :organism     ::orgn/organism
               :orthologs    (s/coll-of ::orthd/ortholog-scored-protein)
               :direct-interactions ::intr/interactions)
  :ret  ::intr/interactions)

(defn merge-proteins-and-get-secondary-interactions
    [clients ref-organism origin-proteins
     direct-interactions orthologs-direct-interactions]
  (let [all-proteins
         (into #{}
           (concat
               origin-proteins
             ;(trace-f "intr/get-proteins direct-interactions"
               (intr/get-proteins direct-interactions)
             ;(trace-f "mapcat intr/get-proteins othologs"
               (mapcat (fn [[organism orthologs interactions]]
                         (->> interactions
                              intr/get-proteins
                              (fetch/get-proteins-orthologs ref-organism)))
                      orthologs-direct-interactions)))
        secondary-interactions
          (fetch/get-secondary-interactions clients ref-organism all-proteins)]
    [all-proteins secondary-interactions]))

(s/fdef merge-proteins-and-get-secondary-interactions
  :args (s/cat :clients      (s/coll-of any?)
               :ref-organism ::orgn/organism
               :origin-proteins (s/coll-of ::prot/protein)
               :direct-interactions ::intr/interactions
               :orthologs-direct-interactions
                 (s/coll-of
                   (s/cat :organism ::orgn/organism
                          :orthologs (s/coll-of ::orthd/ortholog-scored-protein)
                          :interactions ::intr/interactions)))
  :ret  (s/cat  :all-proteins (s/coll-of ::prot/protein)
                :secondary-interactions ::intr/interactions))

(defn fetch-interactome [databases organism]
  (let [clients (fetch/get-clients databases)]
    (fetch/get-taxon-interactions clients organism)))

(s/fdef fetch-interactome
  :args (s/cat :databases (s/coll-of ::intr/database)
               :organism ::orgn/organism)
  :ret  ::intr/interactions)

(comment
  (binding [*print-level* 3]
    (let [organism (orgn/inparanoid-organism-by-shortname "C.elegans")]
      (println
        (take 2 (fetch-interactome ["IntAct"] organism))))))

(defn fetch-protein-network
  [databases ; PSICQUIC databases to query
   ref-organism  ; Organism of Interest
   proteins ; Proteins of Interest
   other-organisms] ; Other Organisms to check
  ;(trace-f "fetch-protein-network" [databases ref-organism proteins other-organisms])

  (let [clients (fetch/get-clients databases)

        ; Get Direct Interactions (left arrow)
        f-direct-interactions
          (future (fetch/get-direct-interactions
                    clients ref-organism proteins))

        ; Get proteins orthologs + Get direct interactors
        orthologs-direct-interactions
         ;(trace-f "orthologs-direct-interactions"
          ; potentially parallellized
          (map (fn [organism]
                  (get-orthologs-direct-interactions
                    clients organism proteins))
               other-organisms)

        direct-interactions
         ;(trace-f "direct-interactions"
           @f-direct-interactions
        ; Three blue arrows + left secondary interactions
        f-proteins-and-secondary-interactions
          (future ;(trace-f "merge-proteins-and-get-secondary-interactions"
                   (merge-proteins-and-get-secondary-interactions
                     clients ref-organism proteins
                     direct-interactions orthologs-direct-interactions))

        ; Two violet arrows + right secondary arrows
        orthologs-secondary-interactions
         ;(trace-f "orthologs-secondary-interactions"
           ; Potentially parallellized
           (mapcat (fn [[organism orthologs interactions]]
                       (get-orthologs-secondary-interactions
                         clients organism orthologs interactions))
                   orthologs-direct-interactions)
        ;return-proteins-orthologs-interactions
        ;   (back-to-ref-organism-orthologs-interaction)
        ; TODO orthologs-secondary-interactions->reference organism
        ; Deref future proteins-and-secondary-interactions
        [return-proteins secondary-interactions]
        @f-proteins-and-secondary-interactions]
        ; TODO Merge interactions for the return
    [direct-interactions secondary-interactions return-proteins orthologs-secondary-interactions]))


(s/fdef fetch-protein-network
  :args (s/cat :databases       (s/coll-of ::intr/database)
               :ref-organism    ::orgn/organism
               :proteins        (s/coll-of ::prot/protein)
               :other-organisms (s/coll-of ::orgn/organism))
  :ret  (s/cat :direct-interactions ::intr/interactions
               :secondary-interactions ::intr/interactions
               :return-proteins (s/coll-of ::prot/protein)
               :orthologs-secondary-interactions ::intr/interactions))
