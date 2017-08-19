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


(defn get-ortholog-direct-interactions
  [clients ortholog proteins]
  (let [ortholog-prots ; parallel to direct-interactions
         ;(trace-f "netwk/get-ortholog-direct-interactions orthologs"
          (into #{} (fetch/get-proteins-orthologs ortholog proteins))
          ; -> proteins
        interactions
         ;(trace-f "netwk/get-ortholog-direct-interactions interactions"
          (fetch/get-direct-interactions clients ortholog ortholog-prots)]
          ; -> interactions
    [ortholog ortholog-prots interactions]))

(s/fdef get-ortholog-direct-interactions
  :args (s/cat :clients      ::intr/clients
               :ortholog     ::orgn/organism
               :proteins     ::prot/proteins)
  :ret  (s/cat :ortholog       ::orgn/organism
               :ortholog-prots ::orthd/ortholog-sourced-proteins
               :interactions   ::intr/interactions))

(defn get-orthologs-direct-interactions
  [clients orthologs proteins]
  (map (fn [ortholog]
          (get-ortholog-direct-interactions
            clients ortholog proteins))
       orthologs))

(s/fdef get-orthologs-direct-interactions
  :args (s/cat :clients      ::intr/clients
               :orthologs    ::orgn/organisms
               :proteins     ::prot/proteins)
  :ret  (s/coll-of
          (s/cat :ortholog       ::orgn/organism
                 :ortholog-prots ::orthd/ortholog-sourced-proteins
                 :interactions   ::intr/interactions)))

(defn get-ortholog-secondary-interactions
  [clients ortholog ortholog-prots direct-interactions]
  (->> direct-interactions
       intr/get-proteins ; Set of Proteins
       (#(disj % (into #{} ortholog-prots)))
       (fetch/get-secondary-interactions clients ortholog)))

(s/fdef get-ortholog-secondary-interactions
  :args (s/cat :clients        ::intr/clients
               :ortholog       ::orgn/organism
               :ortholog-prots ::orthd/ortholog-sourced-proteins
               :direct-interactions ::intr/interactions)
  :ret  ::intr/interactions)

(defn get-orthologs-secondary-interactions
  [clients orthologs-direct-interactions]
  (mapcat (fn [[ortholog ortholog-prots interactions]]
              (get-ortholog-secondary-interactions
                clients ortholog ortholog-prots interactions))
          orthologs-direct-interactions))

(s/fdef get-orthologs-secondary-interactions
  :args (s/cat :clients        ::intr/clients
               :orthologs-direct-interactions
                 (s/coll-of
                   (s/cat :ortholog       ::orgn/organism
                          :ortholog-prots ::orthd/ortholog-sourced-proteins
                          :interactions   ::intr/interactions)))
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
               ; TODO : Use ortholog-prots to get back orgin prots
               (mapcat (fn [[ortholog ortholog-prots interactions]]
                         (->> interactions
                              intr/get-proteins
                              (fetch/get-proteins-orthologs ref-organism)))
                      orthologs-direct-interactions)))
        secondary-interactions
          (fetch/get-secondary-interactions clients ref-organism all-proteins)]
    [all-proteins secondary-interactions]))

(s/fdef merge-proteins-and-get-secondary-interactions
  :args (s/cat :clients             ::intr/clients
               :ref-organism        ::orgn/organism
               :origin-proteins     ::prot/proteins
               :direct-interactions ::intr/interactions
               :orthologs-direct-interactions
                 (s/coll-of
                   (s/cat :organism       ::orgn/organism
                          :ortholog-prots ::orthd/ortholog-scored-proteins
                          :interactions   ::intr/interactions)))
  :ret  (s/cat  :all-proteins           ::prot/proteins
                :secondary-interactions ::intr/interactions))

(defn fetch-interactome [databases organism]
  (let [clients (fetch/get-clients databases)]
    (fetch/get-taxon-interactions clients organism)))

(s/fdef fetch-interactome
  :args (s/cat :databases ::intr/databases
               :organism  ::orgn/organism)
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
        ; future ::intr/interactions
        f-direct-interactions
          (future (fetch/get-direct-interactions
                    clients ref-organism proteins))

        ; Get proteins orthologs + Get direct interactors
        ; ::orgn/organism ::orthd/ortholog-sourced-proteins ::intr/interactions
        orthologs-direct-interactions
         ;(trace-f "orthologs-direct-interactions"
          ; potentially parallellized
          (get-orthologs-direct-interactions
            clients other-organisms proteins)

        ; ::intr/interactions
        direct-interactions
         ;(trace-f "direct-interactions"
           @f-direct-interactions
        ; Three blue arrows + left secondary interactions
        ; future ::prot/proteins ::intr/interactions
        f-proteins-and-secondary-interactions
          (future ;(trace-f "merge-proteins-and-get-secondary-interactions"
                   (merge-proteins-and-get-secondary-interactions
                     clients ref-organism proteins
                     direct-interactions orthologs-direct-interactions))

        ; Two violet arrows + right secondary arrows
        ; ::intr/interactions
        orthologs-secondary-interactions
         ;(trace-f "orthologs-secondary-interactions"
           ; Potentially parallellized
           (get-orthologs-secondary-interactions orthologs-direct-interactions)
        ; TODO : Merge orthologs-direct-interactions and orthologs-secondary-interactions
        ;      and change into ::intrd/otholog-interactions
        ;ref-prot-orthologs-interactions
        ;   (back-to-ref-orthologs-interaction
        ;      ;orthologs-direct-interactions -> list orthologs -> parcours orth-sec-int -
        ;      orthologs-direct-interactions
        ;      orthologs-secondary-interactions)

        ; TODO : Change ::intrd/otholog-interactions
        ;       into    ::intrd/original-org-interactions

        ; Deref future proteins-and-secondary-interactions
        ; ::prot/proteins ::intr/interactions
        [return-proteins secondary-interactions]
        @f-proteins-and-secondary-interactions]

        ; TODO : Change ::intr/interactions
        ;       into    ::intrd/original-org-interactions

        ; TODO Merge interactions for the return
        ;      (all intrd/original-org-interactions)
    [direct-interactions secondary-interactions return-proteins orthologs-secondary-interactions]))


(s/fdef fetch-protein-network
  :args (s/cat :databases       ::intr/databases
               :ref-organism    ::orgn/organism
               :proteins        ::prot/proteins
               :other-organisms ::orgn/organisms)
  :ret  (s/cat :direct-interactions    ::intr/interactions
               :secondary-interactions ::intr/interactions
               :return-proteins        ::prot/proteins
               :orthologs-secondary-interactions ::intr/interactions))
