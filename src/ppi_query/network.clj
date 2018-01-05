(ns ppi-query.network
  (:require [clojure.spec.alpha :as s]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.transform :as intrt]
            [ppi-query.interaction.psicquic.registry :as reg]
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
  :args (s/cat :clients      ::intrd/clients
               :ortholog     ::orgn/organism
               :proteins     ::prot/proteins)
  :ret  (s/cat :ortholog       ::orgn/organism
               :ortholog-prots ::orthd/ortholog-scored-proteins
               :interactions   ::intrd/interactions))

(defn get-orthologs-direct-interactions
  [clients orthologs proteins]
   ; potentially parallellized
  (map (fn [ortholog]
          (get-ortholog-direct-interactions
            clients ortholog proteins))
       orthologs))

(s/fdef get-orthologs-direct-interactions
  :args (s/cat :clients      ::intrd/clients
               :orthologs    ::orgn/organisms
               :proteins     ::prot/proteins)
  :ret  (s/coll-of
          (s/cat :ortholog       ::orgn/organism
                 :ortholog-prots ::orthd/ortholog-scored-proteins
                 :interactions   ::intrd/interactions)))

(defn get-ortholog-secondary-interactions
  [clients ortholog ortholog-prots direct-interactions]
  (->> direct-interactions
       intrt/get-proteins ; Set of Proteins
       (#(disj % (into #{} ortholog-prots)))
       (fetch/get-secondary-interactions clients ortholog)))

(s/fdef get-ortholog-secondary-interactions
  :args (s/cat :clients        ::intrd/clients
               :ortholog       ::orgn/organism
               :ortholog-prots ::orthd/ortholog-scored-proteins
               :direct-interactions ::intrd/interactions)
  :ret  ::intrd/interactions)

(defn print-sec-ints
  [orth secondary-interactions]
  (println (:scientific-name orth) ":" (count secondary-interactions) "secondary interactions found.")
  secondary-interactions)
(s/fdef print-sec-ints
  :args (s/cat :ortholog       ::orgn/organism
               :secondary-interactions ::intrd/interactions)
  :ret  ::intrd/interactions)

(defn get-orthologs-secondary-interactions
  [clients orthologs-direct-interactions]
  ; Potentially parallellized
  (mapcat (fn [[ortholog ortholog-prots interactions]]
              (print-sec-ints ortholog
                (get-ortholog-secondary-interactions
                  clients ortholog ortholog-prots interactions)))
          orthologs-direct-interactions))

(s/fdef get-orthologs-secondary-interactions
  :args (s/cat :clients        ::intrd/clients
               :orthologs-direct-interactions
                 (s/coll-of
                   (s/cat :ortholog       ::orgn/organism
                          :ortholog-prots ::orthd/ortholog-scored-proteins
                          :interactions   ::intrd/interactions)))
  :ret  ::intrd/interactions)

(defn merge-orthologs-direct-secondary-interactions
    [orthologs-direct-interactions orthologs-secondary-interactions]
  (concat
    (mapcat (fn [[ortholog ortholog-prots interactions]]
                interactions)
            orthologs-direct-interactions)
    orthologs-secondary-interactions))

(s/fdef merge-orthologs-direct-secondary-interactions
  :args (s/cat :orthologs-direct-interactions
                 (s/coll-of
                   (s/cat :ortholog       ::orgn/organism
                          :ortholog-prots ::orthd/ortholog-scored-proteins
                          :interactions   ::intrd/interactions))
               :orthologs-direct-interactions
                 ::intrd/interactions)
  :ret  ::intrd/interactions)

(defn orthologs-interactions->ref-organism
    [ref-organism
     orthologs-direct-interactions
     orthologs-secondary-interactions]
  (intrt/proteins-interactions->prot-orths-interactions ref-organism
    (intrt/interactions->proteins-interactions
      (merge-orthologs-direct-secondary-interactions
        orthologs-direct-interactions
        orthologs-secondary-interactions))))

(s/fdef orthologs-interactions->ref-organism
  :args (s/cat :ref-organism                     ::orgn/organism
               :orthologs-direct-interactions
                 (s/coll-of
                   (s/cat :ortholog       ::orgn/organism
                          :ortholog-prots ::orthd/ortholog-scored-proteins
                          :interactions   ::intrd/interactions))
               :orthologs-secondary-interactions ::intrd/interactions)
  :ret  ::intrd/prot-orths-interactions)

(defn merge-proteins-and-get-secondary-interactions
    [clients ref-organism origin-proteins
     direct-interactions orthologs-direct-interactions]
  (let [all-proteins
         (into #{}
           (concat
             ;(trace-f "origin-proteins"
               origin-proteins
             ;(trace-f "intrt/get-proteins direct-interactions"
               (intrt/get-proteins direct-interactions)
             ;(trace-f "mapcat intrt/get-proteins othologs"
               (mapcat (fn [[ortholog ortholog-prots interactions]]
                         (->> interactions
                              intrt/get-proteins
                              (fetch/get-proteins-orthologs ref-organism)))
                      orthologs-direct-interactions)))
        secondary-interactions
         (fetch/get-secondary-interactions clients ref-organism all-proteins)]
    [all-proteins secondary-interactions]))

(s/fdef merge-proteins-and-get-secondary-interactions
  :args (s/cat :clients             ::intrd/clients
               :ref-organism        ::orgn/organism
               :origin-proteins     ::prot/proteins
               :direct-interactions ::intrd/interactions
               :orthologs-direct-interactions
                 (s/coll-of
                   (s/cat :organism       ::orgn/organism
                          :ortholog-prots ::orthd/ortholog-scored-proteins
                          :interactions   ::intrd/interactions)))
  :ret  (s/cat  :all-proteins           ::prot/proteins
                :secondary-interactions ::intrd/interactions))

(defn remove-duplicate-interactions
    [prot-orths-interactions]
  (let [prot-orths-multi-interactions
        (reduce
           (fn [prot-orths-multi-ints
                {:keys [protein-a ortholog-protein-a
                        protein-b ortholog-protein-b
                        original-interaction]}]
              (let [[uni-a orth-uni-a uni-b orth-uni-b]
                    (map :uniprotid
                         [protein-a ortholog-protein-a
                          protein-b ortholog-protein-b])
                    key-1 (apply str [uni-a orth-uni-a uni-b orth-uni-b])
                    key-2 (apply str [uni-b orth-uni-b uni-a orth-uni-a])]
                (if-let [prot-orths-multi-int (prot-orths-multi-ints key-1)]
                  (assoc prot-orths-multi-ints key-1
                    (intrd/add-interaction-to-prot-orths-multi-interactions
                      prot-orths-multi-int original-interaction))
                  (if-let [prot-orths-multi-int (prot-orths-multi-ints key-2)]
                    (assoc prot-orths-multi-ints key-2
                      (intrd/add-interaction-to-prot-orths-multi-interactions
                        prot-orths-multi-int original-interaction))
                    (assoc prot-orths-multi-ints key-1
                      (intrd/->ProtOrthsMultiInteraction
                         protein-a ortholog-protein-a
                         protein-b ortholog-protein-b
                         [original-interaction]))))))
           {}
           prot-orths-interactions)]

    (map val prot-orths-multi-interactions)))

(s/fdef remove-duplicate-interactions
  :args (s/cat :prot-orths-interactions ::intrd/prot-orths-interactions)
  :ret  ::intrd/prot-orths-multi-interactions)

(defn concat-and-format-all-interactions
    "Change all ref-organism interactions into prot-orths-interactions
     Concat all interactions
     Remove duplicate interactions"
    [orthologs-interactions-ref-organism
     ref-organism
     direct-interactions secondary-interactions]
  (remove-duplicate-interactions
    (concat
      orthologs-interactions-ref-organism
      (intrt/proteins-interactions->prot-orths-interactions ref-organism
        (intrt/interactions->proteins-interactions
           (concat direct-interactions secondary-interactions))))))


(s/fdef concat-and-format-all-interactions
  :args (s/cat :orthologs-interactions-ref-organism ::intrd/prot-orths-interactions
               :ref-organism                        ::orgn/organism
               :direct-interactions                 ::intrd/interactions
               :secondary-interactions              ::intrd/interactions)
  :ret  ::intrd/prot-orths-multi-interactions)

(defn fetch-interactome
  "Generate network for a full interactome"
  [databases organism]
  (let [clients (reg/get-clients databases)]
    (fetch/get-taxon-interactions clients organism)))

(s/fdef fetch-interactome
  :args (s/cat :databases ::intrd/databases
               :organism  ::orgn/organism)
  :ret  ::intrd/interactions)

(comment
  (binding [*print-level* 3]
    (let [organism (orgn/inparanoid-organism-by-shortname "C.elegans")]
      (println
        (take 2 (fetch-interactome ["IntAct"] organism))))))

(defn print-orth-dir-ints
  [[org orth-prots ints]]
  (println "===" (:scientific-name org) ":" (count orth-prots) "orthologs and" (count ints) "interactions found.")
  (count ints))

(defn fetch-protein-network
  "Generate the full protein network"
  [databases ; PSICQUIC databases to query
   ref-organism  ; Organism of Interest
   proteins ; Proteins of Interest
   other-organisms] ; Other Organisms to check
  ;(trace-f "fetch-protein-network" [databases ref-organism proteins other-organisms])

  (println ":: Fetch protein network::")
  (println "-- databases:" databases)
  (println "-- ref-organism:" ref-organism)
  (println "-- proteins:" proteins)
  (println "-- other-organisms:" other-organisms)
  (let [clients (reg/get-clients databases)
        ; Get Direct Interactions (left arrow)
        ; future ::intrd/interactions
        f-direct-interactions
          (future (fetch/get-direct-interactions
                    clients ref-organism proteins))

        ; Get proteins orthologs + Get direct interactors
        ; ::orgn/organism ::orthd/ortholog-scored-proteins ::intrd/interactions
        orthologs-direct-interactions
         ;(trace-f "orthologs-direct-interactions"
          (get-orthologs-direct-interactions
            clients other-organisms proteins)
        print-1
          (do (println "== orthologs-direct-interactions finished. Fetched : ")
              (dorun (map print-orth-dir-ints orthologs-direct-interactions)))
        ; ::intrd/interactions
        direct-interactions
         ;(trace-f "direct-interactions"
          @f-direct-interactions
        print-2
          (println "==" (count direct-interactions) "direct interactions fetched")
        ; Three blue arrows + left secondary interactions
        ; future ::prot/proteins ::intrd/interactions
        f-proteins-and-secondary-interactions
          (future ;(trace-f "merge-proteins-and-get-secondary-interactions"
                   (merge-proteins-and-get-secondary-interactions
                     clients ref-organism proteins
                     direct-interactions orthologs-direct-interactions))

        ; Two violet arrows + right secondary arrows
        ; ::intrd/interactions
        orthologs-secondary-interactions
         ;(trace-f "orthologs-secondary-interactions"
          (get-orthologs-secondary-interactions
             clients orthologs-direct-interactions)
        print-3
          (println "==" (count orthologs-secondary-interactions) "orthologs secondary interactions in total")

        ; Merge orthologs-direct-interactions and orthologs-secondary-interactions
        ;      and change into ::intrd/proteins-interactions
        ; Then return orthologs interactions to reference organism, in right format
        ; ::intrd/interactions ->
        ; ::intrd/proteins-interactions ->
        ; ::intrd/prot-orths-interactions
        orthologs-interactions-ref-organism
          (orthologs-interactions->ref-organism
            ref-organism
            orthologs-direct-interactions
            orthologs-secondary-interactions)
        print-4
          (println "== Orthologs interactions brought back to reference organism.")

        ; Deref future proteins-and-secondary-interactions
        ; ::prot/proteins ::intrd/interactions
        [return-proteins secondary-interactions]
        @f-proteins-and-secondary-interactions
        print-5
        (do (println "==" (count return-proteins) "proteins to display in total")
            (println "==" (count secondary-interactions) "secondary interactions in reference organism"))

        ; Change all ref-organism interactions into prot-orths-interactions
        ; Concat all interactions
        ; Remove duplicate interactions
        ; -> ::intrd/prot-orths-multi-interactions
        all-interactions-ref-organism
        (concat-and-format-all-interactions
          orthologs-interactions-ref-organism
          ref-organism
          direct-interactions secondary-interactions)
        print-6
        (println "==" (count all-interactions-ref-organism) "interactions to display in total")]


    [return-proteins all-interactions-ref-organism]))

(s/fdef fetch-protein-network
  :args (s/cat :databases       ::intrd/databases
               :ref-organism    ::orgn/organism
               :proteins        ::prot/proteins
               :other-organisms ::orgn/organisms)
  :ret  (s/cat :return-proteins               ::prot/proteins
               :all-interactions-ref-organism ::intrd/prot-orths-multi-interactions))



(defn fetch-protein-network-strings
    "Generate the full protein network (with simple string an int arguments)"
    [databases ; PSICQUIC databases to query
     ref-organism-name  ; Organism of Interest
     uniprotids ; Proteins of Interest
     other-organism-names] ; Other Organisms to check
  (let [ref-organism
          (orgn/inparanoid-organism-by-id-or-shortname ref-organism-name)
        proteins
          (map (partial prot/->Protein ref-organism)
               uniprotids)
        other-organisms
          (map orgn/inparanoid-organism-by-id-or-shortname other-organism-names)]
    (fetch-protein-network databases ref-organism proteins other-organisms)))

(s/fdef fetch-protein-network-strings
  :args (s/cat :databases            ::intrd/databases
               :ref-organism-name    (s/or :org-id int? :org-short-name string?)
               :uniprotids           string?
               :other-organism-names (s/coll-of (s/or :org-id int? :org-short-name string?)))
  :ret  (s/cat :return-proteins               ::prot/proteins
               :all-interactions-ref-organism ::intrd/prot-orths-multi-interactions))

(defn remove-proteins
  "Filter out a set of proteins from a generated network"
  [proteins-interactions ; Set of proteins and interactions of a network
   remove-proteins] ; Set of proteins to filter out
  (let [[proteins interactions] proteins-interactions
        remove-proteins-set (set remove-proteins)]
    [(remove (partial contains? remove-proteins-set) proteins)
     (remove (fn [{:keys [protein-a protein-b]}]
                 (or (contains? remove-proteins-set protein-a)
                     (contains? remove-proteins-set protein-b)))
             interactions)]))

(s/fdef remove-proteins
  :args (s/cat :proteins-interactions
                 (s/tuple ::prot/proteins ::intrd/prot-orths-multi-interactions)
               :remove-proteins  ::prot/proteins)
  :ret  (s/cat :filtered-proteins      ::prot/proteins
               :filtered-interactions  ::intrd/prot-orths-multi-interactions))
