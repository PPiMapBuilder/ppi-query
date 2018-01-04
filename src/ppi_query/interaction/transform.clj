(ns ppi-query.interaction.transform
  (:require [clojure.spec.alpha :as s]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.protein :as prot]
            [ppi-query.protein.uniprot :as unip]
            [ppi-query.organism :as orgn]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd]))

(def test-client (reg/get-client "IntAct"))

(defn get-interactor-database-ids [database interactor]
  "Get interactor identifiers for a specific database"
  (->> interactor
    :identifiers
    (filter #(= (:database %) database))
    (map :identifier)
    (remove nil?)
    (map unip/get-strict-uniprotid)))

(s/fdef get-interactor-database-ids
  :args (s/cat :database ::intrd/database :interactor ::intrd/interactor)
  :ret (s/coll-of ::unip/uniprotid-strict))

(def get-interactor-uniprotid
  (comp first (partial get-interactor-database-ids "uniprotkb")))

(s/fdef get-interactor-uniprotid
  :args (s/cat :interactor ::intrd/interactor)
  :ret  ::unip/uniprotid-strict)

(defn get-interactor-organism [interactor]
  (when-let [taxId (get-in interactor [:organism :taxid])]
    (when (s/valid? ::orgn/taxon-id (Integer/parseInt taxId))
      (-> taxId
          Integer/parseInt
          orgn/inparanoid-organism-by-id))))

(s/fdef get-interactor-uniprotid
  :args (s/cat :interactor ::intrd/interactor)
  :ret  (s/nilable ::orgn/organism))

(defn get-interactor-protein [interactor]
  "Get interactor identifiers for a specific database"
  (if interactor
    (let [organism  (get-interactor-organism  interactor)
          uniprotid (get-interactor-uniprotid interactor)]
      (if (and organism uniprotid)
        (prot/->Protein organism uniprotid)))))


(s/fdef get-interactor-protein
  :args (s/cat :interactor ::intrd/interactor)
  :ret  ::prot/protein)

(comment
  (binding [*print-level* 3]
    (let [query  "P04040"]
      (println
        (get-interactor-protein
          (-> (get-by-query test-client query)
              first
              :interactorA))))))

; #ppi_query.protein.Protein{
;   :organism #ppi_query.organism.Organism{
;      :common-name Human, :taxon-id 9606, :scientific-name Homo sapiens}
;   :uniprotid P04040}

(def get-interactors-uniprotids
  (juxt
    (comp get-interactor-uniprotid :interactorA)
    (comp get-interactor-uniprotid :interactorB)))

(s/fdef get-interactors-uniprotids
  :args (s/cat :interaction ::intrd/interaction)
  :ret (s/coll-of ::intrd/identifier))

(comment
  (binding [*print-level* 3]
    (let [query  "P04040 or Q14145"]
      (println
        (take 6
          (map get-interactors-uniprotids
            (get-by-query test-client query)))))))

; ([P04040 Q14145] [P04040 P29991-PRO_0000037946] [P04040 P04040]
;  [B4DYC6 Q14145] [Q14145 Q8IVD9] [Q14145 Q96BE0] ###)


(def get-interactors-proteins
  (juxt
    (comp get-interactor-protein :interactorA)
    (comp get-interactor-protein :interactorB)))

(s/fdef get-interactors-proteins
  :args (s/cat :interaction ::intrd/interaction)
  :ret (s/coll-of (s/nilable ::prot/protein) :count 2))

(comment
  (binding [*print-level* 5]
    (let [query  "P04040 or Q14145"]
      (println
        (take 2
          (map get-interactors-proteins
            (get-by-query test-client query)))))))

(defn interactions->proteins-couples [interactions]
  (->> interactions
       (map get-interactors-proteins)
       (remove (partial some nil?))))

(s/fdef interactions->proteins-couples
  :args (s/cat :interactions ::intrd/interactions)
  :ret  (s/coll-of ::intrd/protein-couple))

(comment
  (binding [*print-level* 4]
    (let [query  "P04040 or Q14145"]
      (println
        (take 2
          (interactions->proteins-couples
            (get-by-query test-client query)))))))

(defn interactions->proteins-interactions [interactions]
  (remove nil?
    (map (fn [interaction]
             (let [[prot1 prot2] (get-interactors-proteins interaction)]
                  (if (and prot1 prot2)
                    (intrd/->ProteinsInteraction
                       prot1 prot2 interaction))))
         interactions)))

(s/fdef interactions->proteins-interactions
  :args (s/cat :interactions ::intrd/interactions)
  :ret  ::intrd/proteins-interactions)

(comment
  (binding [*print-level* 3]
    (let [query  "P04040 or Q14145"]
      (println
        (take 2
          (interactions->proteins-interactions
            (get-by-query test-client query)))))))

(defn orth-prot->ref-organism
    [ref-organism prot]
  (if (= ref-organism (:organism prot))
     [prot nil]
     (let [ref-prot (first (orth/get-best-orthologs ref-organism prot))]
          [(orthd/ortholog-scored->protein ref-prot)
           (orthd/protein->ortholog-scored prot (:ortholog-score ref-prot))])))

(s/fdef orth-prot->ref-organism
  :args (s/cat :ref-organism ::orgn/organism
               :prot         ::prot/protein)
  :ret  (s/cat :prot      ::prot/protein
               :orth-prot (s/nilable ::prot/protein)))

(defn proteins-interactions->prot-orths-interactions
    [ref-organism prot-interactions]
  (remove nil?
    (map (fn [{:keys [protein-a protein-b original-interaction]}]
           (let [[prot-a orth-prot-a]
                 (orth-prot->ref-organism ref-organism protein-a)
                 [prot-b orth-prot-b]
                 (orth-prot->ref-organism ref-organism protein-b)]
             (if (and prot-a prot-b)
               (intrd/->ProtOrthsInteraction
                  prot-a orth-prot-a prot-b orth-prot-b original-interaction))))
         prot-interactions)))

(s/fdef proteins-interactions->prot-orths-interactions
  :args (s/cat :ref-organism      ::orgn/organism
               :prot-interactions ::intrd/proteins-interactions)
  :ret  ::intrd/prot-orths-interactions)

(defn proteins-couples->proteins-set [proteins-couples]
  (->> proteins-couples
       (apply concat)
       (into #{})))

(s/fdef proteins-couples->proteins-set
  :args (s/cat :protein-couples (s/coll-of ::intrd/protein-couple))
  :ret  (s/coll-of ::prot/protein :ditinct true))

(comment
  (binding [*print-level* 4]
    (let [query  "P04040 or Q14145"]
      (println
        (take 10
          (proteins-couples->proteins-set
            (interactions->proteins-couples
              (get-by-query test-client query))))))))

; Get proteins from all interactions (including wrong ones)
(defn interactions->proteins-set [interactions]
  (->> interactions
       (map get-interactors-proteins)
       (apply concat)
       (into #{})
       (remove nil?)))

; Get proteins only for interactions between two defined proteins
(defn get-proteins [interactions]
  (-> interactions
      interactions->proteins-couples
      proteins-couples->proteins-set))

(s/fdef get-proteins
  :args (s/cat :interactions ::intrd/interactions)
  :ret  (s/coll-of ::prot/protein :distinct true))
