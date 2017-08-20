(ns ppi-query.interaction
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader))
  (:require [clojure.java.data :refer :all]
            [clojure.spec :as s]
            [ppi-query.utils :refer :all]
            [ppi-query.protein :as prot]
            [ppi-query.protein.uniprot :as unip]
            [ppi-query.organism :as orgn]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.interaction.miql :as miql]))

(s/def ::identifier string?)
(s/def ::client any?)
(s/def ::clients (s/coll-of ::client))
(s/def ::database string?)
(s/def ::databases (s/coll-of ::database))
(s/def ::identifiers (s/coll-of (s/keys :req-un [::identifier ::database])))

(s/def ::interactor (s/keys :req-un [::identifiers]))
(s/def ::interactorA ::interactor)
(s/def ::interactorB ::interactor)

(s/def ::interaction (s/keys :req-un [::interactorA ::interactorB]))
(s/def ::interactions (s/coll-of ::interaction))
(s/def ::protein-couple (s/tuple ::prot/protein ::prot/protein))

(defrecord ProteinsInteraction [protein-a
                                protein-b
                                original-interaction])

(s/def ::protein-a ::prot/protein)
(s/def ::protein-b ::prot/protein)
(s/def ::original-interaction ::interaction)
(s/def ::proteins-interaction
  (s/keys :req-un [::protein-a
                   ::protein-b
                   ::original-interaction]))
(s/def ::proteins-interactions (s/coll-of ::proteins-interaction))

(s/def ::ortholog-protein-a (s/nilable ::prot/protein))
(s/def ::ortholog-protein-b (s/nilable ::prot/protein))
(defrecord ProtOrthsInteraction [protein-a
                                 ortholog-protein-a
                                 protein-b
                                 ortholog-protein-b
                                 original-interaction])
(s/def ::prot-orths-interaction
  (s/keys :req-un [::protein-a
                   ::ortholog-protein-a
                   ::protein-b
                   ::ortholog-protein-b
                   ::original-interaction]))
(s/def ::prot-orths-interactions (s/coll-of ::prot-orths-interaction))

(s/def ::original-interactions ::interactions)
(defrecord ProtOrthsMultiInteraction [protein-a
                                      ortholog-protein-a
                                      protein-b
                                      ortholog-protein-b
                                      original-interactions])
(s/def ::prot-orths-multi-interaction
  (s/keys :req-un [::protein-a
                   ::ortholog-protein-a
                   ::protein-b
                   ::ortholog-protein-b
                   ::original-interactions]))
(s/def ::prot-orths-multi-interactions (s/coll-of ::prot-orths-multi-interaction))

(defn add-interaction-to-prot-orths-multi-interactions
    [prot-orths-multi-interaction interaction]
  (let [{:keys [protein-a ortholog-protein-a
                protein-b ortholog-protein-b
                original-interactions]}
        prot-orths-multi-interaction]
    (->ProtOrthsMultiInteraction
        protein-a ortholog-protein-a
        protein-b ortholog-protein-b
        (conj original-interactions interaction))))

(s/fdef add-interaction-to-prot-orths-multi-interactions
  :args (s/cat :prot-orths-multi-interaction ::prot-orths-multi-interaction
               :interaction                  ::interaction)
  :ret  ::prot-orths-multi-interaction)

; List of APIs
(def registry
  [{:name "IntAct"
    :url  "http://www.ebi.ac.uk/Tools/webservices/psicquic/intact/webservices/current/search/"}])

; Psicquic Clients
(def registry-clients
  (map (fn [service]
         (new PsicquicSimpleClient (:url service)))
       registry))

; Psimi Reader
(def reader (new PsimiTabReader))

(defn get-by-query
  "Get lazy sequence of interactions by query (with psicquic client)"
  ([client query]
   (let [result-stream (.getByQuery client query)
         result-java   (.read reader result-stream)
         result-clj    (from-java result-java)]
     result-clj))
  ([client query max-results first-result]
   (let [result-stream (.getByQuery client query PsicquicSimpleClient/MITAB25
                                    first-result max-results)
         result-java   (.read reader result-stream)
         result-clj    (from-java result-java)]
     result-clj)))

(s/fdef get-by-query
  :args (s/cat :client any? :query string?
               :max-results (s/? pos-int?)
               :first-result (s/? int?))
  :ret ::interactions)

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  (miql/to-miql (miql/get-query-by-taxon 6239))]
      (println (take 2 (get-by-query client query)))))
  (get-by-query (first registry-clients) "taxidA:6239 AND taxidB:6239 AND species:6239"))
;({:detectionMethods (#), :updateDate (), :publications (# #),
;  :negativeInteraction false, :xrefs (), :checksums (),
;  :interactorA {
;    :features #, :organism #, :alternativeIdentifiers #,
;    :identifiers #, :xrefs #, :checksums #, :interactorTypes #,
;    :stoichiometry #, :empty false, :participantIdentificationMethods #,
;    :annotations #, :aliases #, :experimentalRoles #, :biologicalRoles #
;  },
;  :interactorB {
;    :features #, ###},
;  :complexExpansion (), :interactionAcs (# #),
;  :annotations (), :authors (#), :parameters (), :confidenceValues (#),
;  :creationDate (), :sourceDatabases (#), :interactionTypes (#), :hostOrganism nil
;}
;{:detectionMethods (#), ###})

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          ;query  "P04040 or Q14145"
          query  (miql/to-miql (miql/get-query-by-taxon 6239))]
      (println (.countByQuery client query))
      (println (take 10 (get-by-query client query))))))

; https://github.com/PSICQUIC/psicquic-simple-client/blob/master/src/example/java/org/hupo/psi/mi/psicquic/wsclient/PsicquicSimpleExampleLimited.java
(defn fetch-by-query
  "Handle pagination doing get-by-query (with psicquic client)"
  ([client query] (fetch-by-query client query 100))
  ([client query pagesize]
   (mapcat (partial get-by-query client query pagesize)
      (range 0 (.countByQuery client query) pagesize))))

(s/fdef fetch-by-query
  :args (s/cat :client any? :query string? :optional-pagesize (s/? pos-int?))
  :ret ::interactions)

(defn merge-interactions [interactions]
  "Function to be implemented with mi-cluster or by hand"
  (apply concat interactions))

(s/fdef merge-interactions
  :args (s/cat :interactions (s/coll-of ::interactions))
  :ret  ::interactions)

(defn fetch-by-query-all-clients [clients query]
  "Apply the same query on all the clients and merge the result in
   one list of interactions"
  (merge-interactions
    (map #(fetch-by-query % query)
         clients)))

(s/fdef fetch-by-query-all-clients
  :args (s/cat :clients (s/coll-of any?) :query string?)
  :ret  ::interactions)

(comment
  (binding [*print-level* 3]
    (let [query " ( taxidA:9606 AND taxidB:9606 AND species:9606 AND id:P04040 ) "]
      (println
        (take 2 (fetch-by-query-all-clients registry-clients query))))))


(defn get-interactor-database-ids [database interactor]
  "Get interactor identifiers for a specific database"
  (->> interactor
    :identifiers
    (filter #(= (:database %) database))
    (map :identifier)
    (remove nil?)
    (map unip/get-strict-uniprotid)))

(s/fdef get-interactor-database-ids
  :args (s/cat :database ::database :interactor ::interactor)
  :ret (s/coll-of ::unip/uniprotid-strict))

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040"]
      (println
        (get-interactor-database-ids
          "uniprotkb"
          (:interactorA
            (first (get-by-query client query))))))))
; (P04040)


(def get-interactor-uniprotid
  (comp first (partial get-interactor-database-ids "uniprotkb")))

(s/fdef get-interactor-uniprotid
  :args (s/cat :interactor ::interactor)
  :ret  ::unip/uniprotid-strict)

(defn get-interactor-organism [interactor]
  (when-let [taxId (get-in interactor [:organism :taxid])]
    (when (s/valid? ::orgn/taxon-id (Integer/parseInt taxId))
      (-> taxId
          Integer/parseInt
          orgn/inparanoid-organism-by-id))))

(s/fdef get-interactor-uniprotid
  :args (s/cat :interactor ::interactor)
  :ret  (s/nilable ::orgn/organism))

(defn get-interactor-protein [interactor]
  "Get interactor identifiers for a specific database"
  (if interactor
    (let [organism  (get-interactor-organism  interactor)
          uniprotid (get-interactor-uniprotid interactor)]
      (if (and organism uniprotid)
        (prot/->Protein organism uniprotid)))))


(s/fdef get-interactor-protein
  :args (s/cat :interactor ::interactor)
  :ret  ::prot/protein)

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040"]
      (println
        (get-interactor-protein
          (-> (get-by-query client query)
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
  :args (s/cat :interaction ::interaction)
  :ret (s/coll-of ::identifier))

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 6
          (map get-interactors-uniprotids
            (get-by-query client query)))))))

; ([P04040 Q14145] [P04040 P29991-PRO_0000037946] [P04040 P04040]
;  [B4DYC6 Q14145] [Q14145 Q8IVD9] [Q14145 Q96BE0] ###)


(def get-interactors-proteins
  (juxt
    (comp get-interactor-protein :interactorA)
    (comp get-interactor-protein :interactorB)))

(s/fdef get-interactors-proteins
  :args (s/cat :interaction ::interaction)
  :ret (s/coll-of (s/nilable ::prot/protein) :count 2))

(comment
  (binding [*print-level* 5]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 2
          (map get-interactors-proteins
            (get-by-query client query)))))))

(defn interactions->proteins-couples [interactions]
  (->> interactions
       (map get-interactors-proteins)
       (remove (partial some nil?))))

(s/fdef interactions->proteins-couples
  :args (s/cat :interactions ::interactions)
  :ret  (s/coll-of ::protein-couple))

(comment
  (binding [*print-level* 4]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 2
          (interactions->proteins-couples
            (get-by-query client query)))))))

(defn interactions->proteins-interactions [interactions]
  (remove nil?
    (map (fn [interaction]
             (let [[prot1 prot2] (get-interactors-proteins interaction)]
                  (if (and prot1 prot2)
                    (->ProteinsInteraction
                       prot1 prot2 interaction))))
         interactions)))

(s/fdef interactions->proteins-interactions
  :args (s/cat :interactions ::interactions)
  :ret  ::proteins-interactions)

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 2
          (interactions->proteins-interactions
            (get-by-query client query)))))))

(defn orth-prot->ref-organism
    [ref-organism prot]
  (if (= ref-organism (:organism prot))
     [prot nil]
     [(-> (orth/get-best-orthologs ref-organism prot)
          first
          orthd/ortholog-scored->protein)
      prot]))

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
               (->ProtOrthsInteraction
                  prot-a orth-prot-a prot-b orth-prot-b original-interaction))))
         prot-interactions)))

(s/fdef proteins-interactions->prot-orths-interactions
  :args (s/cat :ref-organism      ::orgn/organism
               :prot-interactions ::proteins-interactions)
  :ret  ::prot-orths-interactions)

(defn proteins-couples->proteins-set [proteins-couples]
  (->> proteins-couples
       (apply concat)
       (into #{})))

(s/fdef proteins-couples->proteins-set
  :args (s/cat :protein-couples (s/coll-of ::protein-couple))
  :ret  (s/coll-of ::protein :ditinct true))

(comment
  (binding [*print-level* 4]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 10
          (proteins-couples->proteins-set
            (interactions->proteins-couples
              (get-by-query client query))))))))

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
  :args (s/cat :interactions ::interactions)
  :ret  (s/coll-of ::protein :distinct true))
