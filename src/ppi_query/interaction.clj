(ns ppi-query.interaction
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)
           (psidev.psi.mi.tab PsimiTabReader))
  (:require [clojure.java.data :refer :all]
            [clojure.spec :as s]
            [ppi-query.protein :as prot]
            [ppi-query.protein.uniprot :as unip]
            [ppi-query.organism :as orgn]
            [ppi-query.interaction.miql :refer :all]))


(s/def ::identifier string?)
(s/def ::database string?)
(s/def ::identifiers (s/coll-of (s/keys :req-un [::identifier ::database])))

(s/def ::interactor (s/keys :req-un [::identifiers]))
(s/def ::interactorA ::interactor)
(s/def ::interactorB ::interactor)

(s/def ::interaction (s/keys :req-un [::interactorA ::interactorB]))
(s/def ::protein-couple (s/tuple ::prot/protein ::prot/protein))

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
   (let [result-stream (.getByQuery client query PsicquicSimpleClient/MITAB25 first-result max-results)
         result-java   (.read reader result-stream)
         result-clj    (from-java result-java)]
     result-clj)))

(s/fdef get-by-query
  :args (s/cat :client any? :query string?)
  :ret (s/coll-of ::interaction))

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  (to-miql (get-query-by-taxon 6239))]
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
          query  (to-miql (get-query-by-taxon 6239))]
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
  :args (s/cat :client any? :query string? :optional-pagesize pos-int?)
  :ret (s/coll-of ::interaction))

(defn merge-interactions [interactions]
  "Function to be implemented with mi-cluster or by hand"
  (apply concat interactions))

(s/fdef merge-interactions
  :args (s/cat :interactions (s/coll-of (s/coll-of ::interaction)))
  :ret  (s/coll-of ::interaction))

(defn fetch-by-query-all-clients [clients query]
  "Apply the same query on all the clients and merge the result in
   one list of interactions"
  (merge-interactions
    (map #(fetch-by-query % query)
         clients)))

(s/fdef fetch-by-query-all-clients
  :args (s/cat :clients (s/coll-of any?) :query string?)
  :ret  (s/coll-of ::interaction))

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
  (if-let [taxId (get-in interactor [:organism :taxid])]
    (-> taxId
        Integer/parseInt
        orgn/inparanoid-organism-by-id)))

(s/fdef get-interactor-uniprotid
  :args (s/cat :interactor ::interactor)
  :ret  ::orgn/organism)

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
  :ret (s/coll-of (s/or :nil nil? :prot ::prot/protein) :count 2))

(comment
  (binding [*print-level* 3]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 2
          (map get-interactors-proteins
            (get-by-query client query)))))))

(defn interactions->proteins-couples [interactions]
  (->> interactions
       (map get-interactors-proteins)
       (remove #(.contains % nil))))

(s/fdef interactions->proteins-couples
  :args (s/cat :interactions (s/coll-of ::interaction))
  :ret  (s/coll-of ::protein-couple))

(comment
  (binding [*print-level* 4]
    (let [client (first registry-clients)
          query  "P04040 or Q14145"]
      (println
        (take 2
          (interactions->proteins-couples
            (get-by-query client query)))))))

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

(defn get-proteins [interactions]
  (-> interactions
      interactions->proteins-couples
      proteins-couples->proteins-set))

(s/fdef get-proteins
  :args (s/cat :interactions (s/coll-of ::interaction))
  :ret  (s/coll-of ::protein :distinct true))
