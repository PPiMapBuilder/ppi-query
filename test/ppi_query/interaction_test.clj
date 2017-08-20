(ns ppi-query.interaction-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein :as prot]
            [ppi-query.organism :as orgn]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.query :as intrq]
            [ppi-query.interaction.transform :as intrt]
            [ppi-query.interaction.miql :as miql]))
(stest/instrument)

(defn get-edges [interactions]
  (->> interactions
    (map intrt/get-interactors-uniprotids)
    (remove #(contains? % nil?))))

(def ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans"))
(def protein-1 (prot/->Protein ref-organism "Q18688"))
(def protein-2 (prot/->Protein ref-organism "Q20646"))
(def proteins [protein-1
               protein-2])
(def other-organism-1 (orgn/inparanoid-organism-by-shortname "M.musculus"))
(def other-organism-2 (orgn/inparanoid-organism-by-shortname "S.pombe"))
(def other-organisms [other-organism-1
                      other-organism-2])
(def fake-interaction
       {:interactorA {:identifiers [{:identifier "toto" :database "titi"}]}
        :interactorB {:identifiers [{:identifier "toto" :database "titi"}]}})

(deftest test-orth-prot->ref-organism
  (let [[prot orth-prot]
        (intrt/orth-prot->ref-organism ref-organism protein-1)]
    (is (= prot      protein-1))
    (is (= orth-prot nil)))
  (let [[prot orth-prot]
        (intrt/orth-prot->ref-organism ref-organism protein-2)]
    (is (= prot      protein-2))
    (is (= orth-prot nil)))
  (let [[prot orth-prot]
        (intrt/orth-prot->ref-organism other-organism-1 protein-1)]
    (is (= (:organism prot) other-organism-1))
    (is (= orth-prot protein-1)))
  (let [[prot orth-prot]
        (intrt/orth-prot->ref-organism other-organism-2 protein-1)]
    (is (= (:organism prot) other-organism-2))
    (is (= orth-prot protein-1)))
  (let [[prot orth-prot]
        (intrt/orth-prot->ref-organism other-organism-1 protein-2)]
    (is (= prot      nil))
    (is (= orth-prot protein-2)))
  (let [[prot orth-prot]
        (intrt/orth-prot->ref-organism other-organism-2 protein-2)]
    (is (= prot      nil))
    (is (= orth-prot protein-2))))

(deftest test-proteins-interactions->prot-orths-interactions
  (is (s/valid? ::intrd/interaction fake-interaction))

  (let [protein-interaction
         (intrd/->ProteinsInteraction
           protein-1 protein-2 fake-interaction)]
    (let [[prot-orths-interaction]
          (intrt/proteins-interactions->prot-orths-interactions
             ref-organism
             [protein-interaction])]
     (let [{:keys [protein-a ortholog-protein-a
                   protein-b ortholog-protein-b
                   original-interaction]}
           prot-orths-interaction]
      (is (= protein-a          protein-1))
      (is (= ortholog-protein-a nil))
      (is (= protein-b          protein-2))
      (is (= ortholog-protein-b nil))
      (is (= original-interaction fake-interaction))
      (is (s/valid? ::intrd/prot-orths-interaction prot-orths-interaction)))))

  (let [prot-orths-interaction
         (intrd/->ProteinsInteraction
           protein-1 protein-1 fake-interaction)]
    (let [[prot-orths-interaction]
          (intrt/proteins-interactions->prot-orths-interactions
             other-organism-1
             [prot-orths-interaction])]
     (let [{:keys [protein-a ortholog-protein-a
                   protein-b ortholog-protein-b
                   original-interaction]}
           prot-orths-interaction]
      (is (= (:organism protein-a) other-organism-1))
      (is (= ortholog-protein-a protein-1))
      (is (= (:organism protein-b) other-organism-1))
      (is (= ortholog-protein-b protein-1))
      (is (= original-interaction fake-interaction))
      (is (s/valid? ::intrd/prot-orths-interaction prot-orths-interaction)))))

  (let [prot-orths-interaction
         (intrd/->ProteinsInteraction
           protein-1 protein-2 fake-interaction)]
    (let [prot-orths-interactions
          (intrt/proteins-interactions->prot-orths-interactions
             other-organism-1
             [prot-orths-interaction])]
      (is (= [] prot-orths-interactions)))))

(comment
  (require '[proto-repl-charts.graph :as g]
    (let [client (first intrq/registry-clients)
          query "P04040 or Q14145"
          ;query  (miql/to-miql (miql/get-query-by-taxon 6239))
          interactions (intrq/fetch-by-query client query)
          edges (get-edges interactions)
          nodes-simple (remove nil? (into #{} (flatten edges)))
          nodes (map #(hash-map :id % :label % :group (- (int (first %)) (int \A)))
                 nodes-simple)]
      ;(take 10 nodes)

      (g/graph
        "Psicquic interactions"
        {:nodes nodes :edges edges}))))

(comment)
(deftest test-fetch-by-query
  (let [client (first intrq/registry-clients)
        query  "P04040 or Q14145"
        getby  (intrq/get-by-query client query)]
    (is (= getby
           (intrq/fetch-by-query client query)))
    (is (= getby
           (intrq/fetch-by-query client query 50)))))
