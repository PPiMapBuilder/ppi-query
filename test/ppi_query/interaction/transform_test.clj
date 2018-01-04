(ns ppi-query.interaction.transform-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.protein :as prot]
            [ppi-query.organism :as orgn]
            [ppi-query.orthology :as orth]
            [ppi-query.orthology.data :as orthd]
            [ppi-query.interaction.data :as intrd]
            [ppi-query.interaction.transform :as intrt]))
(stest/instrument)

(def ref-organism (orgn/inparanoid-organism-by-shortname "C.elegans"))
(def protein-1 (prot/->Protein ref-organism "Q18688"))
(def protein-2 (prot/->Protein ref-organism "Q20646"))
(def proteins [protein-1
               protein-2])
(def other-organism-1 (orgn/inparanoid-organism-by-shortname "M.musculus"))
(def other-organism-2 (orgn/inparanoid-organism-by-shortname "S.pombe"))
(def oprotein-1 (orthd/->OrthologScoredProtein other-organism-1 "Q9QY53" 0.6))
(def oprotein-2 (orthd/->OrthologScoredProtein other-organism-1 "O89019" 0.75))
(def other-organisms [other-organism-1
                      other-organism-2])

; See /interaction_example.txt for a full interaction
(def fake-interaction
       {:interactorA {:identifiers [{:identifier "Q18688" :database "titi"}
                                    {:identifier "Q20646" :database "uniprotkb"}]
                      :organism {:identifiers [] :taxid "6239"}}
        :interactorB {:identifiers [{:identifier "P00000" :database "db1"}
                                    {:identifier "P00001" :database "db2"}
                                    {:identifier "P04040" :database "uniprotkb"}
                                    {:identifier "P04041" :database "uniprotkb"}]
                      :organism {:identifiers [] :taxid "9606"}}})
(def fake-interaction-2
       {:interactorA {:identifiers [{:identifier "Q18688" :database "titi"}
                                    {:identifier "Q20646" :database "uniprotkb"}]
                      :organism {:identifiers [] :taxid "0"}}})
(def interactorA (:interactorA fake-interaction))
(def interactorB (:interactorB fake-interaction))
(def bad-interactor-1
       {:identifiers [{:identifier "Q18688" :database "titi"}
                      {:identifier "Q20646" :database "uniprotkb"}]
        :organism {:identifiers [] :taxid "0"}})
(def bad-interactor-2
       {:identifiers [{:identifier "Q18688" :database "titi"}
                      {:identifier "BAD0ID" :database "uniprotkb"}]
        :organism {:identifiers [] :taxid "6239"}})

(deftest test-fake-interaction
  (is (s/valid? ::intrd/interaction fake-interaction)))

(deftest test-get-interactor-database-ids
  (is (= (intrt/get-interactor-database-ids "db2" interactorB)
         '("P00001")))
  (is (thrown? Exception (intrt/get-interactor-database-ids "uniprotkb" bad-interactor-2))) ; TODO catch
  (is (= (intrt/get-interactor-database-ids "db3" interactorB)
         '()))
  (is (= (intrt/get-interactor-database-ids "uniprotkb" interactorB)
         '("P04040" "P04041"))))

(deftest test-get-interactor-uniprotid
  (is (contains? #{"P04040" "P04041"}
                 (intrt/get-interactor-uniprotid interactorB))))

(deftest test-get-interactor-organism
  (let [orgnB (intrt/get-interactor-organism interactorB)]
    (is (s/valid? ::orgn/organism orgnB))
    (is (= (:taxon-id orgnB)
           9606)))
  (is (= nil (intrt/get-interactor-organism bad-interactor-1))))

(deftest test-get-interactor-protein
  (let [protA (intrt/get-interactor-protein interactorA)]
    (is (s/valid? ::prot/protein protA))
    (is (= (:uniprotid protA)
           "Q20646")))
  (is (= nil (intrt/get-interactor-protein bad-interactor-1)))
  (is (thrown? Exception (intrt/get-interactor-protein bad-interactor-2)))) ; TODO catch

(deftest test-get-interactors-uniprotids
  (let [[unipA unipB] (intrt/get-interactors-uniprotids fake-interaction)]
    (is (contains? #{"Q20646"} unipA))
    (is (contains? #{"P04040" "P04041"} unipB))))

(deftest test-get-interactors-proteins
  (let [[protA protB] (intrt/get-interactors-proteins fake-interaction)]
    (is (= "Q20646" (:uniprotid protA)))
    (is (= 6239 (:taxon-id (:organism protA))))
    (is (= "P04040" (:uniprotid protB)))
    (is (= 9606 (:taxon-id (:organism protB)))))
  (let [[protA protB] (intrt/get-interactors-proteins {:interactorA bad-interactor-1 :interactorB interactorB})]
    (is (= nil protA))
    (is (= "P04040" (:uniprotid protB)))
    (is (= 9606 (:taxon-id (:organism protB))))))

(deftest test-get-interactors-proteins) ; TODO
(deftest test-interactions->proteins-couples) ; TODO
(deftest test-interactions->proteins-interactions) ; TODO
(deftest test-proteins-couples->proteins-set) ; TODO
(deftest test-interactions->proteins-set) ; TODO
(deftest test-get-proteins) ; TODO

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

  (is (s/valid? ::prot/protein oprotein-2))
  (let [protein-interaction
         (intrd/->ProteinsInteraction
           oprotein-1 oprotein-2 fake-interaction)]
    (let [[prot-orths-interaction]
          (intrt/proteins-interactions->prot-orths-interactions
             ref-organism
             [protein-interaction])]
     (let [{:keys [protein-a ortholog-protein-a
                   protein-b ortholog-protein-b
                   original-interaction]}
           prot-orths-interaction]
      (is (= (:organism protein-a) ref-organism))
      (is (= ortholog-protein-a oprotein-1))
      (is (= (:organism protein-b) ref-organism))
      (is (= ortholog-protein-b oprotein-2))
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
