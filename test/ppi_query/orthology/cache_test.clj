(ns ppi-query.orthology.cache-test
  (:require [clojure.test :refer :all]
            [ppi-query.orthology.cache :as cache]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.orthology.data :as orth]
            [ppi-query.test.utils :refer :all]
            [clojure.spec.test.alpha :as stest]))

(deftest* test-inparanoid-cache
  ; Bind a new cache
  (binding [cache/mem-cache (atom {})]
    (let [human (org/inparanoid-organism-by-shortname "H.sapiens")
          catalase-human (prot/->Protein human "P04040")

          mouse (org/inparanoid-organism-by-shortname "M.musculus")
          catalase-mouse (orth/->OrthologScoredProtein mouse "P24270" 1.0)
          group1 {mouse [catalase-mouse]}

          guineapig (org/inparanoid-organism-by-shortname "C.porcellus")
          catalase-guineapig1 (orth/->OrthologScoredProtein guineapig "H0VLR7" 1.0)
          catalase-guineapig2 (orth/->OrthologScoredProtein guineapig "Q64405" 0.98)
          group2 {guineapig [catalase-guineapig1 catalase-guineapig2]}

          merged-group (merge group1 group2)]

      (testing "Ortholog cache should be empty at first"
               (is (nil? (cache/get-ortholog-group catalase-human))))

      ; add a mouse ortholog
      (cache/add-ortholog-group catalase-human group1)

      (testing "Ortholog cache should contain an ortholog group for catalase from
                human to mouse"
              (is (= (cache/get-ortholog-group catalase-human)
                     group1)))

      ; add two guinea pig orthologs
      (cache/add-ortholog-group catalase-human group2)

      (testing "Ortholog cache should contain an ortholog group for catalase from
                human to mouse and guinea pig"
              (is (= (cache/get-ortholog-group catalase-human)
                     merged-group))))))

