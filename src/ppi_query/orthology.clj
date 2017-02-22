(ns ppi-query.orthology
  (:require [clojure.spec :as s]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.orthology.data :as orth]
            [ppi-query.orthology.cache :as cache]
            [ppi-query.orthology.inparanoid :as inparanoid]))

(defn get-ortholog-group [protein]
  "Get an ortholog group for a protein."
  (if-let [ortholog-group (cache/get-ortholog-group protein)]
    ortholog-group
    (when-let [ortholog-group (inparanoid/get-ortholog-group protein)]
      (cache/add-ortholog-group protein ortholog-group)
      ortholog-group)))

(s/fdef get-ortholog-group
  :args (s/cat :protein ::prot/protein)
  :ret ::orth/ortholog-group)

(defn get-best-orthologs [target-organism protein]
  "Get the best orthologs for a protein in a target organism."
  (let [ortholog-group (get-ortholog-group protein)
        orthologs (get ortholog-group target-organism)
        best-score (apply max (map :ortholog-score orthologs))]
    (filter #(= best-score (:ortholog-score %)) orthologs)))

(s/fdef get-best-orthologs
  :args (s/cat :target-organism ::org/organism :protein ::prot/protein)
  :ret (s/coll-of ::orth/ortholog-scored-protein))

(comment
  (let [human (org/inparanoid-organism-by-id 9606)
        mouse (org/inparanoid-organism-by-id 15368)
        catalase  (prot/->Protein human "P04040")]
    (get-best-orthologs mouse catalase)))
