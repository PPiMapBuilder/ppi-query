(ns ppi-query.orthology.cache
  (:require [clojure.spec :as s]
            [ppi-query.orthology.data :as orth]
            [ppi-query.organism :as org]
            [ppi-query.protein :as prot]
            [ppi-query.utils :as utils]))

; Ortholog group by protein by organism (protein's organism)
(s/def ::ortholog-cache
  (s/map-of ::org/organism
    (s/map-of ::prot/protein ::orth/ortholog-group)))

; Ortholog memory cache
(def ^:dynamic mem-cache (atom {}))

(defn get-ortholog-group [protein]
  "Get from memory cache an ortholog group for a protein."
  (let [organism (:organism protein)]
    (get-in @mem-cache [organism protein])))

(s/fdef get-ortholog-group
  :args (s/cat :protein ::prot/protein)
  :ret (s/nilable ::orth/ortholog-group))

(defn add-ortholog-group [protein ortholog-group]
  "Add to memory cache an ortholog group for a protein."
  (if-let [organism (:organism protein)]
    (swap!
      mem-cache
      assoc-in
      [organism protein]
      (utils/merge-distinct
        (get-ortholog-group protein)
        ortholog-group))))

(s/fdef add-ortholog-group
  :args (s/cat :protein ::prot/protein
               :ortholog-group ::orth/ortholog-group)
  :ret ::ortholog-cache)
