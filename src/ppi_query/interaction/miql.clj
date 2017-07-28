(ns ppi-query.interaction.miql
  (:require [clojure.spec :as s]
            [ppi-query.protein.uniprot :as uni]
            [ppi-query.organism :as org]
            [ppi-query.spec :refer [def-lucene-syntax]]))

; Clojure data structure representation of the MIQL query syntax
; Reference:
;   https://noedelta.gitbooks.io/psicquic-documentation/content/MiqlReference27.html

; MIQL search fields
(def miql-fields
  #{:idA :idB :id :alias :identifier :pubauth :pubid :taxidA
    :taxidB :species :type :detmethod :interaction_id
    :pbioroleA :pbioroleB :pbiorole :ptypeA :ptypeB :ptype
    :pxrefA :pxrefB :pxref :xref :annot :udate :negative
    :complex :ftypeA :ftypeB :ftype :pmethodA :pmethodB
    :pmethod :stc :param})

(def-lucene-syntax ::query miql-fields)

(defn get-query-by-taxon
  "Returns query filtering interaction for which the taxonomic ID of the two proteins and
   the species is the `taxId` parameter"
  [taxId]
  [:and [:taxidA taxId] [:taxidB taxId] [:species taxId]])

(s/fdef get-query-by-taxon
  :args (s/cat :taxId ::org/taxon-id)
  :ret ::query)

(defn get-query-by-taxon-and-prots
  "Same as above but with restriction on interactor identifiers"
  [taxId protIds]
  [:and [:taxidA taxId] [:taxidB taxId] [:species taxId]
    (concat [:or]
      (map #(vector :id %)
           protIds))])

(s/fdef get-query-by-taxon-and-prots
  :args (s/cat :taxId ::org/taxon-id
               :protIds (s/coll-of ::uni/uniprotid))
  :ret ::query)

(defn get-queries-by-taxon-and-prot-couples
  "Generate a query with taxId restrictions and matching couples of protIds"
  [taxId protCouples]
  [:and [:taxidA taxId] [:taxidB taxId] [:species taxId]
    (concat [:or]
      (map (fn [[a b]] [:and [:idA a] [:idB b]])
           protCouples))])

(s/fdef get-queries-by-taxon-and-prot-couples
  :args (s/cat :taxId ::org/taxon-id
               :protCouples
                 (s/coll-of
                   (s/tuple ::uni/uniprotid ::uni/uniprotid)))
  :ret ::query)

(defn get-queries-by-taxon-and-prot-pool
  "A query should be less than 1000 characters so we have to split it
  into a list of queries.
  The queries have the same taxid restrictions
  but different interactor identifier restrictions (in \"or\" section).
  limit is the number of couples in \"or\" section."
  ([taxId protPool] (get-queries-by-taxon-and-prot-pool taxId protPool 100))
  ([taxId protPool limit]
   (map (partial get-queries-by-taxon-and-prot-couples taxId)
      (partition limit limit nil
        (for [x protPool, y protPool] [x y])))))

(s/fdef get-queries-by-taxon-and-prot-pool
  :args (s/cat :taxId    ::org/taxon-id
               :protPool (s/coll-of ::uni/uniprotid :distinct true)
               :limit    (s/? pos-int?))
  :ret (s/coll-of ::query))

(defn to-miql
  [query]
  (let [[fst & rest] query]
    (case fst
      (:or :and) (str " ( "
                      (clojure.string/join
                         ({:or " OR " :and " AND "} fst)
                         (map to-miql rest))
                      " ) ")
      (str
        (name fst) ":"
        (let [val (first rest)]
          (if (string? val)
            (str \" val \")
            (str val)))))))

(s/fdef to-miql
  :args (s/cat :query ::query)
  :ret string?)

(comment
  (get-query-by-taxon 9606)
  ; [:and [:taxidA 9606] [:taxidB 9606] [:species 9606]]


  (get-query-by-taxon-and-prots 9606 ["P04040"])
  (get-query-by-taxon-and-prots 9606 ["P04040" "O64HD2" "J1D0B7FO54"])
  ; [:and [:taxidA 9606] [:taxIdB 9606] [:species 9606] [:or [:idA "P04040"] [:idB "P04040"]]]

  (get-queries-by-taxon-and-prot-pool 9606 ["P04040" "Q9D2V5"] 2)
  (let [protPool ["P04040" "Q9D2V5"]]
    (partition 2 (for [x protPool, y protPool] [x y])))

  (to-miql (get-queries-by-taxon-and-prot-couples 9606 [["P04040" "Q9D2V5"] ["Q9D2V5" "P04040"]]))
  ;" ( taxidA:9606 AND taxidB:9606 AND species:9606 AND  (  ( idA:P04040 AND idB:Q9D2V5 )  OR  ( idA:Q9D2V5 AND idB:P04040 )  )  ) "

  (to-miql [:species 9606])
  (to-miql [:and [:id "value"] [:or [:idA "value2"] [:idB "value3"]]]))
  ; "(id:value AND (idA:value2 OR idB:value3))"
