(ns ppi-query.interaction.miql
  (:require [clojure.spec :as s]
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
  :args (s/cat :taxId int?)
  :ret (s/coll-of ::query))

(comment
  (get-query-by-taxon 9606))

; [:and [:taxidA 9606] [:taxidB 9606] [:species 9606]]

(defn to-miql
  [query]
  (let [[fst & rest] query]
    (case fst
      (:or :and) (str "("
                      (clojure.string/join
                         ({:or " OR " :and " AND "} fst)
                         (map to-miql rest))
                      ")")
      (str (name fst) ":" (str (first rest))))))

(s/fdef to-miql
  :args (s/cat :query ::query)
  :ret (s/coll-of string?))

(comment
  (to-miql [:species 9606])
  (to-miql [:and [:id "value"] [:or [:idA "value2"] [:idB "value3"]]]))
  ; "(id:value AND (idA:value2 OR idB:value3))"
