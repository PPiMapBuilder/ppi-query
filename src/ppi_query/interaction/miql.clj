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
