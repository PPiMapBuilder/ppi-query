(ns ppi-query.interaction.miql
  (:require [clojure.spec :as s]))

; Clojure data structure representation of the MIQL query syntax
; Reference:
;   https://noedelta.gitbooks.io/psicquic-documentation/content/MiqlReference27.html

; MIQL search fields
(s/def ::field-name
  #{:idA :idB :id :alias :identifier :pubauth :pubid :taxidA
    :taxidB :species :type :detmethod :interaction_id
    :pbioroleA :pbioroleB :pbiorole :ptypeA :ptypeB :ptype
    :pxrefA :pxrefB :pxref :xref :annot :udate :negative
    :complex :ftypeA :ftypeB :ftype :pmethodA :pmethodB
    :pmethod :stc :param})

; MIQL term
; Examples: "A", [:or "A" "B"], [:and "A" "B"]
(s/def ::term
  (s/or :str string?
         :operand (s/cat :operator #{:or :and}
                         :terms (s/+ ::term))))

; MIQL field search ("field:value")
; Examples: [:id "A"], [:species [:or "B" "C"]]
(s/def ::field
  (s/cat :field-name ::field-name
         :value ::term))

; MIQL query
; Examples: "A", [:and "B" [:id "C"]], [:id "D"]
(s/def ::query
  (s/or :term ::term
         :field ::field
         ; resursivly nested sub-queries
         :operand (s/cat :operator #{:or :and}
                         :queries (s/+ ::query))))
