(ns ppi-query.protein.uniprot
         (:require [clojure.spec.alpha :as s]
                   [clojure.spec.gen.alpha :as gen]
                   [clojure.set :refer [union]]
                   [ppi-query.spec :as ps]))

; Official Uniprot ID regexp with optional revision suffix
(def uniprotid-regexp
  #"([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})([\-\.]\w+)?")

; strict Uniprot ID spec on sequence of characters
(s/def ::uniprotid-strict-seq
  (s/alt :0 (s/cat :0 #{\P \O \Q}
                   :1 ::ps/digit-char
                   :2 (ps/s-repeat ::ps/digit-maj-alpha-char 3)
                   :3 ::ps/digit-char)
         :1 (s/cat :0 (union ;[A-NR-Z]
                        ;[A-N]
                        (ps/char-range-set \A \N)
                        ;[R-Z]
                        (ps/char-range-set \R \Z))
                   :1 ::ps/digit-char
                   :2 (ps/s-repeat
                        (s/cat :0 ::ps/maj-alpha-char
                               :1 (ps/s-repeat ::ps/digit-maj-alpha-char 2)
                               :2 ::ps/digit-char)
                        1 2))))

; Uniprot ID spec on sequence of characters (with optional revision suffix)
(s/def ::uniprotid-seq
  (s/cat
    :0 ::uniprotid-strict-seq
    :1 (s/? (s/cat :0 #{\- \.}
                   :1 (s/+ ::ps/word-char)))))

; Strict Uniprot ID spec on strings
(s/def ::uniprotid-strict (ps/string-spec ::uniprotid-strict-seq))

; Uniprot ID spec on strings (with optional revision suffix)
(s/def ::uniprotid (ps/string-spec ::uniprotid-seq))

(defn get-strict-uniprotid [id]
  "Extract a strict UniProt id from an id potentially containing a
   UniProt revision number.

   Example: (get-strict-uniprotid \"P04040-1\") ;=> \"P04040\""
  (get (re-matches uniprotid-regexp id) 1))

(s/fdef get-strict-uniprotid
  :args (s/cat :id ::uniprotid)
  :ret ::uniprotid-strict)
