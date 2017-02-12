(ns ppi-query.protein.uniprot
         (:require [clojure.spec :as s]
                   [clojure.spec.gen :as gen]
                   [clojure.set :refer [union]]
                   [ppi-query.spec :as ps]))

; Official Uniprot ID regexp with optional revision suffix
(def uniprotid-regexp
  #"([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})([\-\.]\w+)?")

; strict Uniprot ID spec on sequence of characters
(s/def ::uniprotid-strict-seq
  (s/alt :0 (s/cat :0 #{\P \O \Q}
                   :1 ps/digit-set
                   :2 (ps/s-repeat ps/digit-alpha-set 3)
                   :3 ps/digit-set)
         :1 (s/cat :0 (union ;[A-NR-Z]
                        ;[A-N]
                        (ps/char-range-set \A \N)
                        ;[R-Z]
                        (ps/char-range-set \R \Z))
                   :1 ps/digit-set
                   :2 (ps/s-repeat
                        (s/cat :0 ps/alpha-set
                               :1 (ps/s-repeat ps/digit-alpha-set 2)
                               :2 ps/digit-set)
                        1 2))))

; Uniprot ID spec on sequence of characters (with optional revision suffix)
(s/def ::uniprotid-seq
  (s/cat
    :0 ::uniprotid-strict-seq
    :1 (s/? (s/cat :0 #{\- \.}
                   :1 (s/+ ps/word-set)))))

(def str-to-seq seq)
(def seq-to-str (partial apply str))

; strict Uniprot ID spec on strings
(s/def ::uniprotid-strict
  (s/with-gen
    (s/and
      string?
      ; if string: convert to seq
      (s/conformer str-to-seq seq-to-str)
      ; unirpot id RegExp
      ::uniprotid-strict-seq)
    #(gen/fmap seq-to-str (s/gen ::uniprotid-strict-seq))))

; Uniprot ID spec on strings (with optional revision suffix)
(s/def ::uniprotid
  (s/with-gen
    (s/and
      string?
      ; if string: convert to seq
      (s/conformer str-to-seq seq-to-str)
      ; unirpot id RegExp
      ::uniprotid-seq)
    #(gen/fmap seq-to-str (s/gen ::uniprotid-seq))))

(defn get-strict-uniprotid [id]
  "Extract a strict UniProt id from an id potentially containing a
   UniProt revision number.

   Example: (get-strict-uniprotid \"P04040-1\") ;=> \"P04040\""
   (get (re-matches uniprotid-regexp id) 1))
