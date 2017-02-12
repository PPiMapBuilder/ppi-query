(ns ppi-query.protein.uniprot
  (:require [clojure.spec :as s]
            [clojure.set :refer [union]]
            [ppi-query.spec :as ps]))

; Official Uniprot ID regexp with optional revision suffix
(def uniprotid-regexp
  #"([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})([\-\.]\w+)?")

(s/def ::uniprotid-seq
  (s/cat
    ; uniprot id main part
    :0 (s/alt :0 (s/cat :0 #{\P \O \Q}
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
                             1 2)))
    ; optional uniprot entry revision (ex: "-1", "-PRO_0E0")
    :1 (s/?
         (s/cat :0 #{\- \.}
                :1 (s/+ ps/word-set)))))

(s/def ::uniprotid
  (s/with-gen
    (s/and
      ; if string: convert to seq
      (s/conformer seq)
      ; unirpot id RegExp
      ::uniprotid-seq)
    #(s/gen ::uniprotid-seq)))

(defn get-strict-uniprotid [id]
  "Extract a strict UniProt id from an id potentially containing a
   UniProt revision number.

   Example: (get-strict-uniprotid \"P04040-1\") ;=> \"P04040\""
  (let [c (s/conform ::uniprotid id)
        search-char (fn search-char [c]
                      (cond
                        (seqable? c) (mapcat search-char (seq c))
                        (char? c) [c]
                        :else []))]
    (if (= s/invalid? c)
      nil
      (apply str (search-char (:0 c))))))

(comment
  ;
  ; Benchmark extracting strict UniProt ID from a thousand generated IDs
  ; Compare clojure.spec parsing with string RegEpx parsing
  ;
  (require '[clojure.core.async :refer [thread]])
  (dotimes [n 1]
    (let [uniprot-ids (doall
                        (->> (s/exercise ::uniprotid-seq 100)
                             (map first)
                             (map (partial apply str))))]
      (thread
        (time
          (doall
            (for [uid uniprot-ids]
              #_
              (doall (s/conform ::uniprotid uid))
              (get-strict-uniprotid uid))))
        (println "Spec parse\n"))

      (thread
        (time
          (doall
            (for [uid uniprot-ids]
              #_
              (doall (re-matches uniprotid-regexp uid))
              (get (re-matches uniprotid-regexp uid) 1))))
        (println "RegExp parse\n"))
      nil)))
