(ns ppi-query.protein.uniprot
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
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

(comment
  (s/conform ::uniprotid "O73")

  (dotimes [n 10]
    (let [uniprot-ids (->> (s/exercise ::uniprotid-seq 100)
                           (map first)
                           (map (partial apply str)))]
      (println "RegExp parse")
      (time
        (doall
          (for [uid uniprot-ids]
            (doall (re-matches uniprotid-regexp uid)))))

      (println "Spec parse")
      (time
        (doall
          (for [uid uniprot-ids]
            (doall (s/conform ::uniprotid uid)))))

      nil)))
