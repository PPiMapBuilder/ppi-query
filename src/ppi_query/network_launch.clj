(ns ppi-query.network-launch
  (:require [ppi-query.network :as network]
            [ppi-query.graph   :as graph]
            [clojure.tools.cli :refer [cli]]
            [clojure.data.json :as json])
  (:gen-class))

(defn add-to-list [m k c]
  (update m k (fnil conj []) c))

(defn parse-int-or-string [arg]
  (try
    (Integer/parseInt arg)
    (catch Exception e
      arg)))

(defn print-help [banner]
  (println banner)
  (println "Example: lein run -m ppi-query.network-launch -- -d IntAct -r C.elegans -p Q18688 -p Q20646 -o M.musculus -o S.pombe -o 3702")
  (println "Short: lein run -- -d IntAct -r C.elegans -p Q18688 -p Q20646 -o M.musculus -o S.pombe -o 3702")
  (println "Long: lein run -- -d IntAct -r 9606 -p Q08752 -o C.elegans -o M.musculus -o S.pombe -o 3702 -o 9913 -o 7227 -o S.cerevisiae -o 10116"))

(defn -main [& args]
  (System/setProperty "sun.net.spi.nameservice.nameservers" "8.8.8.8")
  (System/setProperty "sun.net.spi.nameservice.provider.1" "dns,sun")
  (let [[opts args banner]
        (cli args
          ["-h" "--help" "Print this help"
           :default false :flag true]
          ["-d" "--database" "Database name, can be called multiple times (required)"
           :assoc-fn add-to-list]
          ["-r" "--ref-org" "Reference organism (required)"
           :parse-fn parse-int-or-string]
          ["-p" "--prot" "Protein of interest, can be called multiple times (at least one required)"
           :assoc-fn add-to-list]
          ["-o" "--oth-org" "Other organism, can be called multiple times"
           :parse-fn parse-int-or-string
           :assoc-fn add-to-list])]

    (cond
      (or (:help opts) (empty? (dissoc opts :help)))
      (print-help banner)
      (not-empty args)
      (do (println "Error: You must have a -[rpo] before each argument")
          (print-help banner))
      (nil? (:ref-org opts))
      (do (println "Error: You must set at least one database.")
          (print-help banner))
      (nil? (:database opts))
      (do (println "Error: You must set an reference organism.")
          (print-help banner))
      (nil? (:prot opts))
      (do (println "Error: You must set at least one protein of interest.")
          (print-help banner))
      :else
      (let [databases (:database opts)
            ref-org (:ref-org opts)
            prots (:prot opts)
            oth-orgs (or (:oth-org opts) [])
            [ret-proteins ret-interactions]
            (network/fetch-protein-network-strings
              databases ref-org prots oth-orgs)
            nodes-edges (graph/to-graph-data ret-proteins ret-interactions)]
        (json/pprint nodes-edges)))))
