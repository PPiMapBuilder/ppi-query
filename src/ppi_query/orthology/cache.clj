(ns ppi-query.orthology.cache
  (:require [clojure.spec.alpha :as s]
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

(defn not-found->nil
  "Change a :not-found element into nil"
  [o] (if (= o :ortholog/not-found) nil o))

(defn get-ortholog-group
  "Get from memory cache an ortholog group for a protein."
  [protein]
  (let [organism (:organism protein)]
    (get-in @mem-cache [organism protein] :ortholog/not-found)))

(s/fdef get-ortholog-group
  :args (s/cat :protein ::prot/protein)
  :ret  (s/or :ortholog-group ::orth/ortholog-group
              :not-found #{:ortholog/not-found}))


(defn add-ortholog-group
  "Add to memory cache an ortholog group for a protein."
  [protein ortholog-group]
  (if-let [organism (:organism protein)]
    (swap!
      mem-cache
      assoc-in
      [organism protein]
      (utils/merge-distinct
        (not-found->nil (get-ortholog-group protein))
        ortholog-group))))

(s/fdef add-ortholog-group
  :args (s/cat :protein ::prot/protein
               :ortholog-group ::orth/ortholog-group)
  :ret ::ortholog-cache)

(defn filename-species-pair
  "Get the name of the file were to save or get a species-pair from."
  [organism1 organism2]
  (let [[short-org-1 short-org-2]
        (sort [(org/get-shortname organism1)
               (org/get-shortname organism2)])]
     (str "ortholog-cache/" short-org-1 "-" short-org-2 ".clj")))

(defn write-disk-ortholog-species
  "Add species pair ortholog groups to disk cache"
  [organism1 organism2 species-pair]
  (let [filename (filename-species-pair organism1 organism2)]
    (do (println "## Ortholog cache ## Writing to disk:" filename)
        (with-open [w (clojure.java.io/writer filename)]
          (binding [*out* w
                    *print-level* nil]
            (pr species-pair))))))

(defn add-ortholog-species-pair-mem
  "Add species pair ortholog groups to mem cache"
  [species-pair]
  (swap!
    mem-cache
    utils/merge-distinct
    species-pair))

(s/fdef add-ortholog-species-pair-mem
  :args (s/cat :ortholog-species-pair ::ortholog-cache)
  :ret ::ortholog-cache)

(defn add-ortholog-species-pair-disk
  "Add species pair ortholog groups to disk"
  [species-pair]
  (let [[org1 org2] (keys species-pair)]
     (write-disk-ortholog-species org1 org2 species-pair)))

(s/fdef add-ortholog-species-pair-disk
  :args (s/cat :ortholog-species-pair ::ortholog-cache))

(defn add-ortholog-species-pair
  "Add species pair ortholog groups to disk and memory"
  [species-pair]
  (if species-pair
    (do (add-ortholog-species-pair-disk species-pair)
        (add-ortholog-species-pair-mem  species-pair))))

(s/fdef add-ortholog-species-pair
  :args (s/cat :ortholog-species-pair ::ortholog-cache)
  :ret ::ortholog-cache)

(defn read-disk-ortholog-species
  "Get species pair ortholog groups from disk"
  [organism1 organism2]
  (let [filename (filename-species-pair organism1 organism2)]
    (if (.exists (clojure.java.io/as-file filename))
      (do (println "## Ortholog cache ## Reading from disk:" filename)
          (with-open [r (java.io.PushbackReader. (clojure.java.io/reader filename))]
            (read r))))))

(defn get-ortholog-species-pair-mem
  "Get species pair ortholog groups from mem cache"
  [organism1 organism2]
  (let [orthologs-org1 (get @mem-cache organism1)
        orthologs-org2 (get @mem-cache organism2)]
    (if (and orthologs-org1 orthologs-org2)
      {organism1 orthologs-org1, organism2 orthologs-org2}
      nil)))

(s/fdef get-ortholog-species-pair-mem
  :args (s/cat :organism1 ::org/organism :organism2 ::org/organism)
  :ret (s/nilable ::ortholog-cache))

(defn get-ortholog-species-pair-disk
  "Get species pair ortholog groups from disk cache and add them in mem cache"
  [organism1 organism2]
  (if-let [species-pair (read-disk-ortholog-species organism1 organism2)]
    (add-ortholog-species-pair-mem species-pair)))

(s/fdef get-ortholog-species-pair-disk
  :args (s/cat :organism1 ::org/organism :organism2 ::org/organism)
  :ret (s/nilable ::ortholog-cache))

(defn get-ortholog-species-pair
  "Get species pair ortholog groups from mem cache if possible, else get from disk"
  [organism1 organism2]
  (or (get-ortholog-species-pair-mem  organism1 organism2)
      (get-ortholog-species-pair-disk organism1 organism2)))

(s/fdef get-ortholog-species-pair
  :args (s/cat :organism1 ::org/organism :organism2 ::org/organism)
  :ret (s/nilable ::ortholog-cache))
