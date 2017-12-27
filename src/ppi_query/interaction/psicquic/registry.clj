(ns ppi-query.interaction.psicquic.registry
  (:require [clojure.spec.alpha :as s]
            [clj-http.client :as http]
            [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.data.zip.xml :refer [text xml-> xml1->]]
            [ppi-query.spec :as ps]
            [ppi-query.xml :as pxml]
            [ppi-query.interaction.data :as intrd])
  (:import (org.hupo.psi.mi.psicquic.wsclient PsicquicSimpleClient)))

; PSICQUIC registry & service spec
(s/def ::name string?)
(s/def ::restUrl string?)
(s/def ::active boolean?)
(s/def ::organizationUrl string?)
(s/def ::service (s/keys :req-un [::name ::restUrl ::active ::organizationUrl]))
(s/def ::registry (s/map-of ::name ::service))
(defrecord PsicquicService [name restUrl active organizationUrl])

; PSICQUIC registry XML specs
(s/def ::service-xml
  (s/cat
    :name (pxml/text-node :name)
    :soapUrl (s/? (pxml/text-node :soapUrl))
    :restUrl (pxml/text-node :restUrl)
    :restExample (s/? (pxml/text-node :restExample))
    :active (pxml/text-node :active)
    :count (s/? (pxml/text-node :count))
    :version (s/? (pxml/text-node :version))
    :organizationUrl (pxml/text-node :organizationUrl)
    :description (s/? (pxml/text-node :description))
    :restricted (s/? (pxml/text-node :restricted))
    :tags (s/* (pxml/text-node :tag))))

(s/def ::registry-xml
  (pxml/node
    :tag #{:registry}
    :attrs ::pxml/attrs
    :content (s/coll-of
               (pxml/node
                 :tag #{:service}
                 :content ::service-xml)
               :min-count 1)))

(defn registry-request [active]
  "Creates HTTP request for the PSICQUIC registry"
  {:method       :get
   :url          "http://www.ebi.ac.uk/Tools/webservices/psicquic/registry/registry"
   :query-params {:action (if active "ACTIVE" "STATUS")
                  :format "xml"}
   :as           :stream})

(s/fdef registry-request
        :args (s/cat :active boolean?)
        :ret map?)

(defn fetch-registry-xml [active]
  "Fetch PSICQUIC registry xml as xml-zip."
  (-> (registry-request active)
      (http/request)
      (:body)
      (xml/parse)))

(s/fdef fetch-registry-xml
        :args (s/cat :active boolean?)
        :ret ::registry-xml)

;Parse registry xml as clojure maps
(defn parse-registry [registry-xml]
  (->> registry-xml

       ; registry -> service
       (zip/xml-zip)
       (#(xml-> % :registry :service))

       ; for each service -> create a record
       (map #(->PsicquicService
               ; name
               (xml1-> % :name text)
               ; REST URL
               (xml1-> % :restUrl text)
               ; active boolean
               (= "true" (xml1-> % :active text))
               ; Organization URL
               (xml1-> % :organizationUrl text)))

       ; transform to map of services by there name
       (map (juxt :name identity))
       (into {})))

(s/fdef parse-registry
        :args (s/cat :registry-xml ::registry-xml)
        :ret ::registry)

(defn fetch-registry [active]
  "Fetch the PSICQUIC registry from the official webservice."
  (-> (fetch-registry-xml active)
      (parse-registry)))

(s/fdef fetch-registry
        :args (s/cat :active boolean?)
        :ret ::registry)

; In-memory PSICQUIC registry set
(def ^:private mem-registry (atom {}))

(defn update-registry! []
  "Update the in memory registry."
  (try
    (let [new-registry (fetch-registry true)]
      ; Replace with new registry
      (swap! mem-registry (constantly new-registry)))
    (catch Exception e nil)))

(s/fdef update-registry!
        :ret ::registry)

(defn get-registry! []
  "Get the registry from memory cache or fetch it if the cache is empty"
  (when (empty? @mem-registry)
    (update-registry!))
  @mem-registry)

(s/fdef get-registry!
        :ret ::registry)

(defn get-service [name]
  "Get a PSICQUIC service by name"
  ((get-registry!) name))

(s/fdef get-service
        :args (s/cat :name ::name)
        :ret ::service)

(defn get-client [name]
  "Get one PSICQUIC client by PSICQUIC service name"
  (if-let [service (get-service name)]
    (new PsicquicSimpleClient (:restUrl service))
    (println "/!\\ Warning: No Psicquic client found for database" name)))

(s/fdef get-client
  :args (s/cat :database ::intrd/database)
  :ret (partial instance? PsicquicSimpleClient))

(defn get-clients [names]
  "Get list of PSICQUIC clients by PSICQUIC service name"
  (remove nil? (map get-client names)))

(s/fdef get-clients
  :args (s/cat :databases ::intrd/databases)
  :ret (s/coll-of (partial instance? PsicquicSimpleClient)))
