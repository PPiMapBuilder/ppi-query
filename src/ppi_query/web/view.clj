(ns ppi-query.web.view
  (:use [hiccup core page form])
  (:require [ppi-query.organism :as orgn]
            [ppi-query.interaction.psicquic.registry :as reg]
            [ppi-query.network :as network]
            [ppi-query.graph   :as graph]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:import java.net.URI
           java.net.URLEncoder))

(defn select-orgs [name & {mult :multiple sel :selected :as args}]
  (let [selected
        (set (if (coll? sel) sel [sel]))]
    (into []
     (concat
      [:select {:name name :id name :multiple mult}]
      (map (fn [{id :taxon-id :as org}]
             (vector
               :option {:value id
                        :selected (contains? selected id)}
                       (or (:scientific-name org) (:common-name org))))
           (sort-by :scientific-name orgn/inparanoid-organism-repository))))))

(defn select-dbs [name & {mult :multiple sel :selected :as args}]
  (let [selected
        (set (if (coll? sel) sel [sel]))]
    (into []
     (concat
      [:select {:name name :id name :multiple mult}]
      (map (fn [db]
             (vector :option {:value db
                              :selected (contains? selected db)}
                             db))
           (sort (keys (reg/get-registry!))))))))

(defn view-layout [& content]
  (html
    (doctype :xhtml-strict)
    (xhtml-tag "en"
      [:head
        [:meta {:http-equiv "Content-type"
                :content "text/html; charset=utf-8"
                :name "ppi-query" :description "Fetch interactions from PSIQUICK from multiple orthologs, display them in VR"}]
        [:title "Ppi-query"]
        [:script {:src "https://aframe.io/releases/0.7.1/aframe.min.js"}]
        [:script {:src "https://unpkg.com/aframe-forcegraph-component/dist/aframe-forcegraph-component.js"}]]
      [:body content])))


(defn view-input [dbs ref-org uniprotids oth-orgs]
  (view-layout
    [:h2 "Query a protein"]
    (form-to [:get "/"]
      (label "dbs" "Databases: ")
      (select-dbs "dbs" :multiple true :selected dbs)
      [:br]
      (label "ref-org" "Organism: ")
      (select-orgs "ref-org" :selected ref-org)
      [:br]
      (label "uniprotids" "Proteins (separated by `,` or `;`): ")
      [:input#uniprotids {:type "text" :name "uniprotids" :value (str/join ", " uniprotids)}]
      [:br]
      (label "oth-orgs" "Other organisms: ")
      (select-orgs "oth-orgs" :multiple true :selected oth-orgs)
      [:br]
      (submit-button "Fetch network"))
    [:a {:href "/?dbs=IntAct&ref-org=C.elegans&uniprotids=Q18688,Q20646&oth-orgs=M.musculus&oth-orgs=S.pombe&oth-orgs=3702"}
        "Quick network"]))
(defn html-mutlilines [args]
  (into []
    (concat [:p]
      (map #(vector :p %)
           args))))

(def ^:dynamic *encoding* "UTF-8")

(defmacro with-encoding
  "Sets a default encoding for URL encoding strings. Defaults to UTF-8."
  [encoding & body]
  `(binding [*encoding* ~encoding]
     ~@body))

(defprotocol URLEncode
  (url-encode [x] "Turn a value into a URL-encoded string."))

(extend-protocol URLEncode
  String
  (url-encode [s] (URLEncoder/encode s *encoding*))
  java.util.Map
  (url-encode [m]
    (str/join "&"
      (for [[k v] m]
        (if (vector? v)
          (str/join "&"
            (map #(str (url-encode k) "=" (url-encode %))
                 v))
          (str (url-encode k) "=" (url-encode v)))))))

(defn href-back [dbs ref-org uniprotids oth-orgs]
  (str "/?"
    (url-encode
      {"dbs" (vec dbs)
       "ref-org" (str ref-org)
       "uniprotids" (str/join "," uniprotids)
       "oth-orgs" (vec (map str oth-orgs))
       "force-input" "true"})))

(defn view-output-html [dbs ref-org uniprotids oth-orgs]
  (view-layout
    [:h2 "Databases:"]
    (html-mutlilines dbs)
    [:h2 "Reference organism:"]
    [:p ref-org]
    [:h2 "Uniprot Ids:"]
    (html-mutlilines uniprotids)
    [:h2 "Other orthologs:"]
    (html-mutlilines oth-orgs)
    [:a {:href (href-back dbs ref-org uniprotids oth-orgs)}
        "Get another network"]))

(defn view-graph-json [nodes-edges]
  (view-layout
    [:a-scene {:stats ""}
      [:a-camera {:wasd-controls "fly: true; acceleration: 600"}
        [:a-cursor {:color "lavender" :opacity "0.5"}]]
      [:a-sky {:color "#002"}]

      [:a-entity {:forcegraph
                  (str "nodes:" (json/write-str (nodes-edges :nodes))
                       ";links:" (json/write-str (nodes-edges :edges))
                       ";node-id: id;node-label: label;"
                       "link-source:from;link-target:to;"
                       "link-label:label;link-desc:desc;"
                       "link-auto-color-by:desc;")}]]))

(defn view-output-graph [dbs ref-org uniprotids oth-orgs]
  (let [[ret-proteins ret-interactions]
        (network/fetch-protein-network-strings
          dbs ref-org uniprotids oth-orgs)
        nodes-edges (graph/to-graph-data ret-proteins ret-interactions)]
    (view-graph-json nodes-edges)))

(def view-output view-output-graph)
