(ns ppi-query.web.view
  (:use [hiccup core page])
  (:require [ppi-query.organism :as orgn]
            [ppi-query.interaction.psicquic.registry :as reg]))

(defn html-select [name options & {mult :multiple :as args}]
  (into []
   (concat
    [:select {:name name :id name :multiple mult}]
    (map #(vector :option {:value %} %)
         options))))
(defn html-select-multiple [name options]
  (select name options :multiple true))

(defn select-orgs [name & {mult :multiple :as args}]
  (into []
   (concat
    [:select {:name name :id name :multiple mult}]
    (map (fn [org] (vector :option {:value (:taxon-id org)} (or (:scientific-name org) (:common-name org))))
         (sort-by :scientific-name orgn/inparanoid-organism-repository)))))

(defn select-dbs [name & {mult :multiple :as args}]
  (into []
   (concat
    [:select {:name name :id name :multiple mult}]
    (map (fn [db] (vector :option {:value db} db))
         (sort (keys (reg/fetch-registry true)))))))

(defn view-layout [& content]
  (html
    (doctype :xhtml-strict)
    (xhtml-tag "en"
      [:head
        [:meta {:http-equiv "Content-type"
                :content "text/html; charset=utf-8"}]
        [:title "ppi-query"]]
      [:body content])))

(defn view-input []
  (view-layout
    [:h2 "Query a protein"]
    [:form {:method "get" :action "/graph"}
      [:label {:for "dbs"} "Databases: "]
      (select-dbs "dbs" :multiple true)
      [:br]
      [:label {:for "org-name"} "Organism: "]
      (select-orgs "org-name")
      [:br]
      [:label {:for "uniprotids"} "Proteins (separated by `,` or `;`): "]
      [:input#uniprotids {:type "text" :name "uniprotids"}]
      [:br]
      [:label {:for "oth-orgs"} "Other organisms: "]
      (select-orgs "oth-orgs" :multiple true)
      [:br]
      [:input.action {:type "submit" :value "Fetch network"}]]))

(defn html-mutlilines [args]
  (into []
    (concat [:p]
      (map #(vector :p %)
           args))))

(defn view-output [dbs org-name uniprotids oth-orgs]
  (view-layout
    [:h2 "Databases:"]
    (html-mutlilines dbs)
    [:h2 "Reference organism:"]
    [:p org-name]
    [:h2 "Uniprot Ids:"]
    (html-mutlilines uniprotids)
    [:h2 "Other orthologs:"]
    (html-mutlilines oth-orgs)
    [:a.action {:href "/"} "Get another network"]))
