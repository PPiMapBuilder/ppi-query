(ns ppi-query.web
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.adapter.jetty :refer :all]))

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
    [:form {:method "post" :action "/"}
      [:label "Organism:"]
      [:input {:type "text" :name "org-name"}]
      [:br]
      [:label "Protein:"]
      [:input {:type "text" :name "uniprotid"}]
      [:br]
      [:input.action {:type "submit" :value "Fetch network"}]]))

(defn view-output [org-name uniprotid]
  (view-layout
    [:h2 "Org & Prot:"]
    [:p org-name " & " uniprotid]
    [:a.action {:href "/"} "Get another network"]))

(defn parse-input [org-name uniprotid]
  [(str org-name) (str uniprotid)])

(defroutes app
  (GET "/" []
    (view-input))

  (POST "/" [org-name uniprotid :as request]
    (let [[o p] (parse-input org-name uniprotid)]
      (println org-name uniprotid)
      (println o p)
      (println request)
      (view-output org-name uniprotid)))

  (route/not-found "<h1>404: Page not found</h1>"))

(defn -main []
  (run-jetty #'app {:port 8080}))
