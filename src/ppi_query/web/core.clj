(ns ppi-query.web.core
  (:use compojure.core
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [clojure.string :as str]
            [ppi-query.web.view :as view])
            ;[ring.adapter.jetty :refer :all])
  (:gen-class))

; Launch with `lein run -m ppi-query.web`

(defn rm-space [string]
  (str/replace string #"[\s]" ""))

(defn parse-int-or-string [arg]
  (try
    (Integer/parseInt (rm-space arg))
    (catch Exception e
      arg)))

(defn make-vector [parse-fn arg]
  (cond (nil? arg) []
        (not (coll? arg)) [(parse-fn arg)]
        :else (map parse-fn arg)))

(defn split-s [string]
  (str/split (rm-space string) #"[,;]"))

(defn parse-input [dbs org-name uniprotid oth-orgs]
  [(make-vector str dbs)
   (parse-int-or-string org-name)
   (make-vector str (split-s uniprotid))
   (make-vector parse-int-or-string oth-orgs)])

(defroutes main-routes
  (GET "/" []
    (view/view-input))
  (GET "/graph" [dbs org-name uniprotids oth-orgs]
    (let [[d o p oo] (parse-input dbs org-name uniprotids oth-orgs)]
      (view/view-output d o p oo)))
  (route/resources "/")
  (route/not-found "<h1>404: Page not found</h1>"))

(def app
  (-> (handler/site main-routes)
      (wrap-base-url)))
