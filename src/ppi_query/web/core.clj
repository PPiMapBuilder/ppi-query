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
  (str/replace (str string) #"[\s]" ""))

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
  (set (str/split (rm-space string) #"[,;]")))

(defn parse-input [dbs ref-org uniprotids oth-orgs]
  [(make-vector rm-space dbs)
   (parse-int-or-string ref-org)
   (make-vector str (split-s uniprotids))
   (make-vector parse-int-or-string oth-orgs)])

(defroutes main-routes
  (GET "/" [dbs ref-org uniprotids oth-orgs force-input]
    (let [[d r p o] (parse-input dbs ref-org uniprotids oth-orgs)]
      (if (or (and force-input (contains? #{"t" "T" "true" "True" "TRUE"} force-input))
              (empty? d)
              (nil? r)
              (= "" r)
              (empty? p)
              (empty? o))
        (view/view-input d r p o)
        (view/view-output d r p o))))

  (route/resources "/")
  (route/not-found "<h1>404: Page not found</h1>"))

(def app
  (-> (handler/site main-routes)
      (wrap-base-url)))
