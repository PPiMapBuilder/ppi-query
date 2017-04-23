(ns ppi-query.interaction.psicquic.registry-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]
            [ppi-query.interaction.psicquic.registry :as reg]
            [clojure.test.check.generators :as gen]))

(stest/instrument)

(def example-registry
  {:tag :registry
   :attrs nil
   :content [{:tag :service
              :attrs nil
              :content [{:tag :name
                         :attrs nil
                         :content ["name1"]}
                        {:tag :restUrl
                         :attrs nil
                         :content ["http://url1.com"]}]}

             {:tag :service
              :attrs nil
              :content [{:tag :name
                         :attrs nil
                         :content ["name2"]}
                        {:tag :restUrl
                         :attrs nil
                         :content ["http://url2.com"]}]}]})


(deftest test-parse-registry-xml
  (is (= (reg/parse-registry example-registry)
         {"name1" {:name "name1" :url "http://url1.com"}
          "name2" {:name "name2" :url "http://url2.com"}})))

(deftest check-parse-registry-xml
  (check' `reg/parse-registry))

(deftest check-fetch-registry
  (stest/instrument `reg/fetch-registry-xml
    {:stub #{`reg/fetch-registry-xml}})

  (check' `reg/fetch-registry))
