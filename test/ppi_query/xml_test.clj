(ns ppi-query.xml-test
  (:require [clojure.test :refer :all]
            [ppi-query.test.utils :refer :all]
            [ppi-query.spec :as ps]
            [clojure.spec.alpha :as s]
            [ppi-query.xml :as pxml]
            [clojure.spec.test.alpha :as stest]))


(deftest* test-xml-specs

  (testing "Base specs"
    (are-spec ::pxml/tag
      :valid [:a, :b]

      :invalid [nil, ""])

    (are-spec ::pxml/attrs
      :valid [nil, {:a nil}]

      :invalid [""])

    (are-spec ::pxml/document
      :valid [{:tag :a, :attrs nil, :content nil}]

      :invalid [{:tag :a, :attrs nil, :content ""}
                {:tag "a", :attrs nil, :content nil}
                {:tag :a}])))

